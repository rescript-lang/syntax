open Parsetree

let isJsxConfigAttr ((loc, _) : attribute) = loc.txt = "jsxConfig"

let hasJsxConfigAttrStr {pstr_desc} =
  match pstr_desc with
  | Pstr_attribute attribute -> isJsxConfigAttr attribute
  | _ -> false

let jsxConfigAttributeStr structure =
  let firstStrItem = try Some (List.hd structure) with Not_found -> None in
  match firstStrItem with
  | Some strItem when hasJsxConfigAttrStr strItem -> firstStrItem
  | _ -> None

let hasJsxConfigAttrSig {psig_desc} =
  match psig_desc with
  | Psig_attribute attribute -> isJsxConfigAttr attribute
  | _ -> false

let jsxConfigAttributeSig signature =
  (* maybe just check the first structure_item? *)
  let firstSigItem = try Some (List.hd signature) with Not_found -> None in
  match firstSigItem with
  | Some sigItem when hasJsxConfigAttrSig sigItem -> firstSigItem
  | _ -> None

let getJsxConfig payload =
  match payload with
  | Some
      (PStr
        ({
           pstr_desc =
             Pstr_eval ({pexp_desc = Pexp_record (recordFields, None)}, _);
         }
        :: _rest)) ->
    recordFields
  | _ -> raise (Invalid_argument "jsxConfig accepts a record config only")

type configKey = Int | String

let getJsxConfigByKey ~key ~type_ recordFields =
  let values =
    List.filter_map
      (fun ((lid, expr) : Longident.t Location.loc * expression) ->
        match (type_, lid, expr) with
        | ( Int,
            {txt = Lident k},
            {pexp_desc = Pexp_constant (Pconst_integer (value, None))} )
          when k = key ->
          Some value
        | ( String,
            {txt = Lident k},
            {pexp_desc = Pexp_constant (Pconst_string (value, None))} )
          when k = key ->
          Some value
        | _ -> None)
      recordFields
  in
  match values with
  | [] -> None
  | [v] | v :: _ -> Some v

let transform ~v3 ~v4 ~jsx_version ~jsx_module ~jsx_mode code =
  match (jsx_version, jsx_module, jsx_mode) with
  | 3, _, _ -> v3 code
  | 4, _, "classic" -> v4 ~jsx_mode:"classic" code
  | 4, _, "automatic" -> v4 ~jsx_mode:"automatic" code
  | _ -> code

let rewrite_implementation ~jsx_version ~jsx_module ~jsx_mode
    (code : Parsetree.structure) : Parsetree.structure =
  match jsxConfigAttributeStr code with
  (* if @@jsxConfig({ .. }) *)
  | Some _ -> (
    let _attr_loc, payload =
      match jsxConfigAttributeStr code with
      | Some {pstr_desc = Pstr_attribute ({loc}, payload)} -> (loc, Some payload)
      | _ -> (Location.none, None)
    in
    let version =
      getJsxConfig payload
      |> getJsxConfigByKey ~key:"version" ~type_:Int
      |> Option.map int_of_string_opt
      |> Option.join
    in
    let module_ =
      getJsxConfig payload |> getJsxConfigByKey ~key:"module" ~type_:String
    in
    let mode =
      getJsxConfig payload |> getJsxConfigByKey ~key:"mode" ~type_:String
    in
    match (version, module_, mode) with
    | Some 3, _, _ -> Reactjs_jsx_ppx_v3.rewrite_implementation code
    | Some 4, _, Some "classic" ->
      Reactjs_jsx_ppx_v4.rewrite_implementation ~jsx_mode:"classic" code
    | Some 4, _, Some "automatic" ->
      Reactjs_jsx_ppx_v4.rewrite_implementation ~jsx_mode:"automatic" code
    | _ ->
      raise
        (Invalid_argument
           "jsxConfig has options: version = [3, 4], mode = [\"classic\", \
            \"automatic\"]"))
  | None ->
    transform ~v3:Reactjs_jsx_ppx_v3.rewrite_implementation
      ~v4:Reactjs_jsx_ppx_v4.rewrite_implementation ~jsx_version ~jsx_module
      ~jsx_mode code
  [@@raises Invalid_argument, Failure]

let rewrite_signature ~jsx_version ~jsx_module ~jsx_mode
    (code : Parsetree.signature) : Parsetree.signature =
  match jsxConfigAttributeSig code with
  (* if @@jsxConfig({ .. }) *)
  | Some _ -> (
    let _attr_loc, payload =
      match jsxConfigAttributeSig code with
      | Some {psig_desc = Psig_attribute ({loc}, payload)} -> (loc, Some payload)
      | _ -> (Location.none, None)
    in
    let version =
      getJsxConfig payload
      |> getJsxConfigByKey ~key:"version" ~type_:Int
      |> Option.map int_of_string_opt
      |> Option.join
    in
    let module_ =
      getJsxConfig payload |> getJsxConfigByKey ~key:"module" ~type_:String
    in
    let mode =
      getJsxConfig payload |> getJsxConfigByKey ~key:"mode" ~type_:String
    in
    match (version, module_, mode) with
    | Some 3, _, _ -> Reactjs_jsx_ppx_v3.rewrite_signature code
    | Some 4, _, Some "classic" ->
      Reactjs_jsx_ppx_v4.rewrite_signature ~jsx_mode code
    | Some 4, _, Some "automatic" ->
      Reactjs_jsx_ppx_v4.rewrite_signature ~jsx_mode code
    | _ ->
      raise
        (Invalid_argument
           "jsxConfig has options: version = [3, 4], mode = [\"classic\", \
            \"automatic\"]"))
  | None ->
    transform ~v3:Reactjs_jsx_ppx_v3.rewrite_signature
      ~v4:Reactjs_jsx_ppx_v4.rewrite_signature ~jsx_version ~jsx_module
      ~jsx_mode code
  [@@raises Invalid_argument, Failure]
