open Parsetree

let isJsxConfigAttr ((loc, _) : attribute) = loc.txt = "jsxConfig"

let hasJsxConfigAttrStr {pstr_desc} =
  match pstr_desc with
  | Pstr_attribute attribute -> isJsxConfigAttr attribute
  | _ -> false

let jsxConfigAttributeStr structure =
  match structure with
  | [] -> None
  | ([strItem] | strItem :: _) when hasJsxConfigAttrStr strItem -> Some strItem
  | _ -> None

let hasJsxConfigAttrSig {psig_desc} =
  match psig_desc with
  | Psig_attribute attribute -> isJsxConfigAttr attribute
  | _ -> false

let jsxConfigAttributeSig signature =
  match signature with
  | [] -> None
  | ([sigItem] | sigItem :: _) when hasJsxConfigAttrSig sigItem -> Some sigItem
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
  | _ -> []

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

let getOrDefaultInt ~key ~default payload =
  getJsxConfig payload
  |> getJsxConfigByKey ~key ~type_:Int
  |> Option.map int_of_string_opt
  |> Option.join |> Option.value ~default

let getOrDefaultString ~key ~default payload =
  getJsxConfig payload
  |> getJsxConfigByKey ~key ~type_:String
  |> Option.value ~default

let updateConfig ~jsx_version ~jsx_module ~jsx_mode payload =
  let version = getOrDefaultInt ~key:"version" ~default:jsx_version payload in
  let module_ = getOrDefaultString ~key:"module" ~default:jsx_module payload in
  let mode = getOrDefaultString ~key:"mode" ~default:jsx_mode payload in
  (version, module_, mode)

let rewrite_implementation ~jsx_version ~jsx_module ~jsx_mode
    (code : Parsetree.structure) : Parsetree.structure =
  let _attr_loc, payload =
    match jsxConfigAttributeStr code with
    | Some {pstr_desc = Pstr_attribute ({loc}, payload)} -> (loc, Some payload)
    | _ -> (Location.none, None)
  in
  let version, module_, mode =
    updateConfig ~jsx_version ~jsx_module ~jsx_mode payload
  in
  match (version, module_, mode) with
  | 3, _, _ -> Reactjs_jsx_ppx_v3.rewrite_implementation code
  | 4, _, "classic" ->
    Reactjs_jsx_ppx_v4.rewrite_implementation ~jsx_mode:"classic" code
  | 4, _, "automatic" ->
    Reactjs_jsx_ppx_v4.rewrite_implementation ~jsx_mode:"automatic" code
  | _ -> code
  [@@raises Invalid_argument, Failure]

let rewrite_signature ~jsx_version ~jsx_module ~jsx_mode
    (code : Parsetree.signature) : Parsetree.signature =
  let _attr_loc, payload =
    match jsxConfigAttributeSig code with
    | Some {psig_desc = Psig_attribute ({loc}, payload)} -> (loc, Some payload)
    | _ -> (Location.none, None)
  in
  let version, module_, mode =
    updateConfig ~jsx_version ~jsx_module ~jsx_mode payload
  in
  match (version, module_, mode) with
  | 3, _, _ -> Reactjs_jsx_ppx_v3.rewrite_signature code
  | 4, _, "classic" -> Reactjs_jsx_ppx_v4.rewrite_signature ~jsx_mode code
  | 4, _, "automatic" -> Reactjs_jsx_ppx_v4.rewrite_signature ~jsx_mode code
  | _ -> code
  [@@raises Invalid_argument, Failure]
