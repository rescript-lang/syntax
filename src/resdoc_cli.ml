(* Parsetree docs: https://sanette.github.io/ocaml-api/4.06/Parsetree.html *)

(* could use Ast_iterator for in depth traversing *)
open Format

module Util = struct
  let filter_map f =
    let rec aux accu = function
      | [] -> List.rev accu
      | x :: l ->
        match f x with
        | None -> aux accu l
        | Some v -> aux (v :: accu) l
    in
    aux []
end

module Json = struct
  type t =
    | String of string
    | Number of float
    | Array of t list
    | Object of (string * t) list
    | True
    | False
    | Null

  let string_of_number f =
    let s = string_of_float f in
    if (s.[(String.length s) - 1]) = '.'
    then String.sub s 0 ((String.length s) - 1)
    else s

  let escape text =
    let ln = String.length text in
    let buf = Buffer.create ln in
    let rec loop i =
      if i < ln
      then
        ((match text.[i] with
            | '\012' ->
              Buffer.add_string buf (("\\f")[@reason.raw_literal "\\\\f"])
            | '\\' ->
              Buffer.add_string buf (("\\\\")[@reason.raw_literal "\\\\\\\\"])
            | '"' ->
              Buffer.add_string buf (("\\\"")[@reason.raw_literal "\\\\\\\""])
            | '\n' ->
              Buffer.add_string buf (("\\n")[@reason.raw_literal "\\\\n"])
            | '\b' ->
              Buffer.add_string buf (("\\b")[@reason.raw_literal "\\\\b"])
            | '\r' ->
              Buffer.add_string buf (("\\r")[@reason.raw_literal "\\\\r"])
            | '\t' ->
              Buffer.add_string buf (("\\t")[@reason.raw_literal "\\\\t"])
            | c -> Buffer.add_char buf c);
         loop (i + 1)) in
    loop 0; Buffer.contents buf

  let rec stringify t =
    match t with
    | String value -> "\"" ^ (escape value) ^ "\""
    | Number num -> string_of_number num
    | Array items ->
      "[" ^
      ((String.concat ((", ")[@reason.raw_literal ", "])
          (List.map stringify items))
       ^ (("]")[@reason.raw_literal "]"]))
    | ((Object (items))[@explicit_arity ]) ->
      (("{")[@reason.raw_literal "{"]) ^
      ((String.concat ((", ")[@reason.raw_literal ", "])
          (List.map
             (fun (k,v)  ->
                (("\"")[@reason.raw_literal "\\\""]) ^
                ((String.escaped k) ^
                 ((("\": ")[@reason.raw_literal "\\\": "]) ^
                  (stringify v)))) items))
       ^ (("}")[@reason.raw_literal "}"]))
    | True  -> (("true")[@reason.raw_literal "true"])
    | False  -> (("false")[@reason.raw_literal "false"])
    | Null  -> (("null")[@reason.raw_literal "null"])

  let white n =
    let buffer = Buffer.create n in
    for _ = 0 to n - 1 do Buffer.add_char buffer ' ' done;
    Buffer.contents buffer
  let rec stringifyPretty ?(indent= 0)  t =
    match t with
    | ((String (value))[@explicit_arity ]) ->
      (("\"")[@reason.raw_literal "\\\""]) ^
      ((escape value) ^ (("\"")[@reason.raw_literal "\\\""]))
    | ((Number (num))[@explicit_arity ]) -> string_of_number num
    | ((Array ([]))[@explicit_arity ]) -> (("[]")[@reason.raw_literal "[]"])
    | ((Array ((String _ as contents)::[]))[@explicit_arity ]) ->
      (("[")[@reason.raw_literal "["]) ^
      ((stringifyPretty contents) ^ (("]")[@reason.raw_literal "]"]))
    | ((Array (items))[@explicit_arity ]) ->
      (("[\n")[@reason.raw_literal "[\\n"]) ^
      ((white indent) ^
       ((String.concat
           (((",\n")[@reason.raw_literal ",\\n"]) ^ (white indent))
           (List.map (stringifyPretty ~indent:(indent + 2)) items))
        ^
        ((("\n")[@reason.raw_literal "\\n"]) ^
         ((white (indent - 2)) ^ (("]")[@reason.raw_literal "]"])))))
    | ((Object ([]))[@explicit_arity ]) -> (("{}")[@reason.raw_literal "{}"])
    | ((Object (items))[@explicit_arity ]) ->
      (("{\n")[@reason.raw_literal "{\\n"]) ^
      ((white indent) ^
       ((String.concat
           (((",\n")[@reason.raw_literal ",\\n"]) ^ (white indent))
           (List.map
              (fun (k,v)  ->
                 (("\"")[@reason.raw_literal "\\\""]) ^
                 ((String.escaped k) ^
                  ((("\": ")[@reason.raw_literal "\\\": "]) ^
                   (stringifyPretty ~indent:(indent + 2) v))))
              items))
        ^
        "\n" ^
         ((white (indent - 2)) ^ "}")))
    | True  -> "true"
    | False  -> "false"
    | Null  -> "null"
end


(* all documentation data for a module interface *)
module DocItem = struct
  type doc_constructor = { name: string; docstring: string }
  type t =
    | Doc_value of {
        signature: string;
        name: string;
        docstring: string;
      }
    | Doc_variant of {
        constructors: doc_constructor list;
        docstring: string;
      }
      (* abstract, record *)
    | Doc_type of {
        signature: string;
        name: string;
        docstring: string;
      }
    | Doc_text of string
    | Doc_module of {
        name: string;
        docstring: string;
        items: t list;
      }

  let rec toJson t =
    let open Json in
    match t with 
    | Doc_type {signature; name; docstring }
    | Doc_value {signature; name; docstring } -> 
      let kind = match t with
        | Doc_type _ -> "Doc_type"
        | _ -> "Doc_value"
      in
      Object [
        ("kind", String kind);
        ("signature", String signature);
        ("name", String name);
        ("docstring", String docstring)
      ]
    | Doc_variant { constructors; docstring } ->
      Object [
        ("kind", String "Doc_variant");
        ("constructors", Array (List.map (fun item -> String item.docstring) constructors));
        ("docstring", String docstring)
      ]
    | Doc_text docstring ->
      Object [
        ("kind", String "Doc_text");
        ("docstring", String docstring)
      ]
    | Doc_module {items; docstring; name} ->
      Object [
        ("kind", String "Doc_module");
        ("docstring", String docstring);
        ("name", String name);
        ("items", Array (List.map (fun item -> toJson item) items))
      ]
end

(* Used for the ReScript printer facilities *)
let cmtTable = Res_comments_table.make ()

let drop_doc_attributes (sigItem: Parsetree.signature_item) : Parsetree.signature_item =
  let open Parsetree in
  let filterDocAttrs ((id, _) : attribute) =
    match id.txt with
    | "ocaml.doc"
    | "ocaml.txt" ->
      false
    | _ -> true
  in
  match sigItem.psig_desc with
  | Psig_value ({pval_attributes; _} as desc) ->
    let filteredAttrs = List.filter filterDocAttrs pval_attributes in
    let desc = Psig_value ({ desc with pval_attributes = filteredAttrs }) in
    { sigItem with psig_desc = desc }
  | Psig_type (loc, tdecls) ->
    let newDecls = tdecls |> List.map (fun declr ->
        let filteredAttrs = List.filter filterDocAttrs declr.ptype_attributes in
        { declr with ptype_attributes = filteredAttrs }
      )
    in
    let desc = Psig_type (loc, newDecls) in
    { sigItem with psig_desc = desc }

  | Psig_module desc ->
    let filteredAttrs = List.filter filterDocAttrs desc.pmd_attributes in
    let newDesc = { desc with pmd_attributes = filteredAttrs } in
    { sigItem with psig_desc = Psig_module newDesc }
  | _ -> sigItem


let getOCamlDocString (loc: Parsetree.attribute) =
  let (id, payload) = loc in
  match id.txt, payload with 
  | "ocaml.doc", PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(a,_));_},_); _}]
    -> 
    let docstring = sprintf "@[<v>@ @[%s@]@]@." a in
    Some docstring
  | _ -> None

(* returns (signature, docstring) *)
let extractSignatureAndDocstring signatureItem (attrs: Parsetree.attributes) =
  let open Parsetree in
  let rec findFirstItem (attrs: Parsetree.attributes) =
    match attrs with
    | [] -> None
    | ({Location.txt = "ocaml.doc"}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(a,_));_},_); _}]):: _ ->
      let signature =
        Res_printer.printSignatureItem (drop_doc_attributes signatureItem) cmtTable |> Res_doc.toString ~width:80
      in
      let docstring = sprintf "@[<v>@ @[%s@]@]@." a in
      Some (signature, docstring)
    | _::attrs -> findFirstItem attrs
  in
  findFirstItem attrs

module Signature = struct
  let fromSignatureItem item =
    Res_printer.printSignatureItem (drop_doc_attributes item) cmtTable |> Res_doc.toString ~width:80
end

let extract_doc (acc: DocItem.t list) (signatureItem: Parsetree.signature_item) : DocItem.t list = 
  match signatureItem.psig_desc with
  | Psig_attribute  ({Asttypes.txt="ocaml.text"}, payload) ->
    begin match payload with
      | Parsetree.PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(str,_));_},_); _}]
        -> acc @ [ DocItem.Doc_text str ]
      | _ -> acc
    end
  | Psig_value {pval_attributes; pval_name; _} ->
    begin 
      let items = 
        pval_attributes 
        |> List.map (fun ((id, payload) : Parsetree.attribute) -> 
            match id.txt, payload with 
            | "ocaml.doc", PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(a,_));_},_); _}]
              -> 
              let signature =
                Res_printer.printSignatureItem (drop_doc_attributes signatureItem) cmtTable |> Res_doc.toString ~width:80
              in
              Some (DocItem.Doc_value({
                  signature;
                  name=pval_name.txt;
                  docstring=sprintf "@[<v>@ @[%s@]@]@." a
                }))
            | _ -> None
          ) 
        |> List.fold_left (fun acc next ->
            match next with
            | None -> acc
            | Some v -> acc @ [v]
          ) []
      in
      acc @ items
    end
  | Psig_type (_, tdecls) ->
    (* Retrieve all the doc comments attached to variant constructors *)
    let (items: DocItem.t list) = 
      List.fold_right (fun (decl: Parsetree.type_declaration) acc -> 
          match decl.ptype_kind with
          | Ptype_variant cdecls ->
            let constructors = List.fold_right (fun { Parsetree.pcd_attributes; pcd_name } acc -> 
                let constructors = Util.filter_map (fun ((id, payload) : Parsetree.attribute) ->
                    match id.txt, payload with 
                    | "ocaml.doc", PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(a,_));_},_); _}] -> 
                      let docstring=sprintf "@[<v>@ @[%s@]@]@." a in
                      Some { DocItem.name=pcd_name.txt; docstring }
                    | _ -> None
                  ) pcd_attributes
                in
                acc @ constructors
              ) cdecls [] 
            in
            acc @ [ DocItem.Doc_variant {constructors=constructors; docstring=""}] 
          | Ptype_abstract ->
            let docstrings = Util.filter_map getOCamlDocString decl.ptype_attributes in
            (match docstrings with
             | [docstring] ->
               let signature = Signature.fromSignatureItem signatureItem in
               acc @ [Doc_type {signature= signature; name=decl.ptype_name.txt; docstring=docstring} ]
             | _ -> acc)
          | _ -> acc
        ) tdecls []
    in
    acc @ items
  | Psig_module { pmd_attributes; pmd_name } ->
    (match extractSignatureAndDocstring signatureItem pmd_attributes with
    | Some (_signature, docstring) -> 
      let item = DocItem.Doc_module {name=pmd_name.txt; items=[]; docstring; } in
      acc @ [item]
    | None -> acc)
  | _ -> acc


let extract_signature_docs ~(filename: string) (xs : Parsetree.signature) =
  let items = List.fold_left extract_doc [] xs in
  let json = Json.Object [
      ("filename", String filename);
      ("items", Array (List.map (fun item -> DocItem.toJson item) items))
    ]
  in
  json |> Json.stringifyPretty |> print_endline
  

exception Not_supported of string

type backend = Parser: ('diagnostics) Res_driver.parsingEngine -> backend [@@unboxed]

let extract_file filename =
  let parsingEngine = match Filename.extension filename with
    | ".mli" ->  Parser Res_driver_ml_parser.parsingEngine
    | ".resi" -> Parser Res_driver.parsingEngine
    | ext -> raise (Not_supported ("'" ^ ext ^ "' extension is not supported"))
  in
  let Parser backend = parsingEngine in
  let parseResult = backend.parseInterface ~forPrinter:false ~filename in
  extract_signature_docs ~filename parseResult.parsetree

let () =   
  for i = 0 to Array.length Sys.argv - 1 do 
    if Sys.argv.(i) = "-file" && i + 1 < Array.length Sys.argv then begin 
      extract_file Sys.argv.(i+1);
      exit 0
    end
  done     
