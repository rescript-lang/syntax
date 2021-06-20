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

  (* Recklessly ripped out of: https://github.com/ocaml/ocaml/blob/trunk/driver/compenv.ml#L90 *)
  let module_of_filename filename =
    let basename = Filename.basename filename in
    let name =
      try
        let pos = String.index basename '.' in
        String.sub basename 0 pos
      with Not_found -> basename
    in
    String.capitalize_ascii name
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
  type t =
    | Doc_value of {
        signature: string;
        name: string;
        docstring: string;
      }
    | Doc_variant of {
        signature: string;
        constructorDocs: (string * string) list; (* (constructorName, docstring) *)
        docstring: string;
      }
      (* abstract, record *)
    | Doc_type of {
        signature: string;
        name: string;
        docstring: string;
      }
    | Doc_record of {
        signature: string;
        name: string;
        labelDocs: (string * string) list; (* (labelName, docstring) *) 
        docstring: string;
      }
    | Doc_text of string
    | Doc_module of {
        name: string;
        docstring: string;
        items: t list;
      }
    | Doc_module_alias of {
        name: string;
        signature: string;
        docstring: string;
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
    | Doc_variant { signature; constructorDocs; docstring } ->
      let constructorDocsJson = Array (List.map (fun (name, docs) ->
          Array ( [String name; String docs])) constructorDocs)
      in
      Object [
        ("kind", String "Doc_variant");
        ("signature", String signature);
        ("constructorDocs", constructorDocsJson);
        ("docstring", String docstring)
      ]
    | Doc_record { signature; name; labelDocs; docstring } ->
      let labelDocsJson = Array (List.map (fun (labelName, labelDocs) ->
          Array ( [String labelName; String labelDocs])) labelDocs)
      in
      Object [
        ("kind", String "Doc_variant");
        ("signature", String signature);
        ("name", String name);
        ("labelDocs", labelDocsJson);
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
    | Doc_module_alias {name; signature; docstring} ->
      Object [
        ("kind", String "Doc_module_alias");
        ("name", String name);
        ("docstring", String docstring);
        ("signature", String signature);
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
  | Psig_type (recFlag, tdecls) ->
    let newDecls = tdecls |> List.map (fun declr ->
        let newDeclr = match declr.ptype_kind with
          | Ptype_variant cstrDecls ->
            let newCstrDecls =
              cstrDecls
              |> List.map (fun cdeclr ->
                  let filteredAttrs = List.filter filterDocAttrs cdeclr.pcd_attributes in
                  { cdeclr with pcd_attributes = filteredAttrs }
                )
            in 
            { declr with ptype_kind = Ptype_variant newCstrDecls }
          | Ptype_record labelDecls ->
            let newLabelDecls =
              labelDecls
              |> List.map (fun ldecl ->
                  let filteredAttrs = List.filter filterDocAttrs ldecl.pld_attributes in
                  { ldecl with pld_attributes = filteredAttrs }
                )
            in
            { declr with ptype_kind = Ptype_record newLabelDecls }
          | _ ->
            declr
        in
        let filteredAttrs = List.filter filterDocAttrs newDeclr.ptype_attributes in
        { newDeclr with ptype_attributes = filteredAttrs }
      )
    in
    let desc = Psig_type (recFlag, newDecls) in
    { sigItem with psig_desc = desc }
  | Psig_module desc ->
    let filteredAttrs = List.filter filterDocAttrs desc.pmd_attributes in
    let newDesc = { desc with pmd_attributes = filteredAttrs } in
    { sigItem with psig_desc = Psig_module newDesc }
  | _ -> sigItem

let extractDocstring (attrs: Parsetree.attributes) =
  let open Parsetree in
  let rec findFirstItem (attrs: Parsetree.attributes) =
    match attrs with
    | [] -> None
    | ({Location.txt = "ocaml.doc"}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(a,_));_},_); _}]):: _ ->
      let docstring = sprintf "@[<v>@ @[%s@]@]@." a in
      Some docstring
    | _::attrs -> findFirstItem attrs
  in
  findFirstItem attrs

module Signature = struct
  let fromSignatureItem item =
    Res_printer.printSignatureItem (drop_doc_attributes item) cmtTable |> Res_doc.toString ~width:80
  let fromStructureItem item =
    (* TODO drop_doc_attributes for structure_item *)
    Res_printer.printStructure [item] cmtTable |> Res_doc.toString ~width:80
end

let extractDocItem_attribute (attribute: Parsetree.attribute) = 
  match attribute with
    | ({Asttypes.txt="ocaml.text"}, payload) ->
      begin match payload with
        | Parsetree.PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(str,_));_},_); _}]
          -> Some (DocItem.Doc_text str)
        | _ -> None
      end
    | _ -> None

let extractDocItem_value_description (value_description: Parsetree.value_description) signature = 
  match value_description with
  | {pval_attributes; pval_name; _} ->
    let docstring = match extractDocstring pval_attributes with
      | Some docstring -> docstring
      | None -> ""
    in
    Some (DocItem.Doc_value({
        signature;
        name=pval_name.txt;
        docstring
      }))

let extractDocItem_type_declarations (tdecls: Parsetree.type_declaration list) signature = 
    (* Retrieve all the doc comments attached to variant constructors *)
    let (items: DocItem.t list) = 
      List.fold_right (fun (decl: Parsetree.type_declaration) acc -> 
          match decl.ptype_kind with
          | Ptype_variant cdecls ->
            let docstring = match extractDocstring decl.ptype_attributes with
              | Some docstring -> docstring
              | None -> ""
            in
            let constructorDocs = Util.filter_map (fun { Parsetree.pcd_attributes; pcd_name } ->
                (match extractDocstring pcd_attributes with
                  | Some docstring ->
                    Some (pcd_name.txt, docstring)
                  | None -> None)
              ) cdecls
            in
            acc @ [DocItem.Doc_variant {signature; constructorDocs; docstring}]
          | Ptype_record labelDecls ->
            let docstring = match extractDocstring decl.ptype_attributes with
              | Some docstring -> docstring
              | None -> ""
            in
            let labelDocs = Util.filter_map (fun { Parsetree.pld_attributes; pld_name } ->
                (match extractDocstring pld_attributes with
                  | Some docstring ->
                    Some (pld_name.txt, docstring)
                  | None -> None)
              ) labelDecls
            in
            acc @ [DocItem.Doc_record {name=decl.ptype_name.txt; signature=signature; labelDocs; docstring}]
          | Ptype_abstract ->
            let docstring = match extractDocstring decl.ptype_attributes with
              | Some str -> str
              | None -> ""
            in
            acc @ [Doc_type {signature= signature; name=decl.ptype_name.txt; docstring=docstring}]
          | _ -> acc
        ) tdecls []
    in
    (match items with
    | item :: _ -> Some item
    | _ -> None)

let rec extractSignatureDocItem (signatureItem: Parsetree.signature_item) : DocItem.t option =
  match signatureItem.psig_desc with
  | Psig_value value_description -> extractDocItem_value_description value_description (Signature.fromSignatureItem signatureItem)
  | Psig_type (_, tdecls) -> extractDocItem_type_declarations tdecls (Signature.fromSignatureItem signatureItem)
  | Psig_typext _ -> None (* TODO verify *)
  | Psig_exception _ -> None (* TODO verify *)
  | Psig_module { pmd_attributes; pmd_name; pmd_type } ->
    (match pmd_type.pmty_desc with
     | Pmty_signature signature ->
       let items = Util.filter_map extractSignatureDocItem signature in
       let docstring =
         match extractDocstring pmd_attributes with
         | Some str -> str
         | None -> ""
       in
       Some (DocItem.Doc_module {name=pmd_name.txt; items; docstring; })
     | Pmty_alias {txt = Ldot _} ->
       (*print_endline str;*)
       let signature = Signature.fromSignatureItem signatureItem in 
       let docstring =
         match extractDocstring pmd_attributes with
         | Some docstring -> docstring
         | None -> ""
       in
       Some (DocItem.Doc_module_alias {name=pmd_name.txt; docstring; signature})
     | _ -> None
    )
  | Psig_recmodule _ -> None (* TODO verify *)
  | Psig_modtype _ -> None (* TODO verify *)
  | Psig_open _ -> None (* TODO verify *)
  | Psig_include _ -> None (* TODO verify *)
  | Psig_class _ -> None (* TODO verify *)
  | Psig_class_type _ -> None (* TODO verify *)
  | Psig_attribute attribute -> extractDocItem_attribute attribute
  | Psig_extension _ -> None (* TODO verify *)

let (* rec *) extractStructureDocItem (structureItem: Parsetree.structure_item) : DocItem.t option =
  match structureItem.pstr_desc with
(*| Pstr_eval of Parsetree.expression * Parsetree.attributes *)
(*| Pstr_value of Asttypes.rec_flag * Parsetree.value_binding list *)
  | Pstr_primitive value_description -> extractDocItem_value_description value_description (Signature.fromStructureItem structureItem)
  | Pstr_type (_, tdecls) -> extractDocItem_type_declarations tdecls (Signature.fromStructureItem structureItem)
(*| Pstr_typext of Parsetree.type_extension *)
(*| Pstr_exception of Parsetree.extension_constructor *)
(*| Pstr_module of Parsetree.module_binding *)
(*| Pstr_recmodule of Parsetree.module_binding list *)
(*| Pstr_modtype of Parsetree.module_type_declaration *)
(*| Pstr_open of Parsetree.open_description *)
(*| Pstr_class of Parsetree.class_declaration list *)
(*| Pstr_class_type of Parsetree.class_type_declaration list *)
(*| Pstr_include of Parsetree.include_declaration *)
  | Pstr_attribute attribute -> extractDocItem_attribute attribute
(*| Pstr_extension of Parsetree.extension * Parsetree.attributes *)
  | _ -> None

let extract_signature_docs ~(filename: string) (signature : Parsetree.signature) =
  let name = Util.module_of_filename filename in
  let items = Util.filter_map extractSignatureDocItem signature in
  let root = DocItem.Doc_module {
      name;
      items;
      docstring="";
    } 
  in
  let json = Json.Object [
      ("filename", String filename);
      ("root", DocItem.toJson root);
    ]
  in
  json |> Json.stringifyPretty |> print_endline

let extract_structure_docs ~(filename: string) (signature : Parsetree.structure) =
  let name = Util.module_of_filename filename in
  let items = Util.filter_map extractStructureDocItem signature in
  let root = DocItem.Doc_module {
      name;
      items;
      docstring="";
    } 
  in
  let json = Json.Object [
      ("filename", String filename);
      ("root", DocItem.toJson root);
    ]
  in
  json |> Json.stringifyPretty |> print_endline

exception Not_supported of string

type backend = Parser: ('diagnostics) Res_driver.parsingEngine -> backend [@@unboxed]

let extract_signature_file filename parsingEngine =
  let Parser backend = parsingEngine in
  let parseResult = backend.parseInterface ~forPrinter:false ~filename in
  extract_signature_docs ~filename parseResult.parsetree

let extract_structure_file filename parsingEngine =
  let Parser backend = parsingEngine in
  let parseResult = backend.parseImplementation ~forPrinter:false ~filename in
  extract_structure_docs ~filename parseResult.parsetree

let extract_file filename =
  match Filename.extension filename with
    | ".mli" -> extract_signature_file filename (Parser Res_driver_ml_parser.parsingEngine)
    | ".ml" -> extract_structure_file filename (Parser Res_driver_ml_parser.parsingEngine)
    | ".res" -> extract_structure_file filename (Parser Res_driver.parsingEngine)
    | ".resi" -> extract_signature_file filename (Parser Res_driver.parsingEngine)
    | ext -> raise (Not_supported ("'" ^ ext ^ "' extension is not supported"))

let usage cmd = begin
  Printf.eprintf "Usage: %s -file <input.(ml|mli|resi)>\n" cmd;
  exit 1
end

let () = match Sys.argv with
  | [| _; "-file"; input_file |] -> begin
    try
      extract_file input_file;
      exit 0
    with Syntaxerr.Error err ->
      fprintf Format.err_formatter "@[%a@]@." Syntaxerr.report_error err;
      exit 1
    end
  | _ -> usage Sys.argv.(0)
