(* Parsetree docs: https://sanette.github.io/ocaml-api/4.06/Parsetree.html *)

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

module DropDocAttributes = struct
  let mapper =
    let filterDocAttrs ((id, _): Parsetree.attribute) =
      match id.txt with
      | "ocaml.doc" | "ocaml.txt" -> false
      | _ -> true
    in
    let open Ast_mapper in
    { default_mapper with
      attributes = fun mapper attributes ->
        let filteredAttrs = List.filter filterDocAttrs attributes in
        default_mapper.attributes mapper filteredAttrs;
    }

  let signature_item item = mapper.signature_item mapper item
  let structure_item item = mapper.structure_item mapper item
end

module Signature = struct
  let from_signature_item item =
    Res_printer.printSignatureItem (DropDocAttributes.signature_item item) cmtTable |> Res_doc.toString ~width:80

  let from_structure_item item =
    Res_printer.printStructure [DropDocAttributes.structure_item item] cmtTable |> Res_doc.toString ~width:80
end

module ExtractDocStrings = struct
  let from_attributes (attrs: Parsetree.attributes) =
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

  let rec make =
    let items = ref [] in
    let generate item =
      items := item :: !items
    in
    let signature_stack = ref [] in
    let open Ast_iterator in
    let iterator = {
      default_iterator with

      signature_item = begin fun iterator signature_item ->
        let signature = Signature.from_signature_item signature_item in
        signature_stack := signature :: !signature_stack;
        default_iterator.signature_item iterator signature_item;
        signature_stack := List.tl !signature_stack;
      end;

      structure_item = begin fun iterator structure_item ->
        let signature = Signature.from_structure_item structure_item in
        signature_stack := signature :: !signature_stack;
        default_iterator.structure_item iterator structure_item;
        signature_stack := List.tl !signature_stack;
      end;

      attribute = begin fun iterator attribute ->
        begin match attribute with
        | ( { Asttypes.txt="ocaml.text" },
            Parsetree.PStr [ { pstr_desc = Pstr_eval ( { pexp_desc = Pexp_constant (Pconst_string(str, _)); _ }, _); _ } ]
          ) -> DocItem.Doc_text str |> generate
        | _ -> ()
        end;
        default_iterator.attribute iterator attribute;
      end;

      value_description = begin fun iterator value_description ->
        begin match value_description with
        | {pval_attributes; pval_name; _} ->
          let signature = List.hd !signature_stack in
          let docstring = match from_attributes pval_attributes with
            | Some docstring -> docstring
            | None -> ""
          in
          DocItem.Doc_value({
            signature;
            name = pval_name.txt;
            docstring;
          }) |> generate
        end;
        default_iterator.value_description iterator value_description;
      end;

      type_declaration = begin fun iterator type_declaration ->
        let signature = List.hd !signature_stack in
        let docstring = match from_attributes type_declaration.ptype_attributes with
          | Some docstring -> docstring
          | None -> ""
        in
        begin match type_declaration.ptype_kind with
        | Ptype_variant cdecls ->
          let constructorDocs = Util.filter_map (fun { Parsetree.pcd_attributes; pcd_name } ->
              (match from_attributes pcd_attributes with
                | Some docstring ->
                  Some (pcd_name.txt, docstring)
                | None -> None)
            ) cdecls
          in
          DocItem.Doc_variant {signature; constructorDocs; docstring} |> generate
        | Ptype_record labelDecls ->
          let labelDocs = Util.filter_map
            begin fun { Parsetree.pld_attributes; pld_name } ->
              match from_attributes pld_attributes with
              | Some docstring -> Some (pld_name.txt, docstring)
              | None -> None
            end
            labelDecls
          in
          DocItem.Doc_record {
            name = type_declaration.ptype_name.txt;
            signature;
            labelDocs;
            docstring;
          } |> generate
        | Ptype_abstract ->
          DocItem.Doc_type {
            signature;
            name=type_declaration.ptype_name.txt;
            docstring;
          } |> generate
        | _ -> ()
        end;
        default_iterator.type_declaration iterator type_declaration;
      end;

      module_declaration = begin fun iterator module_declaration ->
        match module_declaration with
        | { pmd_attributes; pmd_name; pmd_type } ->
          match pmd_type.pmty_desc with
          | Pmty_signature signature ->
            (* recursive call; uses new iterator and does not recurse into this one *)
            let items = from_signature signature in
            let docstring = match from_attributes pmd_attributes with
              | Some str -> str
              | None -> ""
            in
            DocItem.Doc_module {name=pmd_name.txt; items; docstring; } |> generate
          | Pmty_alias {txt = Ldot _} ->
            let signature = List.hd !signature_stack in
            let docstring = match from_attributes pmd_attributes with
              | Some docstring -> docstring
              | None -> ""
            in
            DocItem.Doc_module_alias {name=pmd_name.txt; docstring; signature} |> generate;
            default_iterator.module_declaration iterator module_declaration;
          | _ ->
            default_iterator.module_declaration iterator module_declaration;
      end;

      (* TODO: add more iterator methods *)

    } in
    (iterator, items)

  and from_signature signature =
    let (iterator, items) = make in
    iterator.signature iterator signature;
    List.rev !items

  and from_structure structure =
    let (iterator, items) = make in
    iterator.structure iterator structure;
    List.rev !items
end

exception Not_supported of string

type backend = Parser: ('diagnostics) Res_driver.parsingEngine -> backend [@@unboxed]

let extract_signature_file filename parsingEngine =
  let Parser backend = parsingEngine in
  let parseResult = backend.parseInterface ~forPrinter:false ~filename in
  ExtractDocStrings.from_signature parseResult.parsetree

let extract_structure_file filename parsingEngine =
  let Parser backend = parsingEngine in
  let parseResult = backend.parseImplementation ~forPrinter:false ~filename in
  ExtractDocStrings.from_structure parseResult.parsetree

let extract_file filename =
  let items = match Filename.extension filename with
    | ".mli" -> extract_signature_file filename (Parser Res_driver_ml_parser.parsingEngine)
    | ".ml" -> extract_structure_file filename (Parser Res_driver_ml_parser.parsingEngine)
    | ".res" -> extract_structure_file filename (Parser Res_driver.parsingEngine)
    | ".resi" -> extract_signature_file filename (Parser Res_driver.parsingEngine)
    | ext -> raise (Not_supported ("'" ^ ext ^ "' extension is not supported"))
  in
  let name = Util.module_of_filename filename in
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
  json |> Json.stringifyPretty ~indent:0 |> print_endline

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
