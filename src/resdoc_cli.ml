(* could use Ast_iterator for in depth traversing *)
open Format

module Json = struct
  type t =
    | String of string
    | Number of float
    | Array of t list
    | Object of (string * t) list
    | True
    | False
    | Null

  let toStringArray lst =
      "[" ^
      List.fold_left (fun acc next ->
          let value = "\"" ^ next ^ "\"" in
          if acc = "" then
             value
          else
            acc ^ "," ^ value
          ) "" lst
      ^ "]"

  (* Without escaping the values as an explicit string *)
  let toArray lst =
      "[" ^
      List.fold_left (fun acc next ->
          if acc = "" then
            next
          else
            acc ^ "," ^ next
          ) "" lst
      ^ "]"

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
    | ((String (value))[@explicit_arity ]) ->
      (("\"")[@reason.raw_literal "\\\""]) ^
      ((escape value) ^ (("\"")[@reason.raw_literal "\\\""]))
    | ((Number (num))[@explicit_arity ]) -> string_of_number num
    | ((Array (items))[@explicit_arity ]) ->
      (("[")[@reason.raw_literal "["]) ^
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

module DocItem = struct
  type t =
    | Doc_value of {
        signature: string;
        name: string;
        docstring: string;
      }
    | Doc_variant
    | Doc_text of string
    | Doc_module of t list

  let rec toJson t =
    let open Json in
    match t with 
    | Doc_value {signature; name; docstring } -> 
      Object [
        ("kind", String "Doc_value");
        ("signature", String signature);
        ("name", String name);
        ("docstring", String docstring)
      ]
    | Doc_variant ->
      Object [
        ("kind", String "Doc_variant")
      ]
    | Doc_text docstring ->
      Object [
        ("kind", String "Doc_text");
        ("docstring", String docstring)
      ]
    | Doc_module items ->
      Object [
        ("kind", String "Doc_module");
        ("items", Array (List.map (fun item -> toJson item) items))
      ]
end

(* all documentation data for a module interface *)

(* Docstrings for val definitions *)
module ValDocItem = struct
  type t = {
    type_sig: string;
    name: string;
    text: string;
  }

  let toJson t =
    let open Json in
    Object [("type_sig", String t.type_sig); ("name", String t.name); ("text", String t.text)]
end


module ModuleDocData = struct
  type t = {
    module_docs: string list;
    val_docs: ValDocItem.t list;
  }

  let toJson ~filename t =
    let open Json in
    Object [
      ("filename", String filename);
      ("module_docs", Array (List.map (fun str -> String str) t.module_docs));
      ("val_docs", Array (List.map (fun item -> ValDocItem.toJson item) t.val_docs))
    ]
end

(* Used for the ReScript printer facilities *)
let cmtTable = Res_comments_table.make ()

let drop_doc_attributes (sigItem: Parsetree.signature_item) : Parsetree.signature_item =
  let open Parsetree in
  match sigItem.psig_desc with
  | Psig_value ({pval_attributes; _} as desc) ->
    let filteredAttrs = List.filter (fun ((id, _) : attribute) ->
        match id.txt with
          | "ocaml.doc"
          | "ocaml.txt" ->
            false
          | _ -> true) pval_attributes
    in
    let desc = Psig_value ({ desc with pval_attributes = filteredAttrs }) in
    { sigItem with psig_desc = desc }
  | _ -> sigItem


let extract_doc (acc: DocItem.t list) (x: Parsetree.signature_item) : DocItem.t list = 
  match x.psig_desc with
  | Psig_attribute  ({Asttypes.txt="ocaml.text"}, payload)
    ->
    begin match payload with
      | Parsetree.PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(str,_));_},_); _}]
        -> acc @ [ DocItem.Doc_text str ]
      | _ -> acc
    end
  | Psig_value {pval_attributes; pval_name; _} 
    -> 
    begin 
      let items = 
        pval_attributes 
        |> List.map (fun ((id, payload) : Parsetree.attribute) -> 
            match id.txt, payload with 
            | "ocaml.doc", PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(a,_));_},_); _}]
              -> 
              let signature =
                Res_printer.printSignatureItem (drop_doc_attributes x) cmtTable |> Res_doc.toString ~width:80
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
          ) [] in
      acc @ items
    end
  | Psig_type _ ->
    Res_printer.printSignatureItem x cmtTable |> Res_doc.toString ~width:80 |> print_endline;
    acc
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
