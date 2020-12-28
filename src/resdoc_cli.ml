(* could use Ast_iterator for in depth traversing *)
open Format

(* all documentation data for a module interface *)

(* doc element for val signatures *)
type val_doc = {
  type_sig: string;
  name: string;
  text: string;
}

type doc_data = {
  module_docs: string list;
  val_docs: val_doc list;
}

let cmtTable = Res_comments_table.make ()

let extract_doc (acc: doc_data) (x: Parsetree.signature_item) : doc_data = 
  match x.psig_desc with
  | Psig_attribute  ({Asttypes.txt="ocaml.text"}, payload)
    ->
    begin match payload with
      | Parsetree.PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(str,_));_},_); _}]
        -> {acc with module_docs = (acc.module_docs @ [str])}
      | _ -> acc
    end
  | Psig_value {pval_attributes; pval_name; _} 
    -> 
    begin 
      let val_docs = 
        pval_attributes 
        |> List.map (fun ((id, payload) : Parsetree.attribute) -> 
            match id.txt, payload with 
            | "ocaml.doc", PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string(a,_));_},_); _}]
              -> 
              let type_sig =
                Res_printer.printSignatureItem x cmtTable |> Res_doc.toString ~width:80
              in
              Some ({
                  type_sig;
                  name=pval_name.txt;
                  text=sprintf "@[<v>@ @[%s@]@]@." a
                })
            | _ -> None
          ) 
        |> List.fold_left (fun acc next ->
            match next with
            | None -> acc
            | Some v -> acc @ [v]
          ) [] in
      { acc with val_docs=(acc.val_docs @ val_docs) }
    end
  | _ -> acc

let extract_docs (xs : Parsetree.signature) =
  let acc = {
    module_docs=[];
    val_docs=[]
  } in
  let result = List.fold_left extract_doc acc xs in
  List.iter print_endline result.module_docs;
  List.iter (fun v -> 
      print_endline "--- BEGIN ---";
      print_endline v.name;
      print_endline v.type_sig;
      print_endline "--- END ---"
    ) result.val_docs
  
let extract_file fname = 
  let chan  = open_in_bin fname in 
  let lexbuf = Lexing.from_channel chan in 
  let signature = Parse.interface lexbuf in 
  extract_docs signature

let () =   
  for i = 0 to Array.length Sys.argv - 1 do 
    if Sys.argv.(i) = "-file" && i + 1 < Array.length Sys.argv then begin 
      extract_file Sys.argv.(i+1);
      exit 0
    end
  done     
