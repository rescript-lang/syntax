



let  output_name = ref (None : string option) (* -o *)
and include_dirs = ref ([] : string list)(* -I *)
and debug = ref false                   (* -g *)
and fast = ref false                    (* -unsafe *)

and nopervasives = ref false            (* -nopervasives *)
and preprocessor = ref(None : string option) (* -pp *)
and all_ppx = ref ([] : string list)        (* -ppx *)
let annotations = ref false             (* -annot *)
let binary_annotations = ref false      (* -annot *)
and noassert = ref false                (* -noassert *)
and verbose = ref false                 (* -verbose *)
and open_modules = ref []               (* -open *)
and principal = ref false               (* -principal *)
and real_paths = ref true               (* -short-paths *)
and applicative_functors = ref true     (* -no-app-funct *)
and error_size = ref 500                (* -error-size *)
and transparent_modules = ref false     (* -trans-mod *)
let dump_source = ref false             (* -dsource *)
let dump_parsetree = ref false          (* -dparsetree *)
and dump_typedtree = ref false          (* -dtypedtree *)
and dump_rawlambda = ref false          (* -drawlambda *)
and dump_lambda = ref false             (* -dlambda *)
and no_std_include = ref false          (* -nostdlib *)



let dont_write_files = ref false        (* set to true under ocamldoc *)


let reset_dump_state () = begin 
  dump_source := false;
  dump_parsetree := false;
  dump_typedtree := false;
  dump_rawlambda := false
end




let keep_docs = ref false              (* -keep-docs *)
let keep_locs = ref true               (* -keep-locs *)




let parse_color_setting = function
  | "auto" -> Some Misc.Color.Auto
  | "always" -> Some Misc.Color.Always
  | "never" -> Some Misc.Color.Never
  | _ -> None
let color = ref None ;; (* -color *)

let unboxed_types = ref false




type mli_status =  Mli_exists | Mli_non_exists
let assume_no_mli = ref Mli_non_exists
let bs_vscode =
    try ignore @@ Sys.getenv "BS_VSCODE" ; true with _ -> false
    (* We get it from environment variable mostly due to
       we don't want to rebuild when flip on or off
    *)
let dont_record_crc_unit : string option ref = ref None
let bs_gentype = ref None
let no_assert_false = ref false
let dump_location = ref true

let use_threads = ref false
let use_vmthreads = ref false
let compile_only = ref false

let std_include_dir () =
  if !no_std_include then [] else [Config.standard_library]
;;

let unsafe_string =
  if Config.safe_string then ref false
  else ref (not Config.default_safe_string)

let gprofile = ref false                (* -p *)

let afl_instrument = ref Config.afl_instrument (* -afl-instrument *)
let recursive_types = ref false         (* -rectypes *)
and opaque = ref false                  (* -opaque *)

let native_code = ref false             (* set to true under ocamlopt *)
