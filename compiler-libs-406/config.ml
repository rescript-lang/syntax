let version = "4.06.1+BS"
let standard_library =
  let (//) = Filename.concat in   
  Filename.dirname Sys.executable_name // Filename.parent_dir_name //  "lib" // "ocaml"
let standard_library_default = standard_library
let syntax_kind = ref `ml
let bs_only = ref true
let unsafe_empty_array = ref true


and cmi_magic_number = "Caml1999I022"

and ast_impl_magic_number = "Caml1999M022"
and ast_intf_magic_number = "Caml1999N022"
and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"


(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)



let default_uncurry = ref false

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  (* print the magic number *)

  p "cmi_magic_number" cmi_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmt_magic_number" cmt_magic_number;
  flush oc;
;;

let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"
let flexdll_dirs = [];;

let safe_string = true
let default_safe_string = true

let afl_instrument = false

let max_tag = 243
let flat_float_array = true
let flambda = false
let architecture = "arm64"
let ext_obj = ".o"
let ccomp_type = "cc"
let ocamlopt_cflags = "-O2 -fno-strict-aliasing -fwrapv -pthread  "
let ocamlopt_cppflags = "-D_FILE_OFFSET_BITS=64 "
let ocamlc_cflags = "-O2 -fno-strict-aliasing -fwrapv -pthread  "
let ocamlc_cppflags = "-D_FILE_OFFSET_BITS=64 "
let c_compiler = "gcc"
let c_output_obj = "-o "
let ar = "ar"
let ext_lib = ".a"
let ranlib = "ranlib"
let system = "macosx"
let native_pack_linker = "ld -r -o "
let supports_shared_libraries = true
let mkdll, mkexe, mkmaindll =
  (* @@DRA Cygwin - but only if shared libraries are enabled, which we
     should be able to detect? *)
  if Sys.win32 || Sys.cygwin && supports_shared_libraries then
    try
      let flexlink =
        let flexlink = Sys.getenv "OCAML_FLEXLINK" in
        let f i =
          let c = flexlink.[i] in
          if c = '/' && Sys.win32 then '\\' else c in
        (String.init (String.length flexlink) f) ^ " " in
      flexlink ^ "",
      flexlink ^ " -exe",
      flexlink ^ " -maindll"
    with Not_found ->
      "gcc -shared                    -flat_namespace -undefined suppress -Wl,-no_compact_unwind                    ", "gcc -O2 -fno-strict-aliasing -fwrapv -pthread -Wall -Wdeclaration-after-statement -Werror -fno-common    -Wl,-no_compact_unwind", "gcc -shared                    -flat_namespace -undefined suppress -Wl,-no_compact_unwind                    "
  else
    "gcc -shared                    -flat_namespace -undefined suppress -Wl,-no_compact_unwind                    ", "gcc -O2 -fno-strict-aliasing -fwrapv -pthread -Wall -Wdeclaration-after-statement -Werror -fno-common    -Wl,-no_compact_unwind", "gcc -shared                    -flat_namespace -undefined suppress -Wl,-no_compact_unwind                    "
  let cc_profile = "-pg"
  let ext_dll = ".so"
