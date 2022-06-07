
module Int_arg_helper = Arg_helper.Make (struct
module Key = struct
  include Numbers.Int
  let of_string = int_of_string
end

module Value = struct
  include Numbers.Int
  let of_string = int_of_string
end
end)
module Float_arg_helper = Arg_helper.Make (struct
module Key = struct
  include Numbers.Int
  let of_string = int_of_string
end

module Value = struct
  include Numbers.Float
  let of_string = float_of_string
end
end)

let simplify_rounds = ref None        (* -rounds *)

let default_inline_threshold = if Config.flambda then 10. else 10. /. 8.
let inline_toplevel_multiplier = 16
let default_inline_toplevel_threshold =
  int_of_float ((float inline_toplevel_multiplier) *. default_inline_threshold)
let default_inline_call_cost = 5
let default_inline_alloc_cost = 7
let default_inline_prim_cost = 3
let default_inline_branch_cost = 5
let default_inline_indirect_cost = 4
let default_inline_branch_factor = 0.1
let default_inline_lifting_benefit = 1300
let default_inline_max_unroll = 0
let default_inline_max_depth = 1

let inline_max_unroll =
  ref (Int_arg_helper.default default_inline_max_unroll)
let inline_call_cost = ref (Int_arg_helper.default default_inline_call_cost)
let inline_prim_cost = ref (Int_arg_helper.default default_inline_prim_cost)
let inline_branch_cost =
  ref (Int_arg_helper.default default_inline_branch_cost)
let inline_indirect_cost =
  ref (Int_arg_helper.default default_inline_indirect_cost)
let inline_branch_factor =
  ref (Float_arg_helper.default default_inline_branch_factor)
let inline_lifting_benefit =
  ref (Int_arg_helper.default default_inline_lifting_benefit)

let inline_threshold = ref (Float_arg_helper.default default_inline_threshold)
let inline_toplevel_threshold =
  ref (Int_arg_helper.default default_inline_toplevel_threshold)

let default_inline_max_depth = 1
let inline_max_depth =
  ref (Int_arg_helper.default default_inline_max_depth)

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
let afl_inst_ratio = ref 100           (* -afl-inst-ratio *)
and bytecode_compatible_32 = ref false  (* -compat-32 *)
and no_auto_link = ref false            (* -noautolink *)
and link_everything = ref false         (* -linkall *)
and classic = ref false                 (* -nolabels *)
and strict_sequence = ref false         (* -strict-sequence *)
and strict_formats = ref false          (* -strict-formats *)
let force_slash = ref false             (* for ocamldep *)
let optimize_for_speed = ref true       (* -compact *)
let dlcode = ref true (* not -nodynlink *)
let runtime_variant = ref "";;      (* -runtime-variant *)
let c_compiler = ref (None: string option) (* -cc *)
let clambda_checks = ref false          (* -clambda-checks *)
let keep_asm_file = ref false           (* -S *)
let keep_startup_file = ref false       (* -dstartup *)

let classic_inlining = ref false       (* -Oclassic *)
let default_simplify_rounds = ref 1        (* -rounds *)
let inline_alloc_cost = ref (Int_arg_helper.default default_inline_alloc_cost)

type inlining_arguments = {
  inline_call_cost : int option;
  inline_alloc_cost : int option;
  inline_prim_cost : int option;
  inline_branch_cost : int option;
  inline_indirect_cost : int option;
  inline_lifting_benefit : int option;
  inline_branch_factor : float option;
  inline_max_depth : int option;
  inline_max_unroll : int option;
  inline_threshold : float option;
  inline_toplevel_threshold : int option;
}

let set_int_arg round (arg:Int_arg_helper.parsed ref) default value =
  let value : int =
    match value with
    | None -> default
    | Some value -> value
  in
  match round with
  | None ->
    arg := Int_arg_helper.set_base_default value
             (Int_arg_helper.reset_base_overrides !arg)
  | Some round ->
    arg := Int_arg_helper.add_base_override round value !arg

let set_float_arg round (arg:Float_arg_helper.parsed ref) default value =
  let value =
    match value with
    | None -> default
    | Some value -> value
  in
  match round with
  | None ->
    arg := Float_arg_helper.set_base_default value
             (Float_arg_helper.reset_base_overrides !arg)
  | Some round ->
    arg := Float_arg_helper.add_base_override round value !arg

let use_inlining_arguments_set ?round (arg:inlining_arguments) =
  let set_int = set_int_arg round in
  let set_float = set_float_arg round in
  set_int inline_call_cost default_inline_call_cost arg.inline_call_cost;
  set_int inline_alloc_cost default_inline_alloc_cost arg.inline_alloc_cost;
  set_int inline_prim_cost default_inline_prim_cost arg.inline_prim_cost;
  set_int inline_branch_cost
    default_inline_branch_cost arg.inline_branch_cost;
  set_int inline_indirect_cost
    default_inline_indirect_cost arg.inline_indirect_cost;
  set_int inline_lifting_benefit
    default_inline_lifting_benefit arg.inline_lifting_benefit;
  set_float inline_branch_factor
    default_inline_branch_factor arg.inline_branch_factor;
  set_int inline_max_depth
    default_inline_max_depth arg.inline_max_depth;
  set_int inline_max_unroll
    default_inline_max_unroll arg.inline_max_unroll;
  set_float inline_threshold
    default_inline_threshold arg.inline_threshold;
  set_int inline_toplevel_threshold
    default_inline_toplevel_threshold arg.inline_toplevel_threshold

(* o1 is the default *)
let o1_arguments = {
  inline_call_cost = None;
  inline_alloc_cost = None;
  inline_prim_cost = None;
  inline_branch_cost = None;
  inline_indirect_cost = None;
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = None;
  inline_max_unroll = None;
  inline_threshold = None;
  inline_toplevel_threshold = None;
}

let classic_arguments = {
  inline_call_cost = None;
  inline_alloc_cost = None;
  inline_prim_cost = None;
  inline_branch_cost = None;
  inline_indirect_cost = None;
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = None;
  inline_max_unroll = None;
  (* [inline_threshold] matches the current compiler's default.
     Note that this particular fraction can be expressed exactly in
     floating point. *)
  inline_threshold = Some (10. /. 8.);
  (* [inline_toplevel_threshold] is not used in classic mode. *)
  inline_toplevel_threshold = Some 1;
}

let o2_arguments = {
  inline_call_cost = Some (2 * default_inline_call_cost);
  inline_alloc_cost = Some (2 * default_inline_alloc_cost);
  inline_prim_cost = Some (2 * default_inline_prim_cost);
  inline_branch_cost = Some (2 * default_inline_branch_cost);
  inline_indirect_cost = Some (2 * default_inline_indirect_cost);
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = Some 2;
  inline_max_unroll = None;
  inline_threshold = Some 25.;
  inline_toplevel_threshold = Some (25 * inline_toplevel_multiplier);
}

let o3_arguments = {
  inline_call_cost = Some (3 * default_inline_call_cost);
  inline_alloc_cost = Some (3 * default_inline_alloc_cost);
  inline_prim_cost = Some (3 * default_inline_prim_cost);
  inline_branch_cost = Some (3 * default_inline_branch_cost);
  inline_indirect_cost = Some (3 * default_inline_indirect_cost);
  inline_lifting_benefit = None;
  inline_branch_factor = Some 0.;
  inline_max_depth = Some 3;
  inline_max_unroll = Some 1;
  inline_threshold = Some 50.;
  inline_toplevel_threshold = Some (50 * inline_toplevel_multiplier);
}
let unbox_closures = ref false          (* -unbox-closures *)
let default_unbox_closures_factor = 10
let remove_unused_arguments = ref false (* -remove-unused-arguments *)
let inlining_report = ref false    (* -inlining-report *)
and dump_flambda_verbose = ref false    (* -dflambda-verbose *)
let flambda_invariant_checks = ref true (* -flambda-invariants *)

let default_unbox_closures_factor = 10
let unbox_closures_factor =
  ref default_unbox_closures_factor      (* -unbox-closures-factor *)

let objfiles = ref ([] : string list)   (* .cmo and .cma files *)
and ccobjs = ref ([] : string list)     (* .o, .a, .so and -cclib -lxxx *)
and dllibs = ref ([] : string list)     (* .so and -dllib -lxxx *)
let pic_code = ref (match Config.architecture with (* -fPIC *)
                     | "amd64" -> true
                     | _       -> false)
let profile_columns : Profile.column list ref = ref [] (* -dprofile/-dtimings *)
