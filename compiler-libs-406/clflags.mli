(** Optimization parameters represented as ints indexed by round number. *)
module Int_arg_helper : sig
    type parsed
  
    val parse : string -> string -> parsed ref -> unit
  
    type parse_result =
      | Ok
      | Parse_failed of exn
    val parse_no_error : string -> parsed ref -> parse_result
  
    val get : key:int -> parsed -> int
  end
  
  (** Optimization parameters represented as floats indexed by round number. *)
  module Float_arg_helper : sig
    type parsed
  
    val parse : string -> string -> parsed ref -> unit
  
    type parse_result =
      | Ok
      | Parse_failed of exn
    val parse_no_error : string -> parsed ref -> parse_result
  
    val get : key:int -> parsed -> float
  end
  

val output_name : string option ref
val include_dirs : string list ref

val debug : bool ref
val fast : bool ref

val nopervasives : bool ref
val open_modules : string list ref
val preprocessor : string option ref
val all_ppx : string list ref
val annotations : bool ref
val binary_annotations : bool ref
val noassert : bool ref
val verbose : bool ref
val principal : bool ref
val real_paths : bool ref
val applicative_functors : bool ref
val error_size : int ref
val transparent_modules : bool ref
val dump_source : bool ref
val dump_parsetree : bool ref
val dump_typedtree : bool ref
val dump_rawlambda : bool ref
val dump_lambda : bool ref
val dont_write_files : bool ref
val keep_docs : bool ref
val keep_locs : bool ref


val parse_color_setting : string -> Misc.Color.setting option
val color : Misc.Color.setting option ref

val unboxed_types : bool ref

val reset_dump_state: unit -> unit 


type mli_status =  Mli_exists | Mli_non_exists
val assume_no_mli : mli_status ref
val bs_vscode : bool
val dont_record_crc_unit : string option ref
val bs_gentype : string option ref
val no_assert_false : bool ref
val dump_location : bool ref

val use_threads : bool ref
val use_vmthreads : bool ref
val compile_only : bool ref
val no_std_include : bool ref
val std_include_dir : unit -> string list
val unsafe_string : bool ref
val gprofile : bool ref
val afl_instrument : bool ref
val recursive_types : bool ref
val opaque : bool ref
val native_code : bool ref
val afl_inst_ratio : int ref
val bytecode_compatible_32 : bool ref
val no_auto_link : bool ref
val link_everything : bool ref
val classic : bool ref
val strict_sequence : bool ref
val strict_formats : bool ref
val force_slash : bool ref
val optimize_for_speed : bool ref
val dlcode : bool ref
val runtime_variant : string ref
val c_compiler : string option ref
val clambda_checks : bool ref
val keep_asm_file : bool ref
val keep_startup_file : bool ref

val inline_threshold : Float_arg_helper.parsed ref
val inline_toplevel_threshold : Int_arg_helper.parsed ref
val simplify_rounds : int option ref
val inline_max_unroll : Int_arg_helper.parsed ref
val inline_call_cost : Int_arg_helper.parsed ref
val inline_alloc_cost : Int_arg_helper.parsed ref
val inline_prim_cost : Int_arg_helper.parsed ref
val inline_branch_cost : Int_arg_helper.parsed ref
val inline_indirect_cost : Int_arg_helper.parsed ref
val inline_lifting_benefit : Int_arg_helper.parsed ref
val default_inline_branch_factor : float
val inline_branch_factor : Float_arg_helper.parsed ref
val default_inline_max_depth : int
val inline_max_depth : Int_arg_helper.parsed ref
val classic_inlining : bool ref
val default_simplify_rounds : int ref
val inline_alloc_cost : Int_arg_helper.parsed ref

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

(** Set all the inlining arguments for a round.
    The default is set if no round is provided. *)
val use_inlining_arguments_set : ?round:int -> inlining_arguments -> unit

val o1_arguments : inlining_arguments
val o2_arguments : inlining_arguments
val o3_arguments : inlining_arguments
val unbox_closures : bool ref
val unbox_closures_factor : int ref
val remove_unused_arguments : bool ref
val inlining_report : bool ref
val dump_flambda_verbose : bool ref
val flambda_invariant_checks : bool ref
val unbox_closures_factor : int ref
val default_unbox_closures_factor : int
val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val pic_code : bool ref
val profile_columns : Profile.column list ref
val all_ccopts : string list ref
val make_package : bool ref
val std_include_flag : string -> string
val make_archive : bool ref
