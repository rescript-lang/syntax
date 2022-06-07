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
