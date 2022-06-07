(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* System configuration *)

val version: string
        (* The current version number of the system *)

val standard_library: string
        (* The directory containing the standard libraries *)

val syntax_kind : [ `ml | `reason | `rescript ] ref       

val bs_only : bool ref 

val unsafe_empty_array: bool ref 


val load_path: string list ref
        (* Directories in the search path for .cmi and .cmo files *)

val interface_suffix: string ref
        (* Suffix for interface file names *)

val cmi_magic_number: string
        (* Magic number for compiled interface files *)
val ast_intf_magic_number: string
        (* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number: string
        (* Magic number for file holding an implementation syntax tree *)
val cmt_magic_number: string
        (* Magic number for compiled interface files *)


val default_uncurry : bool ref 
val print_config : out_channel -> unit;;

val default_executable_name : string
val flexdll_dirs : string list
val safe_string : bool
val default_safe_string : bool
val afl_instrument : bool
val max_tag : int
val flat_float_array : bool
val flambda : bool
val architecture: string
        (* Name of processor type for the native-code compiler *)
val ext_obj: string
val ccomp_type: string
val ocamlopt_cflags : string
val ocamlopt_cppflags : string
val ocamlc_cflags : string
val ocamlc_cppflags : string
val c_compiler: string
val c_output_obj: string
val ar: string
val ext_lib: string
val ranlib: string
val system: string
val native_pack_linker: string
val mkdll: string
val mkexe: string
val mkmaindll: string
val cc_profile : string
val ext_dll: string
