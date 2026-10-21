(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Binary annotations. The typed tree layout depends on the compiler that
   wrote the file, so the trees are kept opaque here and decoded by a
   compiler-specific backend. *)
type binary_annots =
  | Packed of Obj.t * string list
  | Implementation of Obj.t
  | Interface of Obj.t
  | Partial_implementation of Obj.t
  | Partial_interface of Obj.t

(* Simplified cmt_infos *)
type cmt_infos = {
  cmt_modname : string;
  cmt_annots : binary_annots;
  cmt_value_dependencies :
    (Types.value_description * Types.value_description) list;
  cmt_comments : (string * unit) list; (* Location.t -> unit *)
  cmt_args : string array;
  cmt_sourcefile : string option;
  cmt_builddir : string;
  cmt_loadpath : string list;
  cmt_source_digest : string option;
  cmt_initial_env : Env.t;
  cmt_imports : (string * Digest.t option) list;
  cmt_interface_digest : Digest.t option;
  cmt_use_summaries : bool;
}

(* Error handling *)
exception Error of string

(* Magic number prefixes; the format version follows them *)
val cmi_magic_prefix : string
val cmt_magic_prefix : string
val read_cmt : string -> cmt_infos

(* Like [read_cmt], but also returns the CMT magic number of the file *)
val read_cmt_with_magic : string -> string * cmt_infos
val read_cmi : string -> Cmi_format.cmi_infos
