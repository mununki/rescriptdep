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

open Stdlib

let read_magic_number ic =
  let len_magic_number = String.length Config.cmt_magic_number in
  really_input_string ic len_magic_number

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

let input_cmt ic = (input_value ic : cmt_infos)

(* Several compiler releases write different format versions, so [read]
   accepts any version and returns the CMT magic number with the infos.
   Callers must check the version before reading fields of the infos. *)
let cmi_magic_prefix = "Caml1999I"
let cmt_magic_prefix = "Caml1999T"
let has_prefix prefix magic_number = String.starts_with ~prefix magic_number

let read filename =
  (*  Printf.fprintf stderr "Cmt_format.read %s\n%!" filename; *)
  let ic = open_in_bin filename in
  try
    let magic_number = read_magic_number ic in
    let cmi, cmt =
      if has_prefix cmt_magic_prefix magic_number then
        (None, Some (magic_number, input_cmt ic))
      else if has_prefix cmi_magic_prefix magic_number then
        let cmi = Cmi_format.input_cmi ic in
        let cmt =
          try
            let magic_number = read_magic_number ic in
            if has_prefix cmt_magic_prefix magic_number then
              let cmt = input_cmt ic in
              Some (magic_number, cmt)
            else None
          with _ -> None
        in
        (Some cmi, cmt)
      else raise (Error "Invalid magic number")
    in
    close_in ic;
    (*    Printf.fprintf stderr "Cmt_format.read done\n%!"; *)
    (cmi, cmt)
  with e ->
    close_in ic;
    raise e

let read_cmt_with_magic filename =
  match read filename with
  | _, None -> raise (Error "Not a typedtree")
  | _, Some cmt -> cmt

let read_cmt filename = snd (read_cmt_with_magic filename)

let read_cmi filename =
  match read filename with
  | None, _ -> raise (Error "Not an interface")
  | Some cmi, _ -> cmi
