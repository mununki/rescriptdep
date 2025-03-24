(* Format for cmt and cmti files in ReScript compiler *)

open Stdlib

let read_magic_number ic =
  let len_magic_number = String.length Config.cmt_magic_number in
  really_input_string ic len_magic_number

(* Binary annotations *)
type binary_annots =
  | Packed of Types.signature * string list
  | Implementation of Typedtree.structure
  | Interface of Typedtree.signature
  | Partial_implementation of binary_part array
  | Partial_interface of binary_part array

and binary_part =
  | Partial_structure of Typedtree.structure
  | Partial_structure_item of Typedtree.structure_item
  | Partial_expression of Typedtree.expression
  | Partial_pattern of Typedtree.pattern
  | Partial_class_expr of unit
  | Partial_signature of Typedtree.signature
  | Partial_signature_item of Typedtree.signature_item
  | Partial_module_type of Typedtree.module_type

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

let read filename =
  (*  Printf.fprintf stderr "Cmt_format.read %s\n%!" filename; *)
  let ic = open_in_bin filename in
  try
    let magic_number = read_magic_number ic in
    let cmi, cmt =
      if magic_number = Config.cmt_magic_number then (None, Some (input_cmt ic))
      else if magic_number = Config.cmi_magic_number then
        let cmi = Cmi_format.input_cmi ic in
        let cmt =
          try
            let magic_number = read_magic_number ic in
            if magic_number = Config.cmt_magic_number then
              let cmt = input_cmt ic in
              Some cmt
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

let read_cmt filename =
  match read filename with
  | _, None -> raise (Error "Not a typedtree")
  | _, Some cmt -> cmt

let read_cmi filename =
  match read filename with
  | None, _ -> raise (Error "Not an interface")
  | Some cmi, _ -> cmi
