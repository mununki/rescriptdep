(* Format for cmt and cmti files in ReScript compiler *)

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

val read_cmt : string -> cmt_infos
val read_cmi : string -> Cmi_format.cmi_infos
