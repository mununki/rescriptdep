(* Types for the OCaml core language *)

(* Type expressions for the core language *)

(* Module for handling the cache of method calls *)
module Meths_cache : sig
  type 'a t = 'a list
end

type type_expr = { mutable desc : type_desc; mutable level : int; id : int }

and type_desc =
  | Tvar of string option
  | Tarrow of string * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr (* for copying *)
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * string list * type_expr list
(* Replaced Longident.t list with string list to avoid dependency *)

and row_desc = {
  row_fields : (string * row_field) list;
  row_more : type_expr;
  row_bound : unit; (* Location.t list *)
  row_fixed : bool;
  row_closed : bool;
}

and row_field =
  | Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
  | Rabsent

and abbrev_memo =
  | Mnil
  | Mcons of Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_kind = Fvar of field_kind option ref | Fpresent | Fabsent
and commutable = Cok | Cunknown | Clink of commutable ref

(* Value descriptions *)

type value_description = {
  val_type : type_expr;
  val_kind : value_kind;
  val_loc : unit; (* Location.t *)
  val_attributes : string; (* Parsetree.attributes *)
}

and value_kind =
  | Val_reg
  | Val_prim of string
  | Val_ivar of mutable_flag * string
  | Val_self of (unit * type_expr) Meths_cache.t ref * unit * string list
  | Val_anc of (unit * type_expr) Meths_cache.t ref * string
  | Val_unbound

and mutable_flag = Immutable | Mutable

(* Type definitions *)

type type_declaration = {
  type_params : type_expr list;
  type_arity : int;
  type_kind : type_kind;
  type_private : private_flag;
  type_manifest : type_expr option;
  type_variance : (bool * bool) list;
  type_newtype_level : (int * int) option;
  type_loc : unit; (* Location.t *)
  type_attributes : string; (* Parsetree.attributes *)
  type_immediate : bool;
  type_unboxed : unboxed_status;
}

and type_kind =
  | Type_abstract
  | Type_record of label_declaration list * record_representation
  | Type_variant of constructor_declaration list
  | Type_open

and record_representation = Record_regular | Record_float | Record_unboxed

and label_declaration = {
  ld_id : string; (* Ident.t *)
  ld_mutable : mutable_flag;
  ld_type : type_expr;
  ld_loc : unit; (* Location.t *)
  ld_attributes : string; (* Parsetree.attributes *)
}

and constructor_declaration = {
  cd_id : string; (* Ident.t *)
  cd_args : constructor_arguments;
  cd_res : type_expr option;
  cd_loc : unit; (* Location.t *)
  cd_attributes : string; (* Parsetree.attributes *)
}

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

and unboxed_status = Unboxed | Boxed
and private_flag = Private | Public

type module_type = { mutable desc : module_type_desc; id : int }

and module_type_desc =
  | Tmty_ident of Path.t
  | Tmty_signature of signature
  | Tmty_functor of string * module_type option * module_type
  | Tmty_with of module_type * (Path.t * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t

and signature = signature_item list

and signature_item = {
  sig_desc : signature_item_desc;
  sig_env : unit; (* Env.t *)
  sig_loc : unit; (* Location.t *)
}

and signature_item_desc =
  | Tsig_value of string * value_description
  | Tsig_type of string * type_declaration * rec_status
  | Tsig_typext of type_extension
  | Tsig_exception of extension_constructor
  | Tsig_module of string * module_declaration
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of string * modtype_declaration
  | Tsig_open of unit
  | Tsig_include of unit
  | Tsig_class of unit
  | Tsig_class_type of unit
  | Tsig_attribute of unit

and module_declaration = {
  md_id : string; (* Ident.t *)
  md_type : module_type;
  md_attributes : string; (* Parsetree.attributes *)
  md_loc : unit; (* Location.t *)
}

and module_expr = {
  mod_desc : module_expr_desc;
  mod_loc : unit; (* Location.t *)
  mod_type : module_type;
  mod_env : unit; (* Env.t *)
  mod_attributes : string; (* Parsetree.attributes *)
}

and module_expr_desc =
  | Tmod_ident of Path.t
  | Tmod_structure of structure
  | Tmod_functor of string * module_type option * module_expr
  | Tmod_apply of module_expr * module_expr * unit
  | Tmod_constraint of module_expr * module_type * unit * unit
  | Tmod_unpack of unit * module_type

and structure = structure_item list

and structure_item = {
  str_desc : structure_item_desc;
  str_loc : unit; (* Location.t *)
  str_env : unit; (* Env.t *)
}

and structure_item_desc =
  | Tstr_eval of unit
  | Tstr_value of rec_flag * unit list
  | Tstr_primitive of string * value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of extension_constructor
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of string * modtype_declaration
  | Tstr_open of unit
  | Tstr_class of unit
  | Tstr_class_type of unit
  | Tstr_include of unit
  | Tstr_attribute of unit

and module_binding = {
  mb_id : string; (* Ident.t *)
  mb_name : string; (* Location.t *)
  mb_expr : module_expr;
  mb_attributes : string; (* Parsetree.attributes *)
  mb_loc : unit; (* Location.t *)
}

and modtype_declaration = {
  mtd_type : module_type option;
  mtd_attributes : string; (* Parsetree.attributes *)
  mtd_loc : unit; (* Location.t *)
}

and rec_flag = Nonrecursive | Recursive
and rec_status = Trec_not | Trec_first | Trec_next

and with_constraint =
  | Twith_type of type_declaration
  | Twith_module of Path.t
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t

and type_extension = {
  tyext_path : Path.t;
  tyext_params : type_expr list;
  tyext_constructors : extension_constructor list;
  tyext_private : private_flag;
  tyext_attributes : string; (* Parsetree.attributes *)
}

and extension_constructor = {
  ext_id : string; (* Ident.t *)
  ext_name : string; (* Location.t *)
  ext_type : unit;
  ext_kind : extension_constructor_kind;
  ext_loc : unit; (* Location.t *)
  ext_attributes : string; (* Parsetree.attributes *)
}

and extension_constructor_kind =
  | Text_decl of constructor_arguments * type_expr option
  | Text_rebind of Path.t

(* Class types *)
type class_type = {
  cty_desc : class_type_desc;
  cty_type : type_expr;
  cty_path : Path.t;
  cty_loc : unit; (* Location.t *)
}

and class_type_desc =
  | Tcty_constr of Path.t * type_expr list
  | Tcty_signature of class_signature
  | Tcty_arrow of string * type_expr * class_type

and class_signature = {
  csig_self : type_expr;
  csig_fields : class_type_field list;
  csig_type : type_expr;
}

and class_type_field = {
  ctf_desc : class_type_field_desc;
  ctf_loc : unit; (* Location.t *)
}

and class_type_field_desc =
  | Tcf_val of string * mutable_flag * type_expr
  | Tcf_method of string * private_flag * type_expr
  | Tcf_constraint of type_expr * type_expr
  | Tcf_inher of class_signature
