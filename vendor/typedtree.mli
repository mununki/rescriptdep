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

(** Abstract syntax tree after typing *)

(* Type expressions for the core language *)

open Types

type pattern = {
  pat_desc : pattern_desc;
  pat_loc : Location.t;
  pat_extra : (pat_extra * Location.t * string (* Parsetree.attributes *)) list;
  pat_type : type_expr;
  mutable pat_env : unit; (* Env.t; *)
  pat_attributes : string; (* Parsetree.attributes; *)
}

and pat_extra =
  | Tpat_constraint of core_type
  | Tpat_type of Path.t * string
  | Tpat_unpack
  | Tpat_open of Path.t * unit (* Env.t *)

and pattern_desc =
  | Tpat_any
  | Tpat_var of Ident.t * string Asttypes.loc
  | Tpat_alias of pattern * string * string
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of unit * constructor_description * pattern list
  | Tpat_variant of string * pattern option * row_desc ref
  | Tpat_record of (unit * label_description * pattern) list * closed_flag
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * row_desc option
  | Tpat_lazy of pattern

and expression = {
  exp_desc : expression_desc;
  exp_loc : Location.t;
  exp_extra : (exp_extra * Location.t * string (* Parsetree.attributes *)) list;
  exp_type : type_expr;
  exp_env : unit; (* Env.t; *)
  exp_attributes : string; (* Parsetree.attributes; *)
}

and exp_extra =
  | Texp_constraint of core_type
  | Texp_coerce of core_type option * core_type
  | Texp_open of override_flag * Path.t * unit * unit
  | Texp_poly of core_type option
  | Texp_newtype of string

and expression_desc =
  | Texp_ident of Path.t * unit * value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * value_binding list * expression
  | Texp_function of {
      arg_label : string;
      param : unit;
      cases : case list;
      partial : partial;
    }
  | Texp_apply of expression * (string * expression option) list
  | Texp_match of expression * case list * case list * partial
  | Texp_try of expression * case list
  | Texp_tuple of expression list
  | Texp_construct of Location.t * constructor_description * expression list
  | Texp_variant of string * expression option
  | Texp_record of {
      fields : (label_description * record_label_definition) array;
      representation : Types.record_representation;
      extended_expression : expression option;
    }
  | Texp_field of expression * Location.t * label_description
  | Texp_setfield of expression * Location.t * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      unit * string * expression * expression * direction_flag * expression
  | Texp_send of expression * meth * expression option
  | Texp_new of Path.t * unit * class_declaration
  | Texp_instvar of Path.t * Path.t * string
  | Texp_setinstvar of Path.t * Path.t * string * expression
  | Texp_override of Path.t * (Path.t * string * expression) list
  | Texp_letmodule of unit * string * module_expr * expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_letop of { let_ : binding_op; ands : binding_op list; body : case }
  | Texp_unreachable
  | Texp_extension_constructor of Location.t * Path.t
  | Texp_open of open_declaration * expression

and meth = Tmeth_name of string | Tmeth_val of Ident.t
and case = { c_lhs : pattern; c_guard : expression option; c_rhs : expression }

and record_label_definition =
  | Kept of Types.type_expr
  | Overridden of Location.t * expression

and binding_op = {
  bop_op_path : Path.t;
  bop_op_val : value_description;
  bop_op_type : Types.type_expr;
  bop_exp : expression;
  bop_loc : Location.t;
}

and value_binding = {
  vb_pat : pattern;
  vb_expr : expression;
  vb_attributes : string; (* Parsetree.attributes; *)
  vb_loc : Location.t;
}

and module_binding = {
  mb_id : Ident.t;
  mb_name : string Asttypes.loc;
  mb_expr : module_expr;
  mb_attributes : string; (* Parsetree.attributes; *)
  mb_loc : Location.t;
}

and module_coercion =
  | Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of unit
  | Tcoerce_alias of Path.t * module_coercion

and module_type = {
  mty_desc : module_type_desc;
  mty_type : Types.module_type;
  mty_env : unit; (* Env.t; *)
  mty_loc : Location.t;
  mty_attributes : string; (* Parsetree.attributes; *)
}

and module_type_desc =
  | Tmty_ident of Path.t * unit
  | Tmty_signature of signature
  | Tmty_functor of unit * string * module_type option * module_type
  | Tmty_with of module_type * (Path.t * Location.t * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * unit

and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : unit; (* Env.t; *)
}

and signature_item = {
  sig_desc : signature_item_desc;
  sig_env : unit; (* Env.t; *)
  sig_loc : Location.t;
}

and signature_item_desc =
  | Tsig_value of string * value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typesubst of type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of extension_constructor
  | Tsig_module of string * module_declaration
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of string * modtype_declaration
  | Tsig_modtypesubst of string * modtype_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
  | Tsig_attribute of string (* Parsetree.attribute *)

and module_declaration = {
  md_id : Ident.t;
  md_name : string Asttypes.loc;
  md_type : module_type;
  md_attributes : string; (* Parsetree.attributes; *)
  md_loc : Location.t;
}

and module_expr = {
  mod_desc : module_expr_desc;
  mod_loc : Location.t;
  mod_type : Types.module_type;
  mod_env : unit; (* Env.t; *)
  mod_attributes : string; (* Parsetree.attributes; *)
}

and module_expr_desc =
  | Tmod_ident of Path.t * unit
  | Tmod_structure of structure
  | Tmod_functor of unit * string * module_type option * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
  | Tmod_unpack of expression * Types.module_type

and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : unit; (* Env.t; *)
}

and structure_item = {
  str_desc : structure_item_desc;
  str_loc : Location.t;
  str_env : unit; (* Env.t; *)
}

and structure_item_desc =
  | Tstr_eval of expression * string (* Parsetree.attributes; *)
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of extension_constructor
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_declaration
  | Tstr_class of class_declaration list * string list
  | Tstr_class_type of class_type_declaration list * string list
  | Tstr_include of include_declaration
  | Tstr_attribute of string (* Parsetree.attribute *)

and module_type_declaration = {
  mtd_id : Ident.t;
  mtd_name : string Asttypes.loc;
  mtd_type : module_type option;
  mtd_attributes : string; (* Parsetree.attributes; *)
  mtd_loc : Location.t;
}

and open_declaration = {
  open_expr : module_expr;
  open_bound_items : Types.signature;
  open_override : override_flag;
  open_env : unit; (* Env.t; *)
  open_loc : Location.t;
  open_attributes : string; (* Parsetree.attributes; *)
}

and open_description = {
  open_path : Path.t;
  open_txt : Longident.t Asttypes.loc;
  open_override : override_flag;
  open_loc : Location.t;
  open_attributes : string; (* Parsetree.attributes; *)
}

and include_declaration = {
  incl_mod : module_expr;
  incl_type : Types.signature;
  incl_loc : unit; (* Location.t; *)
  incl_attributes : string; (* Parsetree.attributes; *)
}

and include_description = {
  incl_mod : module_type;
  incl_type : Types.signature;
  incl_loc : Location.t;
  incl_attributes : string; (* Parsetree.attributes; *)
}

and with_constraint =
  | Twith_type of type_declaration
  | Twith_module of Path.t
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t

and core_type = {
  ctyp_desc : core_type_desc;
  ctyp_type : type_expr;
  ctyp_env : unit; (* Env.t; *)
  ctyp_loc : Location.t;
  ctyp_attributes : string; (* Parsetree.attributes; *)
}

and core_type_desc =
  | Ttyp_var of string
  | Ttyp_any
  | Ttyp_poly of string list * core_type
  | Ttyp_constr of Path.t * unit * core_type list
  | Ttyp_arrow of string * core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_object of unit * closed_flag
  | Ttyp_class of Path.t * unit * core_type list
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * closed_flag * string list option
  | Ttyp_package of package_type

and value_description = {
  val_id : Ident.t;
  val_name : string Asttypes.loc;
  val_desc : core_type;
  val_val : Types.value_description;
  val_prim : string list;
  val_loc : Location.t;
  val_attributes : string; (* Parsetree.attributes; *)
}

and type_declaration = {
  typ_id : Ident.t;
  typ_name : string Asttypes.loc;
  typ_params : (core_type * unit) list;
  typ_type : Types.type_declaration;
  typ_cstrs : unit;
  typ_kind : type_kind;
  typ_private : private_flag;
  typ_manifest : core_type option;
  typ_loc : Location.t;
  typ_attributes : string; (* Parsetree.attributes; *)
}

and type_kind =
  | Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open

and label_declaration = {
  ld_id : Ident.t;
  ld_name : string Asttypes.loc;
  ld_mutable : mutable_flag;
  ld_type : core_type;
  ld_loc : Location.t;
  ld_attributes : string; (* Parsetree.attributes; *)
}

and constructor_declaration = {
  cd_id : Ident.t;
  cd_name : string Asttypes.loc;
  cd_args : constructor_arguments;
  cd_res : core_type option;
  cd_loc : Location.t;
  cd_attributes : string; (* Parsetree.attributes; *)
}

and constructor_arguments =
  | Cstr_tuple of core_type list
  | Cstr_record of label_declaration list

and type_extension = {
  tyext_path : Path.t;
  tyext_txt : Longident.t Asttypes.loc;
  tyext_params : (core_type * unit) list;
  tyext_constructors : extension_constructor list;
  tyext_private : private_flag;
  tyext_loc : Location.t;
  tyext_attributes : string; (* Parsetree.attributes; *)
}

and extension_constructor = {
  ext_id : Ident.t;
  ext_name : string Asttypes.loc;
  ext_type : Types.extension_constructor;
  ext_kind : extension_constructor_kind;
  ext_loc : Location.t;
  ext_attributes : string; (* Parsetree.attributes; *)
}

and extension_constructor_kind =
  | Text_decl of constructor_arguments * core_type option
  | Text_rebind of Path.t * unit

and class_declaration = unit
and class_description = unit
and class_type_declaration = unit

and class_structure = {
  cstr_self : pattern;
  cstr_fields : class_field list;
  cstr_type : Types.class_signature;
  cstr_meths : unit; (* No longer Meths.t *)
}

and class_field = {
  cf_desc : class_field_desc;
  cf_loc : Location.t;
  cf_attributes : string; (* Parsetree.attributes; *)
}

and class_field_kind =
  | Tcfk_val of unit * mutable_flag * unit * expression
  | Tcfk_method of unit * private_flag * unit * expression
  | Tcfk_initializer of expression
  | Tcfk_constraint of core_type * core_type

and class_field_desc =
  | Tcf_inherit of override_flag * class_expr * string option * unit * unit
  | Tcf_val of string * string * mutable_flag * class_field_kind
  | Tcf_method of string * string * private_flag * class_field_kind
  | Tcf_constraint of core_type * core_type
  | Tcf_initializer of expression
  | Tcf_attribute of string (* Parsetree.attribute *)

and class_expr = {
  cl_desc : class_expr_desc;
  cl_loc : Location.t;
  cl_type : Types.class_type;
  cl_env : unit; (* Env.t; *)
  cl_attributes : string; (* Parsetree.attributes; *)
}

and class_expr_desc =
  | Tcl_constraint of
      class_expr * class_type option * string list * string list * unit list
  | Tcl_structure of class_structure
  | Tcl_fun of string * pattern * class_expr * unit
  | Tcl_apply of class_expr * (string * expression option) list
  | Tcl_let of rec_flag * value_binding list * class_expr
  | Tcl_ident of Path.t * unit * core_type list

and module_type_constraint =
  | Tmodtype_implicit
  | Tmodtype_explicit of module_type

and row_field =
  | Ttag of
      string Asttypes.loc
      * string (* Parsetree.attributes; *)
      * bool
      * core_type list
  | Tinherit of core_type

and package_type = {
  pack_path : Path.t;
  pack_fields : (string * core_type) list;
  pack_type : Types.module_type;
  pack_txt : Longident.t Asttypes.loc;
}

and constant =
  | Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

and constructor_description = {
  cstr_name : string; (* Constructor name *)
  cstr_res : type_expr; (* Type of the result *)
  cstr_existentials : type_expr list; (* list of existentials *)
  cstr_args : type_expr list; (* Type of the arguments *)
  cstr_arity : int; (* Number of arguments *)
  cstr_tag : constructor_tag; (* Tag for heap blocks *)
  cstr_consts : int; (* Number of constant constructors *)
  cstr_nonconsts : int; (* Number of non-constant constructors *)
  cstr_normal : int; (* Constructor number in the list (for GC) *)
  cstr_private : private_flag; (* Read-only constructor? *)
  cstr_generalized : bool; (* Generalized constructor? *)
  cstr_loc : Location.t;
  cstr_attributes : string; (* Parsetree.attributes *)
  cstr_inlined : type_declaration option;
}

and constructor_tag =
  | Cstr_constant of int (* Constant constructor (an int) *)
  | Cstr_block of int (* Regular constructor (a block) *)
  | Cstr_unboxed (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool
(* Extension constructor
                                   true if a constant false if a block *)

and label_description = {
  lbl_name : string; (* Short name *)
  lbl_res : type_expr; (* Type of the result *)
  lbl_arg : type_expr; (* Type of the argument *)
  lbl_mut : mutable_flag; (* Is this a mutable field? *)
  lbl_pos : int; (* Position in block *)
  lbl_all : label_description array; (* All the labels in this type *)
  lbl_repres : record_representation; (* Representation for this record *)
  lbl_private : private_flag; (* Read-only field? *)
  lbl_loc : Location.t;
  lbl_attributes : string; (* Parsetree.attributes *)
}

and closed_flag = Closed | Open
and override_flag = Override | Fresh
and direction_flag = Upto | Downto
and private_flag = Private | Public
and partial = Partial | Total
