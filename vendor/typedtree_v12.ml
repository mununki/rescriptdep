(* Typedtree layout written by ReScript 12 (compiler/ml/typedtree.mli).

   CMT files are read with [input_value], so constructor order and arity must
   match the compiler exactly. Only the nodes that value usage analysis walks
   are spelled out; everything it never inspects is kept opaque. *)

type opaque = Obj.t

type pattern = {
  pat_desc : pattern_desc;
  pat_loc : Location.t;
  pat_extra : opaque;
  pat_type : opaque;
  mutable pat_env : opaque;
  pat_attributes : opaque;
}

and pattern_desc =
  | Tpat_any
  | Tpat_var of Ident.t * string Asttypes.loc
  | Tpat_alias of pattern * Ident.t * string Asttypes.loc
  | Tpat_constant of opaque
  | Tpat_tuple of pattern list
  | Tpat_construct of opaque * opaque * pattern list
  | Tpat_variant of opaque * pattern option * opaque
  | Tpat_record of opaque * opaque
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * opaque

and expression = {
  exp_desc : expression_desc;
  exp_loc : Location.t;
  exp_extra : opaque;
  exp_type : opaque;
  exp_env : opaque;
  exp_attributes : opaque;
}

and expression_desc =
  | Texp_ident of Path.t * opaque * opaque
  | Texp_constant of opaque
  | Texp_let of opaque * value_binding list * expression
  | Texp_function of {
      arg_label : opaque;
      arity : opaque;
      param : Ident.t;
      case : case;
      partial : opaque;
      async : bool;
    }
  | Texp_apply of {
      funct : expression;
      args : (opaque * expression option) list;
      partial : bool;
      transformed_jsx : bool;
    }
  | Texp_match of expression * case list * case list * opaque
  | Texp_try of expression * case list
  | Texp_tuple of expression list
  | Texp_construct of opaque * opaque * expression list
  | Texp_variant of opaque * expression option
  | Texp_record of {
      fields : (opaque * record_label_definition * bool) array;
      representation : opaque;
      extended_expression : expression option;
    }
  | Texp_field of expression * opaque * opaque
  | Texp_setfield of expression * opaque * opaque * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of opaque * opaque * expression * expression * opaque * expression
  | Texp_send of expression * opaque * expression option
  | Texp_letmodule of Ident.t * opaque * module_expr * expression
  | Texp_letexception of opaque * expression
  | Texp_assert of expression
  | Texp_pack of module_expr
  | Texp_extension_constructor of opaque * Path.t

and case = { c_lhs : pattern; c_guard : expression option; c_rhs : expression }

and record_label_definition =
  | Kept of opaque
  | Overridden of opaque * expression

and value_binding = {
  vb_pat : pattern;
  vb_expr : expression;
  vb_attributes : opaque;
  vb_loc : Location.t;
}

and module_expr = {
  mod_desc : module_expr_desc;
  mod_loc : Location.t;
  mod_type : opaque;
  mod_env : opaque;
  mod_attributes : opaque;
}

and module_expr_desc =
  | Tmod_ident of Path.t * opaque
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * opaque * opaque * module_expr
  | Tmod_apply of module_expr * module_expr * opaque
  | Tmod_constraint of module_expr * opaque * opaque * opaque
  | Tmod_unpack of expression * opaque

and structure = {
  str_items : structure_item list;
  str_type : opaque;
  str_final_env : opaque;
}

and structure_item = {
  str_desc : structure_item_desc;
  str_loc : Location.t;
  str_env : opaque;
}

and structure_item_desc =
  | Tstr_eval of expression * opaque
  | Tstr_value of opaque * value_binding list
  | Tstr_primitive of opaque
  | Tstr_type of opaque * opaque
  | Tstr_typext of opaque
  | Tstr_exception of opaque
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of opaque
  | Tstr_open of opaque
  | Tstr_include of include_declaration
  | Tstr_attribute of opaque

and module_binding = {
  mb_id : Ident.t;
  mb_name : opaque;
  mb_expr : module_expr;
  mb_attributes : opaque;
  mb_loc : Location.t;
}

and include_declaration = {
  incl_mod : module_expr;
  incl_type : opaque;
  incl_loc : Location.t;
  incl_attributes : opaque;
}
