(* compiler_v13.ml - Typed tree decoder for ReScript 13.0.0-alpha.6 *)

open Stdlib
open Typed_ast
open Typedtree_v13

(* Constructor counts (constant, non-constant) of the decoded variants *)
let expression_desc_constructors = (2, 30)
let module_expr_desc_constructors = (0, 6)
let structure_item_desc_constructors = (0, 12)

let rec expr e =
  if not (has_known_constructor expression_desc_constructors e.exp_desc) then
    Node []
  else
    match e.exp_desc with
    | Texp_ident (path, _, _) -> Ident (path_to_list path)
    | Texp_let (_, vbs, body) -> Let (List.map binding vbs, expr body)
    | Texp_function { body; _ } ->
        (* Default values of optional parameters are bound at the head of
           the body *)
        Node [ expr body ]
    | Texp_apply { funct; args; _ } ->
        Node
          (expr funct
          :: List.filter_map (fun (_, arg) -> Option.map expr arg) args)
    | Texp_match (e, cases, exn_cases, _) ->
        Node (expr e :: List.concat_map case (cases @ exn_cases))
    | Texp_try (e, cases) -> Node (expr e :: List.concat_map case cases)
    | Texp_tuple es | Texp_array es | Texp_construct (_, _, es) ->
        Node (List.map expr es)
    | Texp_variant (_, eo) -> Node (option_to_list expr eo)
    | Texp_record { fields; extended_expression; _ } ->
        let overridden =
          Array.to_list fields
          |> List.filter_map (function
            | _, Overridden (_, e), _ -> Some (expr e)
            | _, Kept _, _ -> None)
        in
        Node (overridden @ option_to_list expr extended_expression)
    | Texp_field (e, _, _) | Texp_assert e | Texp_letexception (_, e) ->
        Node [ expr e ]
    | Texp_setfield (e1, _, _, e2) | Texp_sequence (e1, e2) | Texp_while (e1, e2)
      ->
        Node [ expr e1; expr e2 ]
    | Texp_ifthenelse (e1, e2, eo) ->
        Node (expr e1 :: expr e2 :: option_to_list expr eo)
    | Texp_for (_, _, e1, e2, _, e3) -> Node [ expr e1; expr e2; expr e3 ]
    | Texp_for_of (_, _, e1, e2) | Texp_for_await_of (_, _, e1, e2) ->
        Node [ expr e1; expr e2 ]
    | Texp_object_get (e, _) -> Node [ expr e ]
    | Texp_object_set (e1, _, e2) -> Node [ expr e1; expr e2 ]
    | Texp_object_literal fields ->
        Node (List.map (fun (_, e) -> expr e) fields)
    | Texp_tagged_template { tag; values; _ } ->
        Node (expr tag :: List.map expr values)
    | Texp_template { values; _ } -> Node (List.map expr values)
    | Texp_letmodule (_, _, m, body) -> Node [ module_expr m; expr body ]
    | Texp_pack m -> module_expr m
    | Texp_constant _ | Texp_break | Texp_continue
    | Texp_extension_constructor _ ->
        Node []

and case c = option_to_list expr c.c_guard @ [ expr c.c_rhs ]

and binding vb =
  let name =
    match vb.vb_pat.pat_desc with
    | Tpat_var (id, _) -> Some (Ident.name id)
    | _ -> None
  in
  { name; line = vb.vb_pat.pat_loc.loc_start.pos_lnum; expr = expr vb.vb_expr }

and module_expr m =
  if not (has_known_constructor module_expr_desc_constructors m.mod_desc) then
    Node []
  else
    match m.mod_desc with
    | Tmod_ident _ -> Node []
    | Tmod_structure s -> Struct (structure s)
    | Tmod_functor (_, _, _, body) | Tmod_constraint (body, _, _, _) ->
        module_expr body
    | Tmod_apply (f, arg, _) -> Node [ module_expr f; module_expr arg ]
    | Tmod_unpack (e, _) -> expr e

and module_items m =
  match module_expr m with Struct items -> items | e -> [ Eval e ]

and structure s =
  List.concat_map
    (fun item ->
      if
        not
          (has_known_constructor structure_item_desc_constructors item.str_desc)
      then []
      else
        match item.str_desc with
        | Tstr_eval (e, _) -> [ Eval (expr e) ]
        | Tstr_value (_, vbs) -> [ Values (List.map binding vbs) ]
        | Tstr_module mb ->
            [ Module (Ident.name mb.mb_id, module_items mb.mb_expr) ]
        | Tstr_recmodule mbs ->
            List.map
              (fun mb -> Module (Ident.name mb.mb_id, module_items mb.mb_expr))
              mbs
        | Tstr_include incl -> [ Include (module_items incl.incl_mod) ]
        | _ -> [])
    s.str_items

(* Decode the [Implementation] annotation of a CMT file *)
let implementation (tree : Obj.t) = structure (Obj.obj tree : structure)
