(* typed_ast.ml - Version-neutral view of typed implementations

   CMT files are marshalled without type information. Reading one with the
   typedtree definition of another ReScript release misinterprets constructor
   tags and can crash the process, so this module decodes the typed
   implementation with the layout of the compiler that wrote it and keeps only
   what value usage analysis needs. *)

open Stdlib

type expr =
  | Ident of string list  (** Resolved value path, e.g. ["Utils"; "U"; "f"] *)
  | Let of binding list * expr
  | Struct of item list  (** Local or first-class module structure *)
  | Node of expr list  (** Any other expression, by its sub-expressions *)

and binding = {
  name : string option;  (** [None] unless the pattern is a plain variable *)
  line : int;
  expr : expr;
}

and item =
  | Values of binding list
  | Module of string * item list
  | Include of item list
  | Eval of expr

type layout = Rescript_11 | Rescript_12

exception Unsupported_layout of string

(* ReScript 12 appended [cmt_extra_info] to [cmt_infos], which tells the two
   typedtree layouts apart without trusting anything inside the tree. *)
let detect_layout (cmt_info : Cmt_format.cmt_infos) =
  match Obj.size (Obj.repr cmt_info) with
  | 13 -> Rescript_11
  | 14 -> Rescript_12
  | fields ->
      raise
        (Unsupported_layout
           (Printf.sprintf "unknown cmt_infos layout with %d fields" fields))

let rec path_to_list = function
  | Path.Pident id -> [ Ident.name id ]
  | Path.Pdot (p, s, _) -> path_to_list p @ [ s ]
  | Path.Papply _ as p -> [ Path.name p ]

let option_to_list f = function Some x -> [ f x ] | None -> []

module V11 = struct
  open Typedtree

  let rec expr e =
    match e.exp_desc with
    | Texp_ident (path, _, _) -> Ident (path_to_list path)
    | Texp_let (_, vbs, body) -> Let (List.map binding vbs, expr body)
    | Texp_function { cases; _ } -> Node (List.concat_map case cases)
    | Texp_apply (f, args) ->
        Node
          (expr f :: List.filter_map (fun (_, arg) -> Option.map expr arg) args)
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
            | _, Overridden (_, e) -> Some (expr e)
            | _, Kept _ -> None)
        in
        Node (overridden @ option_to_list expr extended_expression)
    | Texp_field (e, _, _)
    | Texp_assert e
    | Texp_lazy e
    | Texp_letexception (_, e) ->
        Node [ expr e ]
    | Texp_setfield (e1, _, _, e2) | Texp_sequence (e1, e2) | Texp_while (e1, e2)
      ->
        Node [ expr e1; expr e2 ]
    | Texp_ifthenelse (e1, e2, eo) ->
        Node (expr e1 :: expr e2 :: option_to_list expr eo)
    | Texp_for (_, _, e1, e2, _, e3) -> Node [ expr e1; expr e2; expr e3 ]
    | Texp_send (e, _, eo) -> Node (expr e :: option_to_list expr eo)
    | Texp_letmodule (_, _, m, body) -> Node [ module_expr m; expr body ]
    | Texp_pack m -> module_expr m
    | Texp_constant _ | Texp_new _ | Texp_instvar _ | Texp_setinstvar _
    | Texp_override _ | Texp_object _ | Texp_unreachable
    | Texp_extension_constructor _ ->
        Node []

  and case c = option_to_list expr c.c_guard @ [ expr c.c_rhs ]

  and binding vb =
    let name =
      match vb.vb_pat.pat_desc with
      | Tpat_var (id, _) -> Some (Ident.name id)
      | _ -> None
    in
    {
      name;
      line = vb.vb_pat.pat_loc.loc_start.pos_lnum;
      expr = expr vb.vb_expr;
    }

  and module_expr m =
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
        match item.str_desc with
        | Tstr_eval (e, _) -> [ Eval (expr e) ]
        | Tstr_value (_, vbs) -> [ Values (List.map binding vbs) ]
        | Tstr_module mb ->
            [ Module (Ident.name mb.mb_id, module_items mb.mb_expr) ]
        | Tstr_recmodule mbs ->
            List.map
              (fun mb -> Module (Ident.name mb.mb_id, module_items mb.mb_expr))
              mbs
        | Tstr_include (incl : include_declaration) ->
            [ Include (module_items incl.incl_mod) ]
        | _ -> [])
      s.str_items
end

module V12 = struct
  open Typedtree_v12

  let rec expr e =
    match e.exp_desc with
    | Texp_ident (path, _, _) -> Ident (path_to_list path)
    | Texp_let (_, vbs, body) -> Let (List.map binding vbs, expr body)
    | Texp_function { case = c; _ } -> Node (case c)
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
    | Texp_send (e, _, eo) -> Node (expr e :: option_to_list expr eo)
    | Texp_letmodule (_, _, m, body) -> Node [ module_expr m; expr body ]
    | Texp_pack m -> module_expr m
    | Texp_constant _ | Texp_extension_constructor _ -> Node []

  and case c = option_to_list expr c.c_guard @ [ expr c.c_rhs ]

  and binding vb =
    let name =
      match vb.vb_pat.pat_desc with
      | Tpat_var (id, _) -> Some (Ident.name id)
      | _ -> None
    in
    {
      name;
      line = vb.vb_pat.pat_loc.loc_start.pos_lnum;
      expr = expr vb.vb_expr;
    }

  and module_expr m =
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
end

(* Decode the typed implementation of a CMT file, or [None] for interfaces and
   partial (error-recovery) annotations. *)
let implementation (cmt_info : Cmt_format.cmt_infos) =
  match cmt_info.cmt_annots with
  | Cmt_format.Implementation structure -> (
      match detect_layout cmt_info with
      | Rescript_11 -> Some (V11.structure structure)
      | Rescript_12 ->
          Some (V12.structure (Obj.magic structure : Typedtree_v12.structure)))
  | _ -> None
