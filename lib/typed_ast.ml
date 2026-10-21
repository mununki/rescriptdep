(* typed_ast.ml - Version-neutral view of typed implementations

   CMT files are marshalled without type information. Reading one with the
   typedtree definition of another ReScript release misinterprets constructor
   tags and can crash the process, so each supported compiler has a decoder
   (Compiler_v11, Compiler_v12, ...) that turns its typed tree into this view,
   which keeps only what value usage analysis needs. *)

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

let rec path_to_list = function
  | Path.Pident id -> [ Ident.name id ]
  | Path.Pdot (p, s, _) -> path_to_list p @ [ s ]
  | Path.Papply _ as p -> [ Path.name p ]

let option_to_list f = function Some x -> [ f x ] | None -> []

(* A decoder only knows the constructors of its compiler release. Matching on
   a constructor added by a later release would jump to an arbitrary branch,
   so decoders skip values whose tag is outside [(constants, blocks)], the
   numbers of constant and non-constant constructors of the variant. *)
let has_known_constructor (constants, blocks) value =
  let repr = Obj.repr value in
  if Obj.is_block repr then Obj.tag repr < blocks
  else (Obj.obj repr : int) < constants
