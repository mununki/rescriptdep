(* Simplified AST types for ReScript compiler *)

type constant =
  | Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive
type direction_flag = Upto | Downto
type private_flag = Private | Public
type mutable_flag = Immutable | Mutable
type virtual_flag = Virtual | Concrete
type override_flag = Override | Fresh
type closed_flag = Closed | Open
type label = string

(* Simplified loc type without Location dependency *)
type 'a loc = { txt : 'a; loc : unit (* Instead of Location.t *) }
type variance = Covariant | Contravariant | Invariant
