(* Path representation for the ReScript compiler *)

type t = Pident of string | Pdot of t * string * int | Papply of t * t

val same : t -> t -> bool
val compare : t -> t -> int
val name : t -> string
val binding_time : t -> int
val isfree : string -> t -> bool
val head : t -> string
