(* vendor/ident.ml *)
type t = { stamp : int; name : string; mutable flags : int }

let create s = { stamp = 0; name = s; flags = 0 }
let name i = i.name
let stamp i = i.stamp
let same i1 i2 = i1.stamp = i2.stamp

(* Add the compare function *)
let compare i1 i2 =
  let c = String.compare i1.name i2.name in
  if c <> 0 then c else compare i1.stamp i2.stamp

let binding_time id = id.stamp
