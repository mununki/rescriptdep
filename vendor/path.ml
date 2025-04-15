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

(* Path representation for the ReScript compiler *)

type t = Pident of Ident.t | Pdot of t * string * int | Papply of t * t

let rec same p1 p2 =
  match (p1, p2) with
  | Pident id1, Pident id2 -> id1 = id2
  | Pdot (p1, s1, _), Pdot (p2, s2, _) -> s1 = s2 && same p1 p2
  | Papply (fun1, arg1), Papply (fun2, arg2) -> same fun1 fun2 && same arg1 arg2
  | _, _ -> false

let rec compare p1 p2 =
  match (p1, p2) with
  | Pident id1, Pident id2 ->
      if id1 < id2 then -1 else if id1 > id2 then 1 else 0
  | Pident _, _ -> -1
  | _, Pident _ -> 1
  | Pdot (p1, s1, _), Pdot (p2, s2, _) ->
      let c = String.compare s1 s2 in
      if c <> 0 then c else compare p1 p2
  | Pdot _, _ -> -1
  | _, Pdot _ -> 1
  | Papply (fun1, arg1), Papply (fun2, arg2) ->
      let c = compare fun1 fun2 in
      if c <> 0 then c else compare arg1 arg2

let kfalse _ = false

(* For printing *)
let rec name ?(paren = kfalse) = function
  | Pident id -> Ident.name id
  | Pdot (p, s, _pos) ->
      name ~paren p ^ if paren s then ".( " ^ s ^ " )" else "." ^ s
  | Papply (p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"

let rec binding_time = function
  | Pident _ -> 0
  | Pdot (p, _, _) -> binding_time p + 1
  | Papply (p, _) -> binding_time p

let rec isfree id = function
  | Pident id' -> id = id'
  | Pdot (p, _, _) -> isfree id p
  | Papply (p1, p2) -> isfree id p1 || isfree id p2

let rec head = function
  | Pident id -> id
  | Pdot (p, _, _) -> head p
  | Papply (p, _) -> head p
