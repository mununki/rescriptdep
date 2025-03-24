type t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
  loc_ghost : bool;
}

let none =
  { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; loc_ghost = true }
