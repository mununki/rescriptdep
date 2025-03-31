open Stdlib
open Printf

(* Function to read a 4-byte integer in big-endian format *)
let read_int32_be ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4

(* Function to read a line from binary file *)
let read_line_bin ic =
  let buf = Buffer.create 128 in
  let rec read_chars () =
    let c = input_char ic in
    if c = '\n' then Buffer.contents buf
    else (
      Buffer.add_char buf c;
      read_chars ())
  in
  try read_chars () with End_of_file -> Buffer.contents buf

(* Module to collect unique module references *)
module StringSet = Set.Make (String)

(* Parse module references from AST file *)
let parse_module_refs filename =
  let ic = open_in_bin filename in
  let modules = ref StringSet.empty in

  try
    (* Read the number of modules (first 4 bytes) *)
    let modules_count = read_int32_be ic in
    printf "Found %d module references in AST file\n" modules_count;

    (* Skip the newline after the count *)
    let _ = input_char ic in

    (* Read module references line by line until we hit something that looks like a file path *)
    let module_names = ref [] in
    let found_source_path = ref false in
    let source_path = ref "" in

    (* Read specified number of lines *)
    for _ = 1 to modules_count do
      let line = read_line_bin ic in

      (* Check if this looks like a file path *)
      if
        (not !found_source_path)
        && String.length line > 0
        && (line.[0] = '/'
           || (String.length line > 1 && line.[0] = 'C' && line.[1] = ':'))
      then (
        found_source_path := true;
        source_path := line
        (* If it's not a file path and is a valid module name (starts with uppercase) *))
      else if
        (not !found_source_path)
        && String.length line > 0
        && line.[0] >= 'A'
        && line.[0] <= 'Z'
      then module_names := line :: !module_names
    done;

    (if !found_source_path then printf "Source file: %s\n" !source_path
     else
       (* If we didn't find the source path in the expected module count range,
         try to read one more line in case it's the source path *)
       try
         let line = read_line_bin ic in
         if
           String.length line > 0
           && (line.[0] = '/'
              || (String.length line > 1 && line.[0] = 'C' && line.[1] = ':'))
         then printf "Source file: %s\n" line
       with End_of_file -> printf "Note: Could not find source file path\n");

    (* Add individual module names to the set *)
    List.iter
      (fun name -> modules := StringSet.add name !modules)
      (List.rev !module_names);

    close_in ic;
    !modules
  with e ->
    close_in_noerr ic;
    printf "Error parsing AST file: %s\n" (Printexc.to_string e);
    StringSet.empty

(* Main function *)
let () =
  let ast_path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "rescript/lib/bs/src/app.ast"
  in

  printf "Analyzing AST file: %s\n" ast_path;

  if not (Sys.file_exists ast_path) then (
    printf "Error: AST file not found at %s\n" ast_path;
    exit 1);

  let modules = parse_module_refs ast_path in

  printf "\nModule references:\n";
  if StringSet.is_empty modules then printf "  (No modules found)\n"
  else StringSet.iter (printf "  - %s\n") modules;

  exit 0
