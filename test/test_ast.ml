open Stdlib
open Printf

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
    let modules_count = try input_binary_int ic with _ -> 0 in
    printf "Found %d module references in AST file\n" modules_count;

    (* Skip the newline after the count *)
    let _ = try input_char ic with _ -> '\000' in

    (* Read all module references efficiently *)
    let module_names = ref [] in

    let rec read_modules remaining max_iterations =
      if remaining <= 0 || max_iterations <= 0 then !module_names
      else
        try
          let buf = Buffer.create 128 in
          let rec read_line max_chars =
            if max_chars <= 0 then
              (* Line too long, skip the rest of this line *)
              try
                while input_char ic <> '\n' do
                  ()
                done;
                read_modules (remaining - 1) (max_iterations - 1)
              with End_of_file -> !module_names
            else
              let c = input_char ic in
              if c = '\n' then (
                let line = Buffer.contents buf in
                (* Filter out file paths - only add module names *)
                if
                  String.length line > 0
                  && line.[0] <> '/'
                  && (String.length line <= 1
                     || not (line.[0] = 'C' && line.[1] = ':'))
                  && line.[0] >= 'A'
                  && line.[0] <= 'Z'
                then module_names := line :: !module_names;
                read_modules (remaining - 1) (max_iterations - 1))
              else (
                Buffer.add_char buf c;
                read_line (max_chars - 1))
          in
          read_line 10000 (* Limit line length to 10,000 chars *)
        with End_of_file -> !module_names
    in

    let result = read_modules modules_count 200000 in
    (* Limit iterations to prevent infinite loops *)

    (* Try to find source file path after reading modules *)
    let source_path = ref "" in
    let found_source_path = ref false in

    (try
       let line = read_line_bin ic in
       if
         String.length line > 0
         && (line.[0] = '/'
            || (String.length line > 1 && line.[0] = 'C' && line.[1] = ':'))
       then (
         found_source_path := true;
         source_path := line;
         printf "Source file: %s\n" !source_path)
     with End_of_file ->
       if not !found_source_path then
         printf "Note: Could not find source file path\n");

    (* Add individual module names to the set *)
    List.iter (fun name -> modules := StringSet.add name !modules) result;

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
