open Stdlib
open Printf

(** * Module to test cmt_value_dependencies from cmt files. *
    cmt_value_dependencies contains value dependency information tracked by the
    OCaml type compiler. * This shows how functions or values depend on other
    functions or values. *)

(* Function to test value_dependencies of a specific cmt file *)
let test_cmt_file_values cmt_file =
  printf "=== Testing cmt_value_dependencies for %s file ===\n"
    (Filename.basename cmt_file);

  try
    (* Read cmt file to extract information *)
    let cmt_info = Cmt_format.read_cmt cmt_file in

    (* Get cmt_value_dependencies list
       This list consists of (source value, target value) pairs for each dependency *)
    let value_deps = cmt_info.Cmt_format.cmt_value_dependencies in

    (* Get module name *)
    let module_name = cmt_info.Cmt_format.cmt_modname in

    (* Output results *)
    printf "cmt_value_dependencies found in module %s (file %s):\n" module_name
      (Filename.basename cmt_file);

    if List.length value_deps = 0 then printf "  No value dependencies found.\n"
    else
      List.iter
        (fun (val_desc1, val_desc2) ->
          printf "  - Value dependency found:\n";

          (* Print value type information *)
          printf "    [Source Value]\n";
          printf "    Type: %s\n"
            (match val_desc1.Types.val_kind with
            | Types.Val_reg -> "Regular value" (* Regular function or value *)
            | Types.Val_prim s ->
                "Primitive: " ^ s (* Built-in function/operation *)
            | Types.Val_ivar (mflag, s) ->
                let mutability =
                  match mflag with
                  | Types.Immutable -> "Immutable"
                  | Types.Mutable -> "Mutable"
                in
                Printf.sprintf "Instance variable(%s): %s" mutability s
            | Types.Val_self _ -> "Self reference"
            | Types.Val_anc _ -> "Ancestor reference"
            | Types.Val_unbound -> "Unbound value");

          printf "    [Target Value]\n";
          printf "    Type: %s\n"
            (match val_desc2.Types.val_kind with
            | Types.Val_reg -> "Regular value"
            | Types.Val_prim s -> "Primitive: " ^ s
            | Types.Val_ivar (mflag, s) ->
                let mutability =
                  match mflag with
                  | Types.Immutable -> "Immutable"
                  | Types.Mutable -> "Mutable"
                in
                Printf.sprintf "Instance variable(%s): %s" mutability s
            | Types.Val_self _ -> "Self reference"
            | Types.Val_anc _ -> "Ancestor reference"
            | Types.Val_unbound -> "Unbound value");

          printf "\n")
        value_deps;

    printf "Total of %d value dependencies found.\n\n" (List.length value_deps);

    true
  with
  | Cmt_format.Error msg ->
      printf "Error: %s\n" msg;
      false
  | Sys_error msg ->
      printf "System error: %s\n" msg;
      false
  | e ->
      printf "Unexpected error: %s\n" (Printexc.to_string e);
      false

(* Main function to test multiple cmt files *)
let test_cmt_values () =
  (* Get the directory of the test executable *)
  let executable_dir = Filename.dirname Sys.executable_name in

  (* Construct absolute paths to the fixture files *)
  let fix_path filename =
    if Sys.file_exists filename then filename
    else if Sys.file_exists (Filename.concat executable_dir filename) then
      Filename.concat executable_dir filename
    else if Sys.file_exists (Filename.concat "test" filename) then
      Filename.concat "test" filename
    else filename
  in

  let files = List.map fix_path [ "fixtures/math.cmt"; "fixtures/app.cmt" ] in

  (* Print the paths we're trying to use *)
  List.iter (fun path -> Printf.printf "Trying to use path: %s\n" path) files;

  let results = List.map test_cmt_file_values files in
  List.for_all (fun x -> x) results

(* Main execution code *)
let () =
  let success = test_cmt_values () in
  exit (if success then 0 else 1)
