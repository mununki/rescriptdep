open Stdlib
open Printf

(* Define stdlib_modules list - taken from parser.ml *)
let stdlib_modules =
  [
    (* ReScript standard library modules *)
    "Js";
    "Belt";
    "Belt_Int";
    "Belt_internals";
    "Array";
    "List";
    "String";
    "Bytes";
    "Printexc";
    "Printf";
    "Stdlib";
    "Pervasives";
    "PervasivesU";
    (* ReScript compiler internal modules *)
    "Caml1999I022";
    "Has_arity1";
    "Has_arity2";
    "DA";
    "GitHub";
    "Users";
    "TH";
    "BZ";
    "BZf";
    "Bgk";
    "Bgq";
    (* Additional internal modules *)
    "CamlinternalLazy";
    "CamlinternalFormat";
    "CamlinternalOO";
    "Js_array";
    "Js_string";
    "Js_dict";
    "Js_json";
    "Js_math";
    "Js_date";
    "Js_global";
    "Js_obj";
    "Js_typed_array";
    "Js_promise";
    "Js_null";
    "Js_undefined";
    "Js_null_undefined";
    (* Additional standard modules *)
    "Int64";
    "Obj";
    "React";
    "ReactEvent";
    "Dom";
    "Fetch";
    "ReactNavigation";
    "WebView";
    "ReactNative";
  ]

(* Function to check if a module is stdlib or internal - taken from parser.ml & extended *)
let is_stdlib_or_internal_module name =
  List.exists
    (fun m ->
      String.lowercase_ascii name = String.lowercase_ascii m
      || String.starts_with ~prefix:"Caml" name
      || String.starts_with ~prefix:"Js_" name
      || String.starts_with ~prefix:"Belt_" name)
    stdlib_modules
  || String.starts_with ~prefix:"Core__"
       name (* Exclude Core__ prefix modules *)
  || String.starts_with ~prefix:"Jsx" name

(* Exclude Jsx related modules *)
(* Exclude *_Query_* modules *)

(* Test execution function *)
let test_cmt_imports () =
  (* Get the directory of the test executable *)
  let executable_dir = Filename.dirname Sys.executable_name in

  (* Construct absolute path to the fixture file *)
  let fix_path filename =
    if Sys.file_exists filename then filename
    else if Sys.file_exists (Filename.concat executable_dir filename) then
      Filename.concat executable_dir filename
    else if Sys.file_exists (Filename.concat "test" filename) then
      Filename.concat "test" filename
    else filename
  in

  let cmt_file = fix_path "fixtures/math.cmt" in

  (* Print the path we're trying to use *)
  printf "Trying to use path: %s\n" cmt_file;

  printf "=== Testing cmt_imports for math.cmt file ===\n";

  try
    (* Read cmt file *)
    let cmt_info = Cmt_format.read_cmt cmt_file in

    (* Get cmt_imports list *)
    let imports = cmt_info.Cmt_format.cmt_imports in

    (* Filter imports excluding std modules *)
    let filtered_imports =
      List.filter
        (fun (module_name, _) -> not (is_stdlib_or_internal_module module_name))
        imports
    in

    (* Output results *)
    printf "cmt_imports found in math.cmt file excluding stdlib modules:\n";
    List.iter
      (fun (module_name, digest_opt) ->
        match digest_opt with
        | Some digest ->
            let digest_str = Digest.to_hex digest in
            printf "  - %s (digest: %s)\n" module_name digest_str
        | None -> printf "  - %s (digest: none)\n" module_name)
      filtered_imports;

    printf "Total of %d import modules, %d non-stdlib modules found.\n"
      (List.length imports)
      (List.length filtered_imports);
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

(* Main execution code *)
let () =
  let success = test_cmt_imports () in
  exit (if success then 0 else 1)
