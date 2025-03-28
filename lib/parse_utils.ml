(* Utility functions for path resolution *)

open Stdlib

(* Normalize module name to ensure consistent casing *)
let normalize_module_name name =
  (* Ensure first letter is uppercase and rest is preserved *)
  if String.length name > 0 then
    String.make 1 (Char.uppercase_ascii name.[0])
    ^ String.sub name 1 (String.length name - 1)
  else name

(* Find the implementation file for a module by its name *)
let find_implementation_file_by_name ?(verbose = false) module_name
    src_directories =
  let name_patterns =
    [
      module_name;
      String.lowercase_ascii module_name;
      normalize_module_name module_name;
    ]
  in

  let extensions = [ ".res"; ".re"; ".ml" ] in

  (* Build all possible file paths *)
  let candidates =
    List.fold_left
      (fun acc dir ->
        List.fold_left
          (fun acc' pattern ->
            List.fold_left
              (fun acc'' ext -> Filename.concat dir (pattern ^ ext) :: acc'')
              acc' extensions)
          acc name_patterns)
      [] src_directories
  in

  if verbose then
    Printf.printf "Searching for module %s in %d paths\n" module_name
      (List.length candidates);

  (* Return the first existing file *)
  try
    let found = List.find Sys.file_exists candidates in
    if verbose then
      Printf.printf "Found implementation for %s: %s\n" module_name found;
    Some found
  with Not_found ->
    if verbose then
      Printf.printf "No implementation file found for %s\n" module_name;
    None

(* Check if a module is part of the standard library or internal modules *)
let is_stdlib_or_internal_module name =
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
      (* Common OCaml modules *)
      "Arg";
      "ArrayLabels";
      "Buffer";
      "BytesLabels";
      "Callback";
      "Char";
      "Complex";
      "Digest";
      "Dom";
      "Dom_storage";
      "Dom_storage2";
      "Filename";
      "Format";
      "Genlex";
      "Hashtbl";
      "HashtblLabels";
      "Int32";
      "Int64";
      "Lazy";
      "Lexing";
      "ListLabels";
      "Map";
      "MapLabels";
      "MoreLabels";
      "Obj";
      "Parsing";
      "Queue";
      "Random";
      "Set";
      "SetLabels";
      "Sort";
      "Stack";
      "StdLabels";
      "Stream";
      "StringLabels";
      "Sys";
      "Uchar";
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
      (* JSX related modules *)
      "JsxC";
      "JsxDOMC";
      "JsxDOMStyle";
      "JsxDOMU";
      "JsxEventC";
      "JsxEventU";
      "JsxPPXReactSupportC";
      "JsxPPXReactSupportU";
      "JsxU";
      (* Additional internal modules *)
      "CamlinternalLazy";
      "CamlinternalFormat";
      "CamlinternalOO";
      "CamlinternalMod";
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
    ]
  in

  (* Check by exact name or common prefixes *)
  List.exists
    (fun m -> String.lowercase_ascii name = String.lowercase_ascii m)
    stdlib_modules
  || String.starts_with ~prefix:"Caml" name
  || String.starts_with ~prefix:"Js_" name
  || String.starts_with ~prefix:"Belt_" name
  || String.starts_with ~prefix:"Jsx" name

(* Check if a string is a valid module name *)
let is_valid_module_name str =
  (* Basic validation for a module name: starts with capital letter, only contains valid chars *)
  String.length str > 0
  && str.[0] >= 'A'
  && str.[0] <= 'Z'
  &&
  (* Check that string only contains valid OCaml identifier chars *)
  try
    String.iter
      (fun c ->
        if
          not
            ((c >= 'A' && c <= 'Z')
            || (c >= 'a' && c <= 'z')
            || (c >= '0' && c <= '9')
            || c = '_' || c = '\'')
        then raise Exit)
      str;
    true
  with Exit -> false
