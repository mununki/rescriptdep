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
