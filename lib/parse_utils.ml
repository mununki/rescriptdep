(* Utility functions for path resolution *)

open Stdlib
open Str (* For regex functions *)
open Yojson.Basic.Util (* For JSON parsing functions *)

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

(* Find external module path in node_modules directory *)
let find_external_module_path ?(verbose = false) module_name project_root =
  if verbose then Printf.printf "Looking for external module: %s\n" module_name;

  (* Verify if path exists and is a directory - no symlink resolution *)
  let verify_path path = Sys.file_exists path && Sys.is_directory path in

  (* Read dependency packages from bsconfig.json or rescript.json using Yojson *)
  let read_bs_dependencies config_path =
    if verbose then Printf.printf "Reading dependencies from: %s\n" config_path;
    if Sys.file_exists config_path then (
      try
        (* Use Yojson to parse the JSON file *)
        let json = Yojson.Basic.from_file config_path in
        if verbose then
          Printf.printf "Successfully parsed JSON from %s\n" config_path;

        (* Helper function to safely extract string array from JSON *)
        let get_string_array json_obj field_name =
          try
            match Yojson.Basic.Util.member field_name json_obj with
            | `List items -> Yojson.Basic.Util.filter_string items
            | _ -> []
          with _ -> []
        in

        (* Extract dependencies from different fields *)
        let bs_deps = get_string_array json "bs-dependencies" in
        let bs_dev_deps = get_string_array json "bs-dev-dependencies" in
        let pinned_deps = get_string_array json "pinned-dependencies" in

        (* Combine all dependencies *)
        let all_deps = List.concat [ bs_deps; bs_dev_deps; pinned_deps ] in

        if verbose then
          Printf.printf "Found dependencies: %s\n" (String.concat ", " all_deps);

        all_deps
      with e ->
        if verbose then
          Printf.printf "Error parsing %s: %s\n" config_path
            (Printexc.to_string e);
        [])
    else []
  in

  (* Find nearest bsconfig.json or rescript.json *)
  let rec find_config_file dir =
    let bsconfig = Filename.concat dir "bsconfig.json" in
    let rescript = Filename.concat dir "rescript.json" in

    if Sys.file_exists bsconfig then (
      if verbose then Printf.printf "Found bsconfig.json at %s\n" bsconfig;
      Some (bsconfig, dir))
    else if Sys.file_exists rescript then (
      if verbose then Printf.printf "Found rescript.json at %s\n" rescript;
      Some (rescript, dir))
    else
      let parent = Filename.dirname dir in
      if parent = dir then None (* Reached root directory *)
      else find_config_file parent
  in

  (* Recursively search up the directory tree for node_modules *)
  let rec find_node_modules dir =
    let node_modules_path = Filename.concat dir "node_modules" in
    if Sys.file_exists node_modules_path && Sys.is_directory node_modules_path
    then Some node_modules_path
    else
      let parent = Filename.dirname dir in
      if parent = dir then None (* Reached root directory *)
      else find_node_modules parent
  in

  (* Find module in specific package *)
  let find_module_in_package package_path module_name =
    if verbose then
      Printf.printf "Looking for module %s in package: %s\n" module_name
        package_path;

    (* Look for common source directories in the package *)
    let possible_src_dirs =
      [
        Filename.concat package_path "src";
        Filename.concat package_path "dist";
        package_path;
      ]
    in

    (* For each potential source directory, look for module files *)
    let rec check_dirs = function
      | [] -> None
      | dir :: rest ->
          if verbose then Printf.printf "  Checking directory: %s\n" dir;
          if Sys.file_exists dir && Sys.is_directory dir then (
            try
              (* Look for files defining the module *)
              let files = Sys.readdir dir |> Array.to_list in

              (* Helper to check if file matches module pattern *)
              let matches_module file =
                let base =
                  Filename.basename file |> Filename.remove_extension
                in
                let exts = [ ".res"; ".resi" ] in

                (* Check if the base name matches our module *)
                (String.lowercase_ascii base
                 = String.lowercase_ascii module_name
                || normalize_module_name base = module_name)
                &&
                (* Check if extension is valid ReScript/OCaml extension *)
                List.exists (fun ext -> Filename.extension file = ext) exts
              in

              let matching_files = List.filter matches_module files in

              if verbose && matching_files <> [] then
                Printf.printf "  Found matching files: %s\n"
                  (String.concat ", " matching_files);

              match matching_files with
              | [] -> (
                  (* Check for matching subdirectory *)
                  let subdirs =
                    List.filter
                      (fun f ->
                        let path = Filename.concat dir f in
                        Sys.is_directory path
                        && (String.lowercase_ascii f
                            = String.lowercase_ascii module_name
                           || normalize_module_name f = module_name))
                      files
                  in

                  if verbose && subdirs <> [] then
                    Printf.printf "  Found matching subdirectories: %s\n"
                      (String.concat ", " subdirs);

                  match subdirs with
                  | [] -> check_dirs rest
                  | subdir :: _ -> Some (Filename.concat dir subdir))
              | _ :: _ -> Some dir (* Found files defining this module *)
            with e ->
              if verbose then
                Printf.printf "  Error checking directory %s: %s\n" dir
                  (Printexc.to_string e);
              check_dirs rest)
          else check_dirs rest
    in

    check_dirs possible_src_dirs
  in

  (* Main function logic *)
  match find_config_file project_root with
  | None ->
      if verbose then Printf.printf "No bsconfig.json or rescript.json found\n";
      None
  | Some (config_path, config_dir) -> (
      (* Only use dependencies declared in bsconfig.json/rescript.json *)
      let dependencies = read_bs_dependencies config_path in

      if verbose then
        Printf.printf "Dependencies from config: %s\n"
          (String.concat ", " dependencies);

      (* Find the node_modules directory *)
      match find_node_modules config_dir with
      | None ->
          if verbose then Printf.printf "No node_modules directory found\n";
          None
      | Some node_modules ->
          if verbose then
            Printf.printf "Found node_modules at %s\n" node_modules;

          (* Try each dependency to find the module *)
          let rec try_dependencies = function
            | [] ->
                if verbose then
                  Printf.printf "Module %s not found in any dependency\n"
                    module_name;
                None
            | dep :: rest ->
                let package_path = Filename.concat node_modules dep in

                if verify_path package_path then (
                  if verbose then Printf.printf "Checking package: %s\n" dep;
                  match find_module_in_package package_path module_name with
                  | Some dir ->
                      if verbose then
                        Printf.printf "Found module %s in package %s at %s\n"
                          module_name dep dir;
                      Some dir
                  | None ->
                      (* Check for scoped packages *)
                      if String.starts_with ~prefix:"@" dep then
                        let parts = String.split_on_char '/' dep in
                        match parts with
                        | [ scope; pkg ] ->
                            let scoped_path =
                              Filename.concat node_modules scope
                            in
                            let full_path = Filename.concat scoped_path pkg in

                            if verify_path full_path then (
                              if verbose then
                                Printf.printf "Checking scoped package: %s/%s\n"
                                  scope pkg;
                              match
                                find_module_in_package full_path module_name
                              with
                              | Some dir ->
                                  if verbose then
                                    Printf.printf
                                      "Found module %s in scoped package %s at \
                                       %s\n"
                                      module_name dep dir;
                                  Some dir
                              | None -> try_dependencies rest)
                            else try_dependencies rest
                        | _ -> try_dependencies rest
                      else try_dependencies rest)
                else try_dependencies rest
          in

          (* Just try with declared dependencies *)
          try_dependencies dependencies)
