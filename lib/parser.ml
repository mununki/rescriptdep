open Stdlib

(* Module info representation *)
type module_info = {
  name : string;
  dependencies : string list;
  interface_digest : Digest.t option;
  implementation_digest : Digest.t option;
  file_path : string option;
}

(* Exceptions *)
exception Invalid_cmt_file of string

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
    ]
  in
  List.exists
    (fun m ->
      String.lowercase_ascii name = String.lowercase_ascii m
      || String.starts_with ~prefix:"Caml" name
      || String.starts_with ~prefix:"Js_" name
      || String.starts_with ~prefix:"Belt_" name)
    stdlib_modules

(* Normalize module name to ensure consistent casing *)
let normalize_module_name name =
  (* Ensure first letter is uppercase and rest is preserved *)
  if String.length name > 0 then
    String.make 1 (Char.uppercase_ascii name.[0])
    ^ String.sub name 1 (String.length name - 1)
  else name

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

(* Recursively scan directories for .cmt files *)
let rec scan_directory_recursive dir =
  try
    let files = Sys.readdir dir in
    let cmt_files = ref [] in

    Array.iter
      (fun file ->
        let path = Filename.concat dir file in
        if Sys.is_directory path then
          (* Recursively scan subdirectory *)
          cmt_files := List.append (scan_directory_recursive path) !cmt_files
        else if Filename.check_suffix file ".cmt" then
          (* Add cmt file *)
          cmt_files := path :: !cmt_files)
      files;

    !cmt_files
  with Sys_error _ ->
    Printf.printf "Warning: Could not read directory %s\n" dir;
    []

(* Use recursive directory scanning *)
let scan_directory = scan_directory_recursive

(* Extract dependencies using the cmt_info structure *)
module DependencyExtractor = struct
  (* Extract module names from paths in the typedtree *)
  let rec extract_module_from_path path =
    match path with
    | Path.Pident id ->
        (* In our simplified Path module, Pident only contains a string *)
        id
    | Path.Pdot (p, s, _) ->
        (* For Pdot, extract just the name part *)
        s
    | Path.Papply (p1, p2) ->
        (* For path applications, we'll use the first path *)
        extract_module_from_path p1

  (* Read source file and check if a module is actually used in the code *)
  let is_module_used_in_source source_file module_name =
    if not (Sys.file_exists source_file) then true
      (* If source file doesn't exist, conservatively return true *)
    else
      try
        let ic = open_in source_file in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;

        (* Various patterns to check if the module is actually used in the code *)
        let check_pattern pattern =
          try
            let regexp = Str.regexp_case_fold pattern in
            Str.search_forward regexp content 0 >= 0
          with Not_found -> false
        in

        (* 1. Module open: open Module *)
        let open_pattern = "open[ \t]+" ^ module_name ^ "\\b" in
        (* 2. Module usage: Module.something *)
        let use_pattern = module_name ^ "\\." in
        (* 3. Module type: module type of Module *)
        let module_type_pattern =
          "module[ \t]+type[ \t]+of[ \t]+" ^ module_name ^ "\\b"
        in
        (* 4. Module include: include Module *)
        let include_pattern = "include[ \t]+" ^ module_name ^ "\\b" in
        (* 5. Module definition: module X = Module *)
        let assign_pattern =
          "module[ \t]+[A-Za-z0-9_]+[ \t]*=[ \t]*" ^ module_name ^ "\\b"
        in
        (* 6. Module as function argument: function(Module) *)
        let param_pattern = "[(,][ \t]*" ^ module_name ^ "[ \t]*[,)]" in

        (* Check if the module is used with regex pattern matching *)
        let is_used =
          check_pattern open_pattern || check_pattern use_pattern
          || check_pattern module_type_pattern
          || check_pattern include_pattern
          || check_pattern assign_pattern
          || check_pattern param_pattern
        in

        if is_used then
          Printf.printf "Module %s is actually used in %s\n" module_name
            source_file
        else
          Printf.printf "Module %s is not used in source %s\n" module_name
            source_file;

        is_used
      with _ ->
        Printf.printf "Error reading source file %s\n" source_file;
        true (* In case of file reading failure, conservatively return true *)

  (* Extract dependencies from cmt_info structure *)
  let extract_dependencies_from_cmt_info cmt_info =
    let deps = ref [] in

    (* Check source file path *)
    let source_file_opt = cmt_info.Cmt_format.cmt_sourcefile in
    let source_file =
      match source_file_opt with
      | Some path -> (
          (* Try to convert .cmt file path to original source file (.res, .re, .ml) *)
          let base_path = Filename.remove_extension path in
          let dir_path = Filename.dirname path in
          let base_name = Filename.basename base_path in

          (* Configure possible directories to look for source files *)
          let possible_dirs =
            if
              String.length path > 11
              && String.sub path (String.length path - 11) 11 = "/lib/bs/src/"
              || Str.string_match (Str.regexp ".*/lib/bs/src/.*") path 0
            then
              (* For ReScript projects, check src directory instead of lib/bs/src *)
              let parts = Str.split (Str.regexp "/lib/bs/src/") path in
              match parts with
              | [ prefix; suffix ] ->
                  [
                    dir_path;
                    Filename.concat prefix "src";
                    Filename.concat (Filename.dirname dir_path) "src";
                  ]
              | _ -> [ dir_path ]
            else [ dir_path; Filename.concat (Filename.dirname dir_path) "src" ]
          in

          (* Extension list *)
          let extensions = [ ".res" ] in

          (* Generate all possible file paths from directory and extension combinations *)
          let candidates =
            List.fold_left
              (fun acc dir ->
                List.fold_left
                  (fun acc' ext ->
                    Filename.concat dir (base_name ^ ext) :: acc')
                  acc extensions)
              [] possible_dirs
          in

          (* Return the first existing file or use the original path *)
          try List.find Sys.file_exists candidates
          with Not_found ->
            Printf.printf "Source file not found for %s, using cmt path\n" path;
            path)
      | None ->
          Printf.printf "No source file info in cmt for %s\n"
            cmt_info.Cmt_format.cmt_modname;
          "" (* No source file information available *)
    in

    (* Extract module names directly from imports list *)
    (try
       List.iter
         (fun (module_name, _) ->
           if
             is_valid_module_name module_name
             && (not (is_stdlib_or_internal_module module_name))
             && (source_file = ""
                || is_module_used_in_source source_file module_name)
           then deps := module_name :: !deps)
         cmt_info.Cmt_format.cmt_imports
     with _ -> ());

    (* Return unique dependencies *)
    List.sort_uniq String.compare !deps
end

(* Find the implementation file using various strategies *)
let find_implementation_file cmt_path =
  (* Try to find .re or .res file by replacing .cmt extension *)
  let base_path = Filename.remove_extension cmt_path in
  let dir_path = Filename.dirname cmt_path in
  let base_name = Filename.basename base_path in
  let module_name = normalize_module_name base_name in

  (* Analyze possible search patterns *)
  (* Common patterns in ReScript projects:
     - lib/bs/src/X.cmt → src/X.res
     - X.cmt → ./X.res 
     - X.cmt → ../src/X.res
     - X.cmt → ../../src/X.res *)

  (* Search for source directories by finding project root directory *)
  let rec find_src_dir current_dir depth =
    if depth > 5 then [] (* Limit depth to prevent excessive searching *)
    else
      let src_dir = Filename.concat current_dir "src" in
      if Sys.file_exists src_dir && Sys.is_directory src_dir then [ src_dir ]
      else
        let parent = Filename.dirname current_dir in
        if parent = current_dir then [] (* When reached the root directory *)
        else find_src_dir parent (depth + 1)
  in

  (* Track related src directories by finding bs directory *)
  let is_bs_path path =
    let parts =
      String.split_on_char '/'
        (String.concat "/" (String.split_on_char '\\' path))
    in
    List.exists
      (fun part -> part = "bs" || part = "lib" || part = "lib/bs")
      parts
  in

  (* Try to move to '/../../../src' if 'lib/bs/src' pattern exists *)
  let src_dirs_from_bs =
    if is_bs_path cmt_path then
      (* Find the src sibling directory two levels up for the lib/bs/src -> ~/src pattern *)
      let bs_parent = Filename.dirname (Filename.dirname dir_path) in
      let project_root = Filename.dirname bs_parent in
      let src_dir = Filename.concat project_root "src" in
      if Sys.file_exists src_dir && Sys.is_directory src_dir then [ src_dir ]
      else []
    else []
  in

  (* Find src directory from current directory *)
  let current_src_dirs = find_src_dir dir_path 0 in

  (* Basic search directory list *)
  let search_dirs =
    [ dir_path; Filename.dirname dir_path ]
    @ current_src_dirs @ src_dirs_from_bs
  in

  (* Remove duplicate search directories *)
  let unique_dirs =
    let rec remove_dups seen = function
      | [] -> []
      | dir :: rest ->
          if List.mem dir seen then remove_dups seen rest
          else dir :: remove_dups (dir :: seen) rest
    in
    remove_dups [] search_dirs
  in

  Printf.printf "Search directory list:\n";
  Printf.printf "Search directory list:\n";
  List.iter (fun dir -> Printf.printf "  - %s\n" dir) unique_dirs;

  (* Create target file list from each directory *)
  let build_candidates dirs base exts =
    List.fold_left
      (fun acc dir ->
        let dir_candidates =
          List.map (fun ext -> Filename.concat dir (base ^ ext)) exts
        in
        acc @ dir_candidates)
      [] dirs
  in

  (* Try multiple filename patterns *)
  let name_patterns =
    [ base_name; String.lowercase_ascii base_name; module_name ]
  in

  let extensions = [ ".res"; ".re"; ".ml" ] in

  (* Generate candidate files with various name patterns *)
  (* Generate candidate files with various name patterns *)
  let candidates =
    List.fold_left
      (fun acc pattern -> acc @ build_candidates unique_dirs pattern extensions)
      [] name_patterns
  in

  Printf.printf "Module: %s, number of search paths: %d\n" module_name
    (List.length candidates);
  Printf.printf "Module: %s, number of search paths: %d\n" module_name
    (List.length candidates);

  (* Return the first existing file from candidates *)
  try
    let found = List.find Sys.file_exists candidates in
    Printf.printf "Implementation file found: %s\n" found;
    Printf.printf "Implementation file found: %s\n" found;
    Some found
  with Not_found ->
    Printf.printf "Implementation file not found\n";
    Printf.printf "Implementation file not found\n";
    None

(* Parse a cmt file and extract module information *)
let parse_cmt_file path =
  let module_name =
    Filename.basename path |> Filename.remove_extension |> normalize_module_name
  in

  Printf.printf "Analyzing module: %s (file: %s)\n" module_name path;
  Printf.printf "Analyzing module: %s (file: %s)\n" module_name path;

  try
    (* Use Cmt_format to read the file - with relaxed format checking *)
    let cmt_info =
      try Cmt_format.read_cmt path
      with Cmt_format.Error msg ->
        (* Provide more detailed error information *)
        Printf.printf
          "Warning: Problem with CMT file %s: %s (continuing anyway)\n" path msg;

        (* Create basic information even if there's a problem to continue processing *)
        {
          Cmt_format.cmt_modname = module_name;
          cmt_annots = Implementation (Obj.magic ());
          cmt_value_dependencies = [];
          cmt_comments = [];
          cmt_args = [||];
          cmt_sourcefile = Some path;
          cmt_builddir = "";
          cmt_loadpath = [];
          cmt_source_digest = None;
          cmt_initial_env = Env.empty;
          cmt_imports = [];
          cmt_interface_digest = None;
          cmt_use_summaries = false;
        }
    in

    (* Extract dependencies using only the parsed cmt_info *)
    let dependencies =
      DependencyExtractor.extract_dependencies_from_cmt_info cmt_info
    in

    (* Filter out self-references and normalize *)
    let filtered_deps =
      dependencies
      |> List.filter (fun name ->
             normalize_module_name name <> module_name
             && not (is_stdlib_or_internal_module name))
      |> List.map normalize_module_name
      |> List.sort_uniq compare
    in

    (* Create the module info *)
    let impl_file = find_implementation_file path in

    (* Set file path to implementation file, or .cmt file if not found *)
    let file_path =
      match impl_file with Some file -> Some file | None -> Some path
    in

    {
      name = module_name;
      dependencies = filtered_deps;
      interface_digest = cmt_info.cmt_interface_digest;
      implementation_digest = None;
      file_path;
    }
  with
  | Invalid_cmt_file _ as e -> raise e
  | Sys_error msg ->
      raise
        (Invalid_cmt_file (Printf.sprintf "System error with %s: %s" path msg))
  | e ->
      raise
        (Invalid_cmt_file
           (Printf.sprintf "Error parsing %s: %s" path (Printexc.to_string e)))

(* Get the list of all project modules using recursive scanning *)
let get_project_modules paths =
  let project_modules = ref [] in
  let visited_dirs = Hashtbl.create 50 in

  (* Recursively scan a directory and add all module names to project_modules *)
  let rec scan_dir_for_modules dir =
    if not (Hashtbl.mem visited_dirs dir) then (
      Hashtbl.add visited_dirs dir true;
      try
        let files = Sys.readdir dir in
        Array.iter
          (fun file ->
            let path = Filename.concat dir file in
            if Sys.is_directory path then scan_dir_for_modules path
            else if Filename.check_suffix file ".cmt" then
              let name =
                Filename.basename file |> Filename.remove_extension
                |> normalize_module_name
              in
              if not (List.mem name !project_modules) then
                project_modules := name :: !project_modules)
          files
      with Sys_error _ -> ())
  in

  (* Process each path recursively *)
  List.iter
    (fun path ->
      if Sys.is_directory path then scan_dir_for_modules path
      else if Filename.check_suffix path ".cmt" then
        let name =
          Filename.basename path |> Filename.remove_extension
          |> normalize_module_name
        in
        if not (List.mem name !project_modules) then
          project_modules := name :: !project_modules)
    paths;

  !project_modules

(* Parse a list of files or directories *)
let parse_files_or_dirs paths =
  (* First get the list of all project modules (recursively) *)
  let project_modules = get_project_modules paths in
  Printf.printf "Project modules: %s\n" (String.concat ", " project_modules);

  let rec process accum = function
    | [] -> accum
    | path :: rest ->
        if Sys.is_directory path then
          let cmt_files = scan_directory path in
          process accum (cmt_files @ rest)
        else if Filename.check_suffix path ".cmt" then (
          try
            let module_info = parse_cmt_file path in
            (* Filter dependencies to only include project modules *)
            let filtered_deps =
              List.filter
                (fun dep -> List.mem dep project_modules)
                module_info.dependencies
            in
            let updated_info =
              { module_info with dependencies = filtered_deps }
            in
            process (updated_info :: accum) rest
          with Invalid_cmt_file msg ->
            Printf.eprintf "Warning: %s\n" msg;
            process accum rest)
        else process accum rest
  in
  process [] paths
