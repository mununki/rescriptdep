open Stdlib

(* Module info representation *)
type module_info = {
  name : string;
  dependencies : string list;
  interface_digest : Digest.t option;
  implementation_digest : Digest.t option;
}

(* Exceptions *)
exception Invalid_cmi_file of string
exception Unsupported_cmi_version of string

(* Magic numbers for cmi files *)
let cmi_magic_number = "Caml1999I"
let rescript_magic_number = "BS00MI"

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

(* Recursively scan directories for .cmi files *)
let rec scan_directory_recursive dir =
  try
    let files = Sys.readdir dir in
    let cmi_files = ref [] in

    Array.iter
      (fun file ->
        let path = Filename.concat dir file in
        if Sys.is_directory path then
          (* Recursively scan subdirectory *)
          cmi_files := List.append (scan_directory_recursive path) !cmi_files
        else if Filename.check_suffix file ".cmi" then
          (* Add cmi file *)
          cmi_files := path :: !cmi_files)
      files;

    !cmi_files
  with Sys_error _ ->
    Printf.printf "Warning: Could not read directory %s\n" dir;
    []

(* Use recursive directory scanning *)
let scan_directory = scan_directory_recursive

(* Parse a cmi file and extract module information *)
let parse_cmi_file project_modules path =
  let module_name =
    Filename.basename path |> Filename.remove_extension |> normalize_module_name
  in

  (* We'll extract dependencies through binary analysis *)
  let dependencies = ref [] in

  (* Check if a module name is valid and belongs to the project *)
  let is_valid_project_module name =
    is_valid_module_name name
    && (not (is_stdlib_or_internal_module name))
    && name <> module_name
    &&
    let normalized = normalize_module_name name in
    List.mem normalized project_modules
  in

  (* Based on ReScript VSCode's approach for analyzing binary files *)
  let extract_module_names content =
    let len = String.length content in
    let i = ref 0 in

    while !i < len do
      (* Look for potential module name start (capital letter) *)
      if !i < len && content.[!i] >= 'A' && content.[!i] <= 'Z' then (
        (* Extract potential module name *)
        let start = !i in
        while
          !i < len
          && ((content.[!i] >= 'A' && content.[!i] <= 'Z')
             || (content.[!i] >= 'a' && content.[!i] <= 'z')
             || (content.[!i] >= '0' && content.[!i] <= '9')
             || content.[!i] = '_'
             || content.[!i] = '\'')
        do
          incr i
        done;

        let potential_module = String.sub content start (!i - start) in

        (* Check if it's a valid module name in this project *)
        if is_valid_project_module potential_module then
          dependencies := potential_module :: !dependencies);
      incr i
    done
  in

  (try
     let ic = open_in_bin path in
     try
       (* Check magic number *)
       let magic_len = String.length cmi_magic_number in
       let buffer = really_input_string ic magic_len in

       let is_ocaml = buffer = cmi_magic_number in
       let is_rescript = buffer = rescript_magic_number in

       if not (is_ocaml || is_rescript) then (
         close_in ic;
         raise
           (Invalid_cmi_file (Printf.sprintf "Invalid magic number in %s" path)));

       (* Read version *)
       let version = input_binary_int ic in
       Printf.printf "Processing file %s (version: %d)\n" path version;

       (* Read the entire content for scanning *)
       seek_in ic 0;
       let file_size = in_channel_length ic in
       let content = really_input_string ic file_size in

       (* Extract module names from binary content *)
       extract_module_names content;

       close_in ic
     with e ->
       close_in_noerr ic;
       Printf.printf "Warning: Error parsing %s: %s\n" path
         (Printexc.to_string e)
   with Sys_error msg ->
     Printf.printf "Warning: System error with %s: %s\n" path msg);

  (* Remove duplicates and sort *)
  let unique_deps = List.sort_uniq String.compare !dependencies in

  {
    name = module_name;
    dependencies = unique_deps;
    interface_digest = None;
    implementation_digest = None;
  }

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
            else if Filename.check_suffix file ".cmi" then
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
      else if Filename.check_suffix path ".cmi" then
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
          let cmi_files = scan_directory path in
          process accum (cmi_files @ rest)
        else if Filename.check_suffix path ".cmi" then (
          try
            let module_info = parse_cmi_file project_modules path in
            process (module_info :: accum) rest
          with Invalid_cmi_file msg ->
            Printf.eprintf "Warning: %s\n" msg;
            process accum rest)
        else process accum rest
  in
  process [] paths
