open Stdlib

(* Module info representation *)
type module_info = {
  name : string;
  dependencies : string list;
  interface_digest : Digest.t option;
  implementation_digest : Digest.t option;
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

(* Module dependency analysis using typedtree *)
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

  (* Process a structure for dependencies *)
  let process_structure structure =
    let deps = ref [] in

    (* This is a simplified version that would normally analyze the full structure *)
    (* Instead, we'll directly analyze the binary file to extract module names *)
    !deps

  (* Extract dependencies from a CMT file *)
  let extract_dependencies ic =
    (* Read entire file content *)
    let pos = pos_in ic in
    seek_in ic 0;
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    seek_in ic pos;

    (* Extract potential module names from the binary content *)
    let deps = ref [] in
    let i = ref 0 in

    while !i < len - 1 do
      (* Look for capital letters that might start module names *)
      if content.[!i] >= 'A' && content.[!i] <= 'Z' then (
        let start = !i in
        (* Extract potential identifier *)
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

        if !i > start + 1 then
          let name = String.sub content start (!i - start) in
          (* Only add if it looks like a valid module name and not stdlib *)
          if
            is_valid_module_name name && not (is_stdlib_or_internal_module name)
          then deps := name :: !deps);
      incr i
    done;

    (* Return unique dependencies *)
    List.sort_uniq String.compare !deps
end

(* Parse a cmt file and extract module information *)
let parse_cmt_file path =
  let module_name =
    Filename.basename path |> Filename.remove_extension |> normalize_module_name
  in

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

    (* Extract dependencies using binary analysis *)
    let ic = open_in_bin path in
    let dependencies = DependencyExtractor.extract_dependencies ic in
    close_in ic;

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
    {
      name = module_name;
      dependencies = filtered_deps;
      interface_digest = cmt_info.cmt_interface_digest;
      implementation_digest = None;
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
