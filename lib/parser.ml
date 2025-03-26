open Stdlib
open Parse_utils

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
  let is_module_used_in_source ?(verbose = false) source_file module_name =
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
          if verbose then
            Printf.printf "Module %s is actually used in %s\n" module_name
              source_file
          else if verbose then
            Printf.printf "Module %s is not used in source %s\n" module_name
              source_file;

        is_used
      with _ ->
        if verbose then
          Printf.printf "Error reading source file %s\n" source_file;
        true (* In case of file reading failure, conservatively return true *)

  (* Extract dependencies from cmt_info structure *)
  let extract_dependencies_from_cmt_info ?(verbose = false) cmt_info =
    let deps = ref [] in

    (* Check source file path - directly use cmt_sourcefile *)
    let source_file =
      match cmt_info.Cmt_format.cmt_sourcefile with
      | Some path when Sys.file_exists path ->
          if verbose then
            Printf.printf "Using cmt_sourcefile directly: %s\n" path;
          path
      | Some _ | None ->
          if verbose then
            Printf.printf "cmt_sourcefile not usable for %s\n"
              cmt_info.Cmt_format.cmt_modname;
          "" (* No valid source file available *)
    in

    (* Extract module names directly from imports list *)
    (try
       List.iter
         (fun (module_name, _) ->
           if
             is_valid_module_name module_name
             && (not (is_stdlib_or_internal_module module_name))
             && (source_file = ""
                || is_module_used_in_source ~verbose source_file module_name)
           then deps := module_name :: !deps)
         cmt_info.Cmt_format.cmt_imports
     with _ -> ());

    (* Return unique dependencies *)
    List.sort_uniq String.compare !deps
end

(* Find the implementation file using various strategies *)
let find_implementation_file ?(verbose = false) cmt_path =
  (* First, try direct path transformation from lib/bs/src to src *)
  if Str.string_match (Str.regexp "\\(.*\\)/lib/bs/src\\(/.*\\)?$") cmt_path 0
  then (
    let project_root = Str.matched_group 1 cmt_path in
    let base_name = Filename.basename (Filename.remove_extension cmt_path) in

    let rel_path =
      try
        let sub_path = Str.matched_group 2 cmt_path in
        if sub_path = "" then base_name
        else
          let sub_dir = String.sub sub_path 1 (String.length sub_path - 1) in
          if Filename.basename sub_dir = base_name then sub_dir
          else Filename.concat sub_dir base_name
      with Not_found -> base_name
    in

    let src_path =
      Filename.concat (Filename.concat project_root "src") rel_path
    in

    (* Try with different extensions *)
    let extensions = [ ".res"; ".re"; ".ml" ] in
    let candidates = List.map (fun ext -> src_path ^ ext) extensions in

    try
      let found = List.find Sys.file_exists candidates in
      if verbose then
        Printf.printf "Found source file with direct path conversion: %s\n"
          found;
      Some found
    with Not_found ->
      if verbose then
        Printf.printf
          "Direct path conversion failed for %s, trying other methods\n"
          cmt_path;

      (* Continue with existing implementation if direct conversion failed *)
      (* Try to find .re or .res file by replacing .cmt extension *)
      let base_path = Filename.remove_extension cmt_path in
      let dir_path = Filename.dirname cmt_path in
      let base_name = Filename.basename base_path in
      let module_name = normalize_module_name base_name in

      (* Search for source directories by finding project root directory *)
      let rec find_src_dir current_dir depth =
        if depth > 5 then [] (* Limit depth to prevent excessive searching *)
        else
          let src_dir = Filename.concat current_dir "src" in
          if Sys.file_exists src_dir && Sys.is_directory src_dir then
            [ src_dir ]
          else
            let parent = Filename.dirname current_dir in
            if parent = current_dir then []
              (* When reached the root directory *)
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
          if Sys.file_exists src_dir && Sys.is_directory src_dir then
            [ src_dir ]
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

      if verbose then
        List.iter (fun dir -> Printf.printf "  - %s\n" dir) unique_dirs;

      (* Use the utility function from Parse_utils to find the implementation file *)
      find_implementation_file_by_name ~verbose module_name unique_dirs)
  else
    (* If not a lib/bs/src path, use the original logic *)
    let base_path = Filename.remove_extension cmt_path in
    let dir_path = Filename.dirname cmt_path in
    let base_name = Filename.basename base_path in
    let module_name = normalize_module_name base_name in

    (* Existing directory search logic *)
    let rec find_src_dir current_dir depth =
      if depth > 5 then []
      else
        let src_dir = Filename.concat current_dir "src" in
        if Sys.file_exists src_dir && Sys.is_directory src_dir then [ src_dir ]
        else
          let parent = Filename.dirname current_dir in
          if parent = current_dir then [] else find_src_dir parent (depth + 1)
    in

    let current_src_dirs = find_src_dir dir_path 0 in
    let search_dirs =
      [ dir_path; Filename.dirname dir_path ] @ current_src_dirs
    in

    let unique_dirs =
      let rec remove_dups seen = function
        | [] -> []
        | dir :: rest ->
            if List.mem dir seen then remove_dups seen rest
            else dir :: remove_dups (dir :: seen) rest
      in
      remove_dups [] search_dirs
    in

    if verbose then
      List.iter (fun dir -> Printf.printf "  - %s\n" dir) unique_dirs;

    find_implementation_file_by_name ~verbose module_name unique_dirs

(* Parse a cmt file and extract module information *)
let parse_cmt_file ?(verbose = false) path =
  let module_name =
    Filename.basename path |> Filename.remove_extension |> normalize_module_name
  in

  if verbose then
    Printf.printf "Analyzing module: %s (file: %s)\n" module_name path;

  try
    (* Use Cmt_format to read the file - with relaxed format checking *)
    let cmt_info =
      try Cmt_format.read_cmt path
      with Cmt_format.Error msg ->
        (* Provide more detailed error information *)
        if verbose then
          Printf.printf
            "Warning: Problem with CMT file %s: %s (continuing anyway)\n" path
            msg;

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

    (* Print cmt_sourcefile for debugging *)
    (if verbose then
       match cmt_info.Cmt_format.cmt_sourcefile with
       | Some source_file ->
           Printf.printf "cmt_sourcefile for %s: %s (extension: %s)\n"
             module_name source_file
             (try Filename.extension source_file with _ -> "none")
       | None -> Printf.printf "No cmt_sourcefile found for %s\n" module_name);

    (* Extract dependencies using only the parsed cmt_info *)
    let dependencies =
      DependencyExtractor.extract_dependencies_from_cmt_info ~verbose cmt_info
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

    (* Use cmt_sourcefile directly if available *)
    let file_path =
      match cmt_info.Cmt_format.cmt_sourcefile with
      | Some source_file when Sys.file_exists source_file -> Some source_file
      | _ -> (
          if verbose then
            Printf.printf "Using find_implementation_file fallback for %s\n"
              module_name;
          (* Fallback to the existing implementation if sourcefile doesn't exist *)
          let impl_file = find_implementation_file ~verbose path in
          match impl_file with Some file -> Some file | None -> Some path)
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
let get_project_modules ?(verbose = false) paths =
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

(* Helper function to split a list into chunks *)
let chunk_list chunk_size lst =
  let rec aux acc current n = function
    | [] -> List.rev (if current = [] then acc else List.rev current :: acc)
    | hd :: tl ->
        if n = 0 then
          aux (List.rev current :: acc) [hd] (chunk_size - 1) tl
        else
          aux acc (hd :: current) (n - 1) tl
  in
  aux [] [] chunk_size lst

(* Parse a list of files or directories with parallel processing *)
let parse_files_or_dirs ?(verbose = false) paths =
  (* Initialize benchmarking *)
  let benchmark = ref false in
  let benchmark_start = ref (Unix.gettimeofday()) in
  let benchmark_points = ref [] in
  
  (* Check if benchmarking is enabled via environment variable *)
  begin
    try 
      benchmark := Sys.getenv "RESCRIPTDEP_BENCHMARK" = "1"
    with Not_found -> ()
  end;
  
  let bench_checkpoint name =
    if !benchmark then begin
      let current = Unix.gettimeofday() in
      let elapsed = current -. !benchmark_start in
      benchmark_points := (name, elapsed) :: !benchmark_points;
      if verbose then
        Printf.eprintf "[PARSER-BENCH] %s: %.4f seconds\n" name elapsed
    end
  in
  
  if !benchmark then benchmark_start := Unix.gettimeofday();
  bench_checkpoint "Parser started";

  (* Collect all cmt files *)
  let collect_cmt_files paths =
    let cmt_files = ref [] in
    let rec collect = function
      | [] -> ()
      | path :: rest ->
          if Sys.is_directory path then (
            if verbose then Printf.printf "Scanning directory: %s\n" path;
            let dir_cmt_files = scan_directory path in
            cmt_files := dir_cmt_files @ !cmt_files;
            collect rest
          ) else if Filename.check_suffix path ".cmt" then (
            cmt_files := path :: !cmt_files;
            collect rest
          ) else
            collect rest
    in
    collect paths;
    !cmt_files
  in

  (* First get the list of all project modules (recursively) *)
  bench_checkpoint "Start collecting project modules";
  let project_modules = get_project_modules ~verbose paths in
  bench_checkpoint (Printf.sprintf "Collected %d project modules" (List.length project_modules));
  
  if verbose then
    Printf.printf "Project modules: %s\n" (String.concat ", " project_modules);

  (* Collect all CMT files to process *)
  bench_checkpoint "Start collecting CMT files";
  let cmt_files = collect_cmt_files paths in
  bench_checkpoint (Printf.sprintf "Collected %d CMT files" (List.length cmt_files));

  (* Process a single file *)
  let process_file file =
    try
      if verbose then
        Printf.printf "Processing file: %s\n" file;
      
      let module_info = parse_cmt_file ~verbose file in
      
      (* Filter dependencies to only include project modules *)
      let filtered_deps =
        List.filter
          (fun dep -> List.mem dep project_modules)
          module_info.dependencies
      in
      
      Some { module_info with dependencies = filtered_deps }
    with Invalid_cmt_file msg ->
      if verbose then
        Printf.printf "Invalid cmt file: %s - %s\n" file msg;
      None
  in

  (* Process a chunk of files *)
  let process_chunk chunk =
    List.filter_map process_file chunk
  in

  (* Configure parallel processing *)
  let num_domains = 
    try int_of_string (Sys.getenv "RESCRIPTDEP_DOMAINS")
    with _ -> 
      max 2 (min 8 (Domain.recommended_domain_count () - 1))
  in

  let results =
    (* Process files sequentially if there are few files or parallel processing is limited *)
    if List.length cmt_files < 20 || num_domains < 2 then begin
      bench_checkpoint "Using sequential processing";
      let results = process_chunk cmt_files in
      bench_checkpoint (Printf.sprintf "Sequential processing completed: %d modules" (List.length results));
      results
    end else begin
      if verbose then
        Printf.printf "Using %d domains for parallel processing\n" num_domains;

      bench_checkpoint "Starting parallel processing";
      
      (* Split files into chunks *)
      let chunk_size = max 1 (List.length cmt_files / num_domains) in
      let chunks = chunk_list chunk_size cmt_files in
      
      if verbose then
        Printf.printf "Split %d files into %d chunks of approx. size %d\n" 
          (List.length cmt_files) (List.length chunks) chunk_size;
      
      (* Create a queue for tasks to be processed by each domain *)
      let all_results = Atomic.make [] in
      
      (* Create domains and assign tasks *)
      let domains = 
        List.mapi (fun i chunk ->
          if verbose then 
            Printf.printf "Starting domain %d with %d files\n" i (List.length chunk);
          
          Domain.spawn (fun () ->
            let domain_results = process_chunk chunk in
            
            (* Update results atomically *)
            let rec update () =
              let current = Atomic.get all_results in
              if not (Atomic.compare_and_set all_results current (domain_results @ current)) then
                update ()
            in
            update ()
          )
        ) chunks
      in
      
      (* Wait for all domains to complete *)
      List.iter Domain.join domains;
      
      let final_results = Atomic.get all_results in
      bench_checkpoint (Printf.sprintf "Parallel processing completed: %d modules" (List.length final_results));
      
      final_results
    end
  in

  if !benchmark && verbose then begin
    Printf.eprintf "\n[PARSER-BENCH] Summary:\n";
    List.rev !benchmark_points |> List.iter (fun (name, time) ->
      Printf.eprintf "  %s: %.4f seconds\n" name time
    );
  end;
  
  results
