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

(* Cache management module *)
module Cache = struct
  (* Cache entry type including digest information *)
  type cache_entry = { module_info : module_info }

  (* Cache storage using file paths as keys *)
  let cache_table = Hashtbl.create 100

  (* Cache file path - where the cache will be stored on disk *)
  let cache_file = ref None

  (* Set the cache file path *)
  let set_cache_file path = cache_file := Some path

  (* Get the path of the cache file, creating default if needed *)
  let get_cache_file () =
    match !cache_file with
    | Some path -> path
    | None ->
        let path =
          Filename.concat (Sys.getcwd ()) ".rescriptdep_cache.marshal"
        in
        cache_file := Some path;
        path

  (* Initialize the cache from disk if available *)
  let initialize ?(verbose = false) ?(skip_cache = false) () =
    (* Skip initialization if skip_cache is set *)
    if skip_cache then (
      if verbose then
        Printf.printf "Skip cache flag is set, not loading cache\n";
      false)
    else
      let path = get_cache_file () in
      if Sys.file_exists path then (
        try
          let ic = open_in_bin path in
          let stored_cache : (string, cache_entry) Hashtbl.t =
            Marshal.from_channel ic
          in
          close_in ic;

          (* Transfer to our in-memory cache *)
          Hashtbl.iter (fun k v -> Hashtbl.replace cache_table k v) stored_cache;

          if verbose then
            Printf.printf "Cache loaded from %s with %d entries\n" path
              (Hashtbl.length cache_table);
          true
        with e ->
          if verbose then
            Printf.printf "Error loading cache: %s\n" (Printexc.to_string e);
          false)
      else (
        if verbose then Printf.printf "No cache file found at %s\n" path;
        false)

  (* Save the cache to disk *)
  let save ?(verbose = false) ?(skip_cache = false) () =
    (* Skip saving if skip_cache is set *)
    if skip_cache then (
      if verbose then Printf.printf "Skip cache flag is set, not saving cache\n";
      false)
    else
      let path = get_cache_file () in
      try
        let oc = open_out_bin path in
        Marshal.to_channel oc cache_table [];
        close_out oc;
        if verbose then
          Printf.printf "Cache saved to %s with %d entries\n" path
            (Hashtbl.length cache_table);
        true
      with e ->
        if verbose then
          Printf.printf "Error saving cache: %s\n" (Printexc.to_string e);
        false

  (* Add or update an entry in the cache *)
  let add ?(skip_cache = false) path module_info =
    (* Skip adding to cache if skip_cache is set *)
    if not skip_cache then
      let entry = { module_info } in
      Hashtbl.replace cache_table path entry

  (* Find an entry in the cache, comparing digest information *)
  let find ?(verbose = false) ?(skip_cache = false) path
      current_interface_digest current_source_digest =
    (* Always return None if skip_cache is set *)
    if skip_cache then (
      if verbose then
        Printf.printf "Skip cache flag is set, forcing cache miss for %s\n" path;
      None)
    else
      match Hashtbl.find_opt cache_table path with
      | None ->
          if verbose then Printf.printf "Cache miss for %s\n" path;
          None
      | Some entry ->
          (* Check if the digests match *)
          let interface_match =
            match
              (entry.module_info.interface_digest, current_interface_digest)
            with
            | Some d1, Some d2 -> d1 = d2
            | None, None -> true
            | _ -> false
          in

          let implementation_match =
            match
              (entry.module_info.implementation_digest, current_source_digest)
            with
            | Some d1, Some d2 -> d1 = d2
            | None, None -> true
            | _ -> false
          in

          if interface_match && implementation_match then (
            if verbose then Printf.printf "Cache hit for %s\n" path;
            Some entry.module_info)
          else (
            if verbose then
              Printf.printf "Cache invalid for %s (digest mismatch)\n" path;
            None)

  (* Clear the cache *)
  let clear () = Hashtbl.clear cache_table
end

(* Exceptions *)
exception Invalid_cmt_file of string

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
let parse_cmt_file ?(verbose = false) ?(skip_cache = false) path =
  let module_name =
    Filename.basename path |> Filename.remove_extension |> normalize_module_name
  in

  if verbose then
    Printf.printf "Analyzing module: %s (file: %s)\n" module_name path;

  try
    (* Check if file has a cached version with matching digests *)
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

    (* Try to get from cache first using digest information *)
    match
      Cache.find ~verbose ~skip_cache path cmt_info.cmt_interface_digest
        cmt_info.cmt_source_digest
    with
    | Some cached_module_info ->
        if verbose then
          Printf.printf "Using cached module info for %s\n" module_name;
        cached_module_info
    | None ->
        (* Continue with normal processing since no valid cache entry was found *)
        if verbose then (
          Printf.printf "Processing module %s from scratch\n" module_name;
          (* Print digest information for debugging *)
          (match cmt_info.cmt_interface_digest with
          | Some digest ->
              Printf.printf "Interface digest: %s\n" (Digest.to_hex digest)
          | None -> Printf.printf "No interface digest available\n");

          match cmt_info.cmt_source_digest with
          | Some digest ->
              Printf.printf "Source digest: %s\n" (Digest.to_hex digest)
          | None -> Printf.printf "No source digest available\n");

        (* Print cmt_sourcefile for debugging *)
        (if verbose then
           match cmt_info.Cmt_format.cmt_sourcefile with
           | Some source_file ->
               Printf.printf "cmt_sourcefile for %s: %s (extension: %s)\n"
                 module_name source_file
                 (try Filename.extension source_file with _ -> "none")
           | None ->
               Printf.printf "No cmt_sourcefile found for %s\n" module_name);

        (* Extract dependencies using only the parsed cmt_info *)
        let dependencies =
          DependencyExtractor.extract_dependencies_from_cmt_info ~verbose
            cmt_info
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
          | Some source_file when Sys.file_exists source_file ->
              Some source_file
          | _ -> (
              if verbose then
                Printf.printf "Using find_implementation_file fallback for %s\n"
                  module_name;
              (* Fallback to the existing implementation if sourcefile doesn't exist *)
              let impl_file = find_implementation_file ~verbose path in
              match impl_file with Some file -> Some file | None -> Some path)
        in

        let module_info =
          {
            name = module_name;
            dependencies = filtered_deps;
            interface_digest = cmt_info.cmt_interface_digest;
            implementation_digest = cmt_info.cmt_source_digest;
            file_path;
          }
        in

        (* Add to cache *)
        Cache.add ~skip_cache path module_info;

        module_info
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
        if n = 0 then aux (List.rev current :: acc) [ hd ] (chunk_size - 1) tl
        else aux acc (hd :: current) (n - 1) tl
  in
  aux [] [] chunk_size lst

(* Parse a list of files or directories with parallel processing *)
let parse_files_or_dirs ?(verbose = false) ?(skip_cache = false) paths =
  (* Initialize benchmarking *)
  let benchmark = ref false in
  let benchmark_start = ref (Unix.gettimeofday ()) in
  let benchmark_points = ref [] in

  (* Check if benchmarking is enabled via environment variable *)
  (try benchmark := Sys.getenv "RESCRIPTDEP_BENCHMARK" = "1"
   with Not_found -> ());

  let bench_checkpoint name =
    if !benchmark then (
      let current = Unix.gettimeofday () in
      let elapsed = current -. !benchmark_start in
      benchmark_points := (name, elapsed) :: !benchmark_points;
      if verbose then
        Printf.eprintf "[PARSER-BENCH] %s: %.4f seconds\n" name elapsed)
  in

  if !benchmark then benchmark_start := Unix.gettimeofday ();
  bench_checkpoint "Parser started";

  (* Initialize the cache system *)
  let _ = Cache.initialize ~verbose ~skip_cache () in
  bench_checkpoint "Cache initialization completed";

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
            collect rest)
          else if Filename.check_suffix path ".cmt" then (
            cmt_files := path :: !cmt_files;
            collect rest)
          else collect rest
    in
    collect paths;
    !cmt_files
  in

  (* First get the list of all project modules (recursively) *)
  bench_checkpoint "Start collecting project modules";
  let project_modules = get_project_modules ~verbose paths in
  bench_checkpoint
    (Printf.sprintf "Collected %d project modules"
       (List.length project_modules));

  if verbose then
    Printf.printf "Project modules: %s\n" (String.concat ", " project_modules);

  (* Collect all CMT files to process *)
  bench_checkpoint "Start collecting CMT files";
  let cmt_files = collect_cmt_files paths in
  bench_checkpoint
    (Printf.sprintf "Collected %d CMT files" (List.length cmt_files));

  (* Process a single file *)
  let process_file file =
    try
      if verbose then Printf.printf "Processing file: %s\n" file;

      let module_info = parse_cmt_file ~verbose ~skip_cache file in

      (* Filter dependencies to only include project modules *)
      let filtered_deps =
        List.filter
          (fun dep -> List.mem dep project_modules)
          module_info.dependencies
      in

      Some { module_info with dependencies = filtered_deps }
    with Invalid_cmt_file msg ->
      if verbose then Printf.printf "Invalid cmt file: %s - %s\n" file msg;
      None
  in

  (* Process a chunk of files *)
  let process_chunk chunk = List.filter_map process_file chunk in

  (* Configure parallel processing *)
  let num_domains =
    try int_of_string (Sys.getenv "RESCRIPTDEP_DOMAINS")
    with _ ->
      (* Allocate more domains for better performance *)
      let available = Domain.recommended_domain_count () in
      min
        (available * 3 / 2)
        12 (* Use up to 150% of recommended but cap at 12 *)
  in

  let results =
    (* Process files sequentially if there are few files or parallel processing is limited *)
    if List.length cmt_files < 20 || num_domains < 2 then (
      bench_checkpoint "Using sequential processing";
      let results = process_chunk cmt_files in
      bench_checkpoint
        (Printf.sprintf "Sequential processing completed: %d modules"
           (List.length results));
      results)
    else (
      if verbose then
        Printf.printf "Using Eio with %d workers for parallel processing\n"
          num_domains;

      bench_checkpoint "Starting Eio parallel processing";

      (* Use optimized chunking strategy *)
      let total_files = List.length cmt_files in
      (* Fewer but larger chunks for better performance - aim for at least 100 files per domain *)
      let task_count = min num_domains (max 2 ((total_files / 100) + 1)) in
      let chunk_size = (total_files + task_count - 1) / task_count in
      let chunks = chunk_list chunk_size cmt_files in

      if verbose then
        Printf.printf "Split %d files into %d chunks of approx. size %d\n"
          total_files (List.length chunks) chunk_size;

      (* Use Eio for parallel processing with optimizations *)
      let results =
        Eio_main.run @@ fun env ->
        let domain_mgr = Eio.Stdenv.domain_mgr env in

        (* Array to collect results *)
        let chunk_results = Array.make (List.length chunks) [] in

        (* Process chunks in parallel *)
        Eio.Fiber.all
          (List.mapi
             (fun i chunk ->
               fun () ->
                (* Process chunk in a separate domain *)
                let result =
                  Eio.Domain_manager.run domain_mgr (fun () ->
                      process_chunk chunk)
                in
                (* Store result directly in array *)
                chunk_results.(i) <- result)
             chunks);

        (* Combine results *)
        Array.fold_left
          (fun acc result -> List.rev_append result acc)
          [] chunk_results
      in

      bench_checkpoint
        (Printf.sprintf "Eio parallel processing completed: %d modules"
           (List.length results));

      results)
  in

  (* After all files are processed, save the cache *)
  let _ = bench_checkpoint "Processing completed" in
  let _ = Cache.save ~verbose ~skip_cache () in
  bench_checkpoint "Cache saved";

  if !benchmark && verbose then (
    Printf.eprintf "\n[PARSER-BENCH] Summary:\n";
    List.rev !benchmark_points
    |> List.iter (fun (name, time) ->
           Printf.eprintf "  %s: %.4f seconds\n" name time));

  results

(* Clear the module info cache *)
let clear_cache () = Cache.clear ()

(* Set the cache file path *)
let set_cache_file path = Cache.set_cache_file path

(* Set the skip cache flag - now just a stub since we pass skip_cache directly *)
let set_skip_cache _flag = ()
