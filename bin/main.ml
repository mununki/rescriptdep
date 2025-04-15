open Stdlib

let input_files = ref []
let output_file = ref None
let format = ref Rescriptdep.Formatter.Dot
let focus_module = ref None
let verbose = ref false
let benchmark = ref false
let skip_cache = ref false
let clear_cache = ref false
let no_dependents = ref false
let value_binding = ref None

let spec_list =
  [
    ( "-o",
      Arg.String (fun s -> output_file := Some s),
      "Output file (default: stdout)" );
    ( "--output",
      Arg.String (fun s -> output_file := Some s),
      "Output file (default: stdout)" );
    ( "-f",
      Arg.Symbol
        ( [ "dot"; "json" ],
          fun s ->
            format :=
              match s with
              | "dot" -> Rescriptdep.Formatter.Dot
              | "json" -> Rescriptdep.Formatter.Json
              | _ -> !format ),
      "Output format (dot, json)" );
    ( "--format",
      Arg.Symbol
        ( [ "dot"; "json" ],
          fun s ->
            format :=
              match s with
              | "dot" -> Rescriptdep.Formatter.Dot
              | "json" -> Rescriptdep.Formatter.Json
              | _ -> !format ),
      "Output format (dot, json)" );
    ( "-m",
      Arg.String (fun s -> focus_module := Some s),
      "Focus on specific module and its dependencies" );
    ( "--module",
      Arg.String (fun s -> focus_module := Some s),
      "Focus on specific module and its dependencies" );
    ("-v", Arg.Set verbose, "Enable verbose output");
    ("--verbose", Arg.Set verbose, "Enable verbose output");
    ("-b", Arg.Set benchmark, "Enable performance benchmarking");
    ("--benchmark", Arg.Set benchmark, "Enable performance benchmarking");
    ("--no-cache", Arg.Set skip_cache, "Skip using the in-memory cache");
    ( "--clear-cache",
      Arg.Set clear_cache,
      "Clear the in-memory cache before analyzing" );
    ( "--no-dependents",
      Arg.Set no_dependents,
      "Output modules with no dependents" );
    ( "-nd",
      Arg.Set no_dependents,
      "Output modules with no dependents (short for --no-dependents)" );
    ( "-vb",
      Arg.String (fun s -> value_binding := Some s),
      "Analyze usage count of a value binding in dependents of the focused \
       module" );
    ( "--value-binding",
      Arg.String (fun s -> value_binding := Some s),
      "Analyze usage count of a value binding in dependents of the focused \
       module" );
  ]

let anon_fun file = input_files := file :: !input_files

let parse_args () =
  Arg.parse spec_list anon_fun
    "Usage: rescriptdep [options] files_or_dirs\nOptions:";
  input_files := List.rev !input_files

let main () =
  parse_args ();

  if !input_files = [] then (
    Printf.eprintf "Error: No input files or directories specified.\n";
    Arg.usage spec_list "Usage: rescriptdep [options] files_or_dirs\nOptions:";
    exit 1);

  try
    (* Initialize timing *)
    let start_time = Unix.gettimeofday () in
    let time_checkpoint name =
      if !benchmark then
        let current = Unix.gettimeofday () in
        let elapsed = current -. start_time in
        Printf.eprintf "[BENCH] %s: %.4f seconds\n" name elapsed
    in

    time_checkpoint "Start";

    (* Clear in-memory cache if requested *)
    if !clear_cache then (
      if !verbose then Printf.eprintf "Clearing in-memory cache\n";
      Rescriptdep.Parser.clear_cache ());

    (* Parse files and directories to get module infos *)
    let module_infos =
      if !skip_cache then (
        if !verbose then Printf.eprintf "Skipping in-memory cache usage\n";
        Rescriptdep.Parser.clear_cache ();
        Rescriptdep.Parser.parse_files_or_dirs ~verbose:!verbose
          ~skip_cache:true !input_files)
      else
        Rescriptdep.Parser.parse_files_or_dirs ~verbose:!verbose
          ~skip_cache:false !input_files
    in

    time_checkpoint "Parsing completed";

    (* Build dependency graph *)
    let graph =
      Rescriptdep.Dependency_graph.build_from_module_infos module_infos
    in

    time_checkpoint "Graph building completed";

    (* 3: If both -m and -vb are specified, only output value usage count *)
    (match (!focus_module, !value_binding) with
    | Some module_name, Some value_name ->
        let normalized_name =
          Rescriptdep.Parse_utils.normalize_module_name module_name
        in
        let focused_graph =
          Rescriptdep.Dependency_graph.create_focused_graph graph
            normalized_name
        in
        time_checkpoint "Module focusing completed";
        let usage_list =
          Rescriptdep.Dependency_graph.count_value_usage_in_dependents
            focused_graph ~module_name:normalized_name ~value_name
        in
        let output_to =
          match !output_file with
          | Some file -> Some (open_out file)
          | None -> None
        in
        (match output_to with
        | Some ch ->
            Rescriptdep.Formatter.output_value_usage !format usage_list ch;
            close_out ch
        | None ->
            Rescriptdep.Formatter.output_value_usage !format usage_list stdout);
        exit 0
    | _ -> ());

    (* 1,2: Graph output branch *)
    let focused_graph =
      match !focus_module with
      | Some module_name ->
          let normalized_name =
            Rescriptdep.Parse_utils.normalize_module_name module_name
          in
          let result =
            Rescriptdep.Dependency_graph.create_focused_graph graph
              normalized_name
          in
          time_checkpoint "Module focusing completed";
          result
      | None ->
          (* If no focus module is specified, filter out standard modules *)
          let filtered_graph =
            Rescriptdep.Dependency_graph.create_filtered_graph graph
          in
          time_checkpoint "Standard module filtering completed";
          filtered_graph
    in

    (* Output the graph to the specified output *)
    if !verbose then
      Printf.eprintf "Output format: %s\n"
        (match !format with
        | Rescriptdep.Formatter.Dot -> "dot"
        | Rescriptdep.Formatter.Json -> "json");

    let output_start_time = Unix.gettimeofday () in

    (match !output_file with
    | Some file ->
        (* Output to file only *)
        if !verbose then Printf.eprintf "Writing output to file: %s\n" file;
        let out_channel = open_out file in
        if !no_dependents then
          Rescriptdep.Formatter.output_no_dependents !format focused_graph
            out_channel
        else
          Rescriptdep.Formatter.output_graph !format focused_graph out_channel;
        close_out out_channel
    | None ->
        (* Output to stdout when no file output option is provided *)
        if !verbose then Printf.eprintf "Writing output to stdout\n";
        if !no_dependents then
          Rescriptdep.Formatter.output_no_dependents !format focused_graph
            stdout
        else Rescriptdep.Formatter.output_graph !format focused_graph stdout);

    let output_end_time = Unix.gettimeofday () in
    let output_time = output_end_time -. output_start_time in

    if !benchmark then
      Printf.eprintf "[BENCH] Pure output generation: %.4f seconds\n"
        output_time;

    time_checkpoint "Output generation completed";

    if !benchmark then (
      let total_time = Unix.gettimeofday () -. start_time in
      Printf.eprintf "[BENCH] Total execution time: %.4f seconds\n" total_time;
      exit 0)
  with
  | Rescriptdep.Parser.Invalid_cmt_file msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "System error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

let () = main ()
