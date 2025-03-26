open Stdlib

let input_files = ref []
let output_file = ref None
let format = ref Rescriptdep.Formatter.Dot
let focus_module = ref None
let verbose = ref false
let benchmark = ref false
let skip_cache = ref false
let cache_file = ref None
let clear_cache = ref false

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
    ("--no-cache", Arg.Set skip_cache, "Skip using the cache");
    ( "--cache-file",
      Arg.String (fun s -> cache_file := Some s),
      "Specify cache file location" );
    ("--clear-cache", Arg.Set clear_cache, "Clear the cache before analyzing");
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

    (* Setup cache configuration *)

    (* Set cache file if provided *)
    (match !cache_file with
    | Some path -> Rescriptdep.Parser.set_cache_file path
    | None ->
        ();

        (* Clear cache if requested *)
        if !clear_cache then (
          if !verbose then Printf.eprintf "Clearing cache\n";
          Rescriptdep.Parser.clear_cache ()));

    (* Parse files and directories to get module infos *)
    let module_infos =
      if !skip_cache then (
        if !verbose then Printf.eprintf "Skipping cache usage\n";
        Rescriptdep.Parser.clear_cache ();
        Rescriptdep.Parser.parse_files_or_dirs ~verbose:!verbose !input_files)
      else Rescriptdep.Parser.parse_files_or_dirs ~verbose:!verbose !input_files
    in

    time_checkpoint "Parsing completed";

    (* Build dependency graph *)
    let graph =
      Rescriptdep.Dependency_graph.build_from_module_infos module_infos
    in

    time_checkpoint "Graph building completed";

    (* Apply module focus if specified *)
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
      | None -> graph
    in

    (* Output the graph to the specified output *)
    if !verbose then
      Printf.eprintf "Output format: %s\n"
        (match !format with
        | Rescriptdep.Formatter.Dot -> "dot"
        | Rescriptdep.Formatter.Json -> "json");

    (match !output_file with
    | Some file ->
        (* Output to file only *)
        if !verbose then Printf.eprintf "Writing output to file: %s\n" file;
        let out_channel = open_out file in
        Rescriptdep.Formatter.output_graph !format focused_graph out_channel;
        close_out out_channel
    | None ->
        (* Output to stdout when no file output option is provided *)
        if !verbose then Printf.eprintf "Writing output to stdout\n";
        Rescriptdep.Formatter.output_graph !format focused_graph stdout);

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
