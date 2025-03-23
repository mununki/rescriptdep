open Stdlib

let input_files = ref []
let output_file = ref None
let format = ref Rescriptdep.Formatter.Dot
let focus_module = ref None

let spec_list = [
  ("-o", Arg.String (fun s -> output_file := Some s), "Output file (default: stdout)");
  ("--output", Arg.String (fun s -> output_file := Some s), "Output file (default: stdout)");
  
  ("-f", Arg.Symbol (["dot"; "json"], 
                    (fun s -> format := match s with 
                              | "dot" -> Rescriptdep.Formatter.Dot 
                              | "json" -> Rescriptdep.Formatter.Json
                              | _ -> !format)), 
        "Output format (dot, json)");
  ("--format", Arg.Symbol (["dot"; "json"], 
                          (fun s -> format := match s with 
                                    | "dot" -> Rescriptdep.Formatter.Dot 
                                    | "json" -> Rescriptdep.Formatter.Json
                                    | _ -> !format)), 
              "Output format (dot, json)");
  
  ("-m", Arg.String (fun s -> focus_module := Some s), "Focus on specific module and its dependencies");
  ("--module", Arg.String (fun s -> focus_module := Some s), "Focus on specific module and its dependencies");
]

let anon_fun file =
  input_files := file :: !input_files

let parse_args () =
  Arg.parse spec_list anon_fun "Usage: rescriptdep [options] files_or_dirs\nOptions:";
  input_files := List.rev !input_files

let main () =
  parse_args ();
  
  if !input_files = [] then begin
    Printf.eprintf "Error: No input files or directories specified.\n";
    Arg.usage spec_list "Usage: rescriptdep [options] files_or_dirs\nOptions:";
    exit 1
  end;
  
  try
    (* Parse files and directories to get module infos *)
    let module_infos = Rescriptdep.Cmi_parser.parse_files_or_dirs !input_files in
    
    (* Build dependency graph *)
    let graph = Rescriptdep.Dependency_graph.build_from_module_infos module_infos in
    
    (* Apply module focus if specified *)
    let focused_graph = 
      match !focus_module with
      | Some module_name -> 
        let normalized_name = Rescriptdep.Cmi_parser.normalize_module_name module_name in
        Rescriptdep.Dependency_graph.create_focused_graph graph normalized_name
      | None -> graph
    in
    
    (* Output the graph to the specified output *)
    let out_channel = 
      match !output_file with
      | Some file -> open_out file
      | None -> stdout
    in
    
    Rescriptdep.Formatter.output_graph !format focused_graph out_channel;
    
    if !output_file <> None then close_out out_channel;
    
    exit 0
  with
  | Rescriptdep.Cmi_parser.Invalid_cmi_file msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "System error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

let () = main ()