(* formatter.ml - Output formatters for dependency graphs
   This module handles formatting and outputting dependency graphs
   in various formats like DOT graphs and JSON. *)

open Stdlib
open Parse_utils

(* Output format types *)
type format =
  | Dot
  (* Format as Graphviz DOT *)
  | Json (* Format as JSON *)

(* Output a dependency graph in the specified format *)
let rec output_graph format graph out_channel =
  match format with
  | Dot -> output_dot graph out_channel
  | Json -> output_json graph out_channel

(* Output as Graphviz DOT format *)
and output_dot graph out_channel =
  let modules = Dependency_graph.get_modules graph in

  (* Output header *)
  output_string out_channel "digraph dependencies {\n";
  output_string out_channel "  rankdir=LR;\n";
  output_string out_channel
    "  node [shape=box, style=filled, fillcolor=lightblue];\n\n";

  (* Create a lookup table for all module paths *)
  let path_map = Hashtbl.create (List.length modules) in

  (* Get all source directories from module paths for dependency resolution *)
  let source_dirs = ref [] in
  List.iter
    (fun m ->
      match Dependency_graph.get_module_path graph m with
      | Some path ->
          Hashtbl.add path_map m path;
          let dir = Filename.dirname path in
          if not (List.mem dir !source_dirs) then
            source_dirs := dir :: !source_dirs
      | None -> ())
    modules;

  (* Helper function to resolve path for a module *)
  let resolve_path m =
    try Some (Hashtbl.find path_map m)
    with Not_found -> (
      match Parse_utils.find_implementation_file_by_name m !source_dirs with
      | Some p ->
          Hashtbl.add path_map m p;
          Some p
      | None -> (
          (* If not found, try to resolve from node_modules *)
          let node_path_opt =
            if List.length !source_dirs > 0 then
              Parse_utils.find_external_module_path m
                (Filename.dirname (List.hd !source_dirs))
            else None
          in
          match node_path_opt with
          | Some p ->
              Hashtbl.add path_map m p;
              Some p
          | None -> None))
  in

  (* Collect all modules including external dependencies *)
  let all_modules_set = Hashtbl.create (List.length modules * 2) in

  (* Add all known modules *)
  List.iter (fun m -> Hashtbl.replace all_modules_set m true) modules;

  (* Add all dependencies *)
  List.iter
    (fun m ->
      let deps = Dependency_graph.get_dependencies graph m in
      List.iter (fun dep -> Hashtbl.replace all_modules_set dep true) deps)
    modules;

  (* Convert set to list and sort alphabetically *)
  let all_modules =
    Hashtbl.fold (fun k _ acc -> k :: acc) all_modules_set []
    |> List.sort String.compare
  in

  (* Output nodes with metadata *)
  List.iter
    (fun module_name ->
      let path_opt = resolve_path module_name in
      let label_parts = [ module_name ] in

      (* Create label with metadata *)
      let label = String.concat "\\n" label_parts in

      (* Create tooltip with file path if available *)
      let tooltip =
        match path_opt with
        | Some path_str -> "tooltip=\"" ^ path_str ^ "\""
        | None -> ""
      in

      output_string out_channel
        ("  \"" ^ module_name ^ "\" [label=\"" ^ label ^ "\""
        ^ (if tooltip <> "" then ", " ^ tooltip else "")
        ^ "];\n"))
    all_modules;

  output_string out_channel "\n";

  (* Sort modules alphabetically before outputting edges *)
  let sorted_modules = List.sort String.compare modules in

  (* Output edges *)
  List.iter
    (fun module_name ->
      let deps = Dependency_graph.get_dependencies graph module_name in
      (* Sort dependencies alphabetically as well *)
      let sorted_deps = List.sort String.compare deps in
      List.iter
        (fun dep ->
          output_string out_channel
            ("  \"" ^ module_name ^ "\" -> \"" ^ dep ^ "\";\n"))
        sorted_deps)
    sorted_modules;

  (* Find cycles and highlight them *)
  let sccs = Dependency_graph.find_strongly_connected_components graph in
  if sccs <> [] then (
    output_string out_channel "\n  /* Cycles */\n";
    List.iteri
      (fun i scc ->
        if List.length scc > 1 then (
          output_string out_channel
            ("  subgraph cluster_" ^ string_of_int i ^ " {\n");
          output_string out_channel "    style=filled;\n";
          output_string out_channel "    color=pink;\n";
          output_string out_channel "    label=\"Cyclic dependency\";\n";
          (* Sort modules in the cycle alphabetically *)
          let sorted_scc = List.sort String.compare scc in
          List.iter
            (fun m -> output_string out_channel ("    \"" ^ m ^ "\";\n"))
            sorted_scc;
          output_string out_channel "  }\n"))
      sccs);

  output_string out_channel "}\n"

(* Output as JSON format *)
and output_json graph out_channel =
  let modules = Dependency_graph.get_modules graph in

  (* Pre-compute all expensive operations once *)
  let metrics = Dependency_graph.calculate_metrics graph in
  let cycles = Dependency_graph.find_all_cycles graph in
  let sccs = Dependency_graph.find_strongly_connected_components graph in

  (* Create a lookup table for modules in cycles *)
  let in_cycle_table = Hashtbl.create (List.length modules) in
  List.iter
    (fun scc ->
      if List.length scc > 1 then
        List.iter (fun m -> Hashtbl.replace in_cycle_table m true) scc)
    sccs;

  (* Pre-compute all module paths and create a lookup map *)
  let path_map = Hashtbl.create (List.length modules) in
  List.iter
    (fun m ->
      match Dependency_graph.get_module_path graph m with
      | Some path -> Hashtbl.add path_map m path
      | None -> ())
    modules;

  (* Get all source directories from module paths for dependency resolution *)
  let source_dirs = ref [] in
  List.iter
    (fun m ->
      match Dependency_graph.get_module_path graph m with
      | Some path ->
          let dir = Filename.dirname path in
          if not (List.mem dir !source_dirs) then
            source_dirs := dir :: !source_dirs
      | None -> ())
    modules;

  (* Pre-compute all dependencies and dependents *)
  let deps_map = Hashtbl.create (List.length modules) in
  let dependents_map = Hashtbl.create (List.length modules) in

  (* First compute all dependencies *)
  List.iter
    (fun m ->
      let deps = Dependency_graph.get_dependencies graph m in
      Hashtbl.add deps_map m deps)
    modules;

  (* Then compute all dependents using the deps map *)
  List.iter
    (fun m ->
      let deps = try Hashtbl.find deps_map m with Not_found -> [] in
      List.iter
        (fun dep ->
          let current_dependents =
            try Hashtbl.find dependents_map dep with Not_found -> []
          in
          Hashtbl.replace dependents_map dep (m :: current_dependents))
        deps)
    modules;

  (* Helper function to resolve path for a module *)
  let resolve_path m =
    try Some (Hashtbl.find path_map m)
    with Not_found -> (
      match Parse_utils.find_implementation_file_by_name m !source_dirs with
      | Some p ->
          Hashtbl.add path_map m p;
          Some p
      | None -> (
          (* If not found, try to resolve from node_modules *)
          let node_path_opt =
            if List.length !source_dirs > 0 then
              Parse_utils.find_external_module_path m
                (Filename.dirname (List.hd !source_dirs))
            else None
          in
          match node_path_opt with
          | Some p ->
              Hashtbl.add path_map m p;
              Some p
          | None -> None))
  in

  (* JSON opening *)
  output_string out_channel "{\n";

  (* Output modules *)
  output_string out_channel "  \"modules\": [\n";

  List.iteri
    (fun i module_name ->
      let deps = try Hashtbl.find deps_map module_name with Not_found -> [] in
      let dependents =
        try Hashtbl.find dependents_map module_name with Not_found -> []
      in
      let path_opt =
        try Some (Hashtbl.find path_map module_name) with Not_found -> None
      in

      output_string out_channel "    {\n";
      output_string out_channel ("      \"name\": \"" ^ module_name ^ "\",\n");

      (* Add file path info if available *)
      (match path_opt with
      | Some path_str ->
          output_string out_channel ("      \"path\": \"" ^ path_str ^ "\",\n")
      | None -> output_string out_channel "      \"path\": null,\n");

      (* Dependencies - modules this module depends on *)
      output_string out_channel "      \"dependencies\": [";

      if deps <> [] then (
        output_string out_channel "\n";
        List.iteri
          (fun j dep ->
            let dep_path_opt = resolve_path dep in

            output_string out_channel "        {\n";
            output_string out_channel ("          \"name\": \"" ^ dep ^ "\"");

            (* Add dependency file path if available *)
            (match dep_path_opt with
            | Some path_str ->
                output_string out_channel
                  (",\n          \"path\": \"" ^ path_str ^ "\"")
            | None -> output_string out_channel ",\n          \"path\": null");

            output_string out_channel "\n        }";
            if j < List.length deps - 1 then output_string out_channel ",\n"
            else output_string out_channel "\n")
          deps;
        output_string out_channel "      ");

      output_string out_channel "],\n";

      (* Dependents - modules that depend on this module *)
      output_string out_channel "      \"dependents\": [";

      if dependents <> [] then (
        output_string out_channel "\n";
        List.iteri
          (fun j dependent ->
            let dependent_path_opt = resolve_path dependent in

            output_string out_channel "        {\n";
            output_string out_channel
              ("          \"name\": \"" ^ dependent ^ "\"");

            (* Add dependent file path if available *)
            (match dependent_path_opt with
            | Some path_str ->
                output_string out_channel
                  (",\n          \"path\": \"" ^ path_str ^ "\"")
            | None -> output_string out_channel ",\n          \"path\": null");

            output_string out_channel "\n        }";
            if j < List.length dependents - 1 then
              output_string out_channel ",\n"
            else output_string out_channel "\n")
          dependents;
        output_string out_channel "      ");

      output_string out_channel "],\n";

      (* Get additional metrics *)
      let fan_in = List.length dependents in
      let fan_out = List.length deps in
      output_string out_channel ("      \"fan_in\": " ^ string_of_int fan_in);
      output_string out_channel
        (",\n      \"fan_out\": " ^ string_of_int fan_out);

      (* Check for circular dependencies using pre-computed table *)
      let is_in_cycle =
        try Hashtbl.find in_cycle_table module_name with Not_found -> false
      in
      output_string out_channel
        (",\n      \"in_cycle\": " ^ string_of_bool is_in_cycle);

      output_string out_channel "\n    }";
      if i < List.length modules - 1 then output_string out_channel ",\n"
      else output_string out_channel "\n")
    modules;

  output_string out_channel "  ],\n";

  (* Output cycles information *)
  output_string out_channel "  \"cycles\": [";

  if cycles <> [] then (
    output_string out_channel "\n";
    List.iteri
      (fun i cycle ->
        output_string out_channel "    [";
        List.iteri
          (fun j module_name ->
            output_string out_channel ("\"" ^ module_name ^ "\"");
            if j < List.length cycle - 1 then output_string out_channel ", ")
          cycle;
        output_string out_channel "]";
        if i < List.length cycles - 1 then output_string out_channel ",\n"
        else output_string out_channel "\n")
      cycles;
    output_string out_channel "  ");

  output_string out_channel "],\n";

  (* Calculate and output metrics *)
  output_string out_channel "  \"metrics\": {\n";
  output_string out_channel
    ("    \"total_modules\": " ^ string_of_int (List.length modules) ^ ",\n");

  (* Calculate average fan-in and fan-out *)
  let total_fan_in, total_fan_out =
    List.fold_left
      (fun (in_acc, out_acc) (_, fan_in, fan_out) ->
        (in_acc + fan_in, out_acc + fan_out))
      (0, 0) metrics
  in

  let avg_fan_in =
    float_of_int total_fan_in /. float_of_int (max 1 (List.length modules))
  in
  let avg_fan_out =
    float_of_int total_fan_out /. float_of_int (max 1 (List.length modules))
  in

  let format_float f =
    let s = string_of_float f in
    let len = String.length s in
    if len > 0 && s.[len - 1] = '.' then s ^ "0"
    (* Add 0 if ending with decimal point *)
      else s
  in

  output_string out_channel
    ("    \"average_fan_in\": " ^ format_float avg_fan_in ^ ",\n");
  output_string out_channel
    ("    \"average_fan_out\": " ^ format_float avg_fan_out ^ ",\n");

  (* Find module with highest fan-in (most depended upon) *)
  let max_fan_in_module, max_fan_in, _ =
    List.fold_left
      (fun (max_m, max_in, max_out) (m, fan_in, fan_out) ->
        if fan_in > max_in then (m, fan_in, fan_out)
        else (max_m, max_in, max_out))
      ("", 0, 0) metrics
  in

  (* Find module with highest fan-out (depends on most) *)
  let max_fan_out_module, _, max_fan_out =
    List.fold_left
      (fun (max_m, max_in, max_out) (m, fan_in, fan_out) ->
        if fan_out > max_out then (m, fan_in, fan_out)
        else (max_m, max_in, max_out))
      ("", 0, 0) metrics
  in

  output_string out_channel
    ("    \"most_depended_upon\": {\"module\": \"" ^ max_fan_in_module
   ^ "\", \"count\": " ^ string_of_int max_fan_in ^ "},\n");

  output_string out_channel
    ("    \"most_dependencies\": {\"module\": \"" ^ max_fan_out_module
   ^ "\", \"count\": " ^ string_of_int max_fan_out ^ "},\n");

  output_string out_channel
    ("    \"cycles_count\": " ^ string_of_int (List.length cycles) ^ "\n");
  output_string out_channel "  }\n";

  (* JSON closing *)
  output_string out_channel "}\n"

(* Output modules with no dependents *)
let output_no_dependents_dot modules out_channel =
  output_string out_channel "digraph G {\n";
  output_string out_channel "  rankdir=LR;\n";
  output_string out_channel "  node [shape=box];\n";
  List.iter
    (fun m -> output_string out_channel (Printf.sprintf "  \"%s\";\n" m))
    modules;
  output_string out_channel "}\n"

let output_no_dependents_json modules out_channel =
  let json = `List (List.map (fun m -> `String m) modules) in
  Yojson.Basic.pretty_to_channel out_channel json

let output_no_dependents format graph out_channel =
  let modules = Dependency_graph.find_modules_with_no_dependents graph in
  match format with
  | Dot -> output_no_dependents_dot modules out_channel
  | Json -> output_no_dependents_json modules out_channel

(* String representation of a format *)
let format_to_string = function Dot -> "dot" | Json -> "json"

(* Convert string to format *)
let format_of_string = function
  | "dot" -> Some Dot
  | "json" -> Some Json
  | _ -> None

(* Get all supported formats *)
let all_formats = [ Dot; Json ]

(* Get the default format *)
let default_format = Dot
