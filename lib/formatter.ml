(* formatter.ml - Output formatters for dependency graphs
   This module handles formatting and outputting dependency graphs
   in various formats like DOT graphs and JSON. *)

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

  (* Output nodes with metadata *)
  List.iter
    (fun module_name ->
      let path = Dependency_graph.get_module_path graph module_name in
      let label_parts = [ module_name ] in

      (* Create label with metadata *)
      let label = String.concat "\\n" label_parts in

      (* Create tooltip with file path if available *)
      let tooltip =
        match path with
        | Some path_str -> "tooltip=\"" ^ path_str ^ "\""
        | None -> ""
      in

      output_string out_channel
        ("  \"" ^ module_name ^ "\" [label=\"" ^ label ^ "\""
        ^ (if tooltip <> "" then ", " ^ tooltip else "")
        ^ "];\n"))
    modules;

  output_string out_channel "\n";

  (* Output edges *)
  List.iter
    (fun module_name ->
      let deps = Dependency_graph.get_dependencies graph module_name in
      List.iter
        (fun dep ->
          output_string out_channel
            ("  \"" ^ module_name ^ "\" -> \"" ^ dep ^ "\";\n"))
        deps)
    modules;

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
          List.iter
            (fun m -> output_string out_channel ("    \"" ^ m ^ "\";\n"))
            scc;
          output_string out_channel "  }\n"))
      sccs);

  output_string out_channel "}\n"

(* Output as JSON format *)
and output_json graph out_channel =
  let modules = Dependency_graph.get_modules graph in

  (* JSON opening *)
  output_string out_channel "{\n";
  output_string out_channel "  \"modules\": [\n";

  (* Output each module and its dependencies *)
  List.iteri
    (fun i module_name ->
      let deps = Dependency_graph.get_dependencies graph module_name in
      let dependents = Dependency_graph.find_dependents graph module_name in
      let path = Dependency_graph.get_module_path graph module_name in

      output_string out_channel "    {\n";
      output_string out_channel ("      \"name\": \"" ^ module_name ^ "\",\n");

      (* Add file path info if available *)
      (match path with
      | Some path_str ->
          output_string out_channel ("      \"path\": \"" ^ path_str ^ "\",\n")
      | None -> output_string out_channel "      \"path\": null,\n");

      (* Dependencies - modules this module depends on *)
      output_string out_channel "      \"dependencies\": [";

      if deps <> [] then (
        output_string out_channel "\n";
        List.iteri
          (fun j dep ->
            let dep_path = Dependency_graph.get_module_path graph dep in
            output_string out_channel "        {\n";
            output_string out_channel ("          \"name\": \"" ^ dep ^ "\"");

            (* Add dependency file path if available *)
            (match dep_path with
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
            let dependent_metadata =
              Dependency_graph.get_module_metadata graph dependent
            in
            output_string out_channel "        {\n";
            output_string out_channel
              ("          \"name\": \"" ^ dependent ^ "\"");

            (* Add dependent file path if available *)
            (match dependent_metadata with
            | { path = Some path; _ } ->
                output_string out_channel
                  (",\n          \"path\": \"" ^ path ^ "\"")
            | _ -> output_string out_channel ",\n          \"path\": null");

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

      (* Check for circular dependencies *)
      let is_in_cycle =
        List.exists
          (fun scc -> List.length scc > 1 && List.mem module_name scc)
          (Dependency_graph.find_strongly_connected_components graph)
      in
      output_string out_channel
        (",\n      \"in_cycle\": " ^ string_of_bool is_in_cycle);

      output_string out_channel "\n    }";
      if i < List.length modules - 1 then output_string out_channel ",\n"
      else output_string out_channel "\n")
    modules;

  output_string out_channel "  ],\n";

  (* Output cycles information *)
  let cycles = Dependency_graph.find_all_cycles graph in
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
  let metrics = Dependency_graph.calculate_metrics graph in
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
    if String.ends_with ~suffix:"." s then s ^ "0"
    (* Add 0 if ending with decimal point *) else s
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
