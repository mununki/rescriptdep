(* formatter.ml - Output formatters for dependency graphs
   This module handles formatting and outputting dependency graphs
   in various formats like DOT graphs and JSON. *)

(* Output format types *)
type format = 
  | Dot       (* Format as Graphviz DOT *)
  | Json      (* Format as JSON *)

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
  output_string out_channel "  node [shape=box, style=filled, fillcolor=lightblue];\n\n";
  
  (* Output nodes *)
  List.iter
    (fun module_name ->
       output_string out_channel ("  \"" ^ module_name ^ "\";\n"))
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
  if sccs <> [] then begin
    output_string out_channel "\n  /* Cycles */\n";
    List.iteri
      (fun i scc ->
         if List.length scc > 1 then begin
           output_string out_channel ("  subgraph cluster_" ^ string_of_int i ^ " {\n");
           output_string out_channel "    style=filled;\n";
           output_string out_channel "    color=pink;\n";
           output_string out_channel "    label=\"Cyclic dependency\";\n";
           List.iter
             (fun m -> output_string out_channel ("    \"" ^ m ^ "\";\n"))
             scc;
           output_string out_channel "  }\n";
         end)
      sccs
  end;
  
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
       output_string out_channel "    {\n";
       output_string out_channel ("      \"name\": \"" ^ module_name ^ "\",\n");
       output_string out_channel "      \"dependencies\": [";
       
       if deps <> [] then begin
         output_string out_channel "\n";
         List.iteri
           (fun j dep ->
              output_string out_channel ("        \"" ^ dep ^ "\"");
              if j < List.length deps - 1 then
                output_string out_channel ",\n"
              else
                output_string out_channel "\n")
           deps;
         output_string out_channel "      ";
       end;
       
       output_string out_channel "]";
       
       (* Get additional metrics *)
       let fan_in = List.length (Dependency_graph.find_dependents graph module_name) in
       output_string out_channel (",\n      \"fan_in\": " ^ string_of_int fan_in);
       output_string out_channel (",\n      \"fan_out\": " ^ string_of_int (List.length deps));
       
       (* Check for circular dependencies *)
       let is_in_cycle = 
         List.exists 
           (fun scc -> List.length scc > 1 && List.mem module_name scc)
           (Dependency_graph.find_strongly_connected_components graph)
       in
       output_string out_channel (",\n      \"in_cycle\": " ^ string_of_bool is_in_cycle);
       
       output_string out_channel "\n    }";
       if i < List.length modules - 1 then
         output_string out_channel ",\n"
       else
         output_string out_channel "\n")
    modules;
  
  output_string out_channel "  ],\n";
  
  (* Output cycles information *)
  let cycles = Dependency_graph.find_all_cycles graph in
  output_string out_channel "  \"cycles\": [";
  
  if cycles <> [] then begin
    output_string out_channel "\n";
    List.iteri
      (fun i cycle ->
         output_string out_channel "    [";
         List.iteri
           (fun j module_name ->
              output_string out_channel ("\"" ^ module_name ^ "\"");
              if j < List.length cycle - 1 then
                output_string out_channel ", ")
           cycle;
         output_string out_channel "]";
         if i < List.length cycles - 1 then
           output_string out_channel ",\n"
         else
           output_string out_channel "\n")
      cycles;
    output_string out_channel "  ";
  end;
  
  output_string out_channel "],\n";
  
  (* Calculate and output metrics *)
  let metrics = Dependency_graph.calculate_metrics graph in
  output_string out_channel "  \"metrics\": {\n";
  output_string out_channel ("    \"total_modules\": " ^ string_of_int (List.length modules) ^ ",\n");
  
  (* Calculate average fan-in and fan-out *)
  let total_fan_in, total_fan_out =
    List.fold_left
      (fun (in_acc, out_acc) (_, fan_in, fan_out) ->
         (in_acc + fan_in, out_acc + fan_out))
      (0, 0)
      metrics
  in
  
  let avg_fan_in = float_of_int total_fan_in /. float_of_int (max 1 (List.length modules)) in
  let avg_fan_out = float_of_int total_fan_out /. float_of_int (max 1 (List.length modules)) in
  
  let format_float f =
    let s = string_of_float f in
    if String.ends_with ~suffix:"." s then
      s ^ "0"  (* 소수점으로 끝나면 0 추가 *)
    else
      s
  in
  
  output_string out_channel ("    \"average_fan_in\": " ^ format_float avg_fan_in ^ ",\n");
  output_string out_channel ("    \"average_fan_out\": " ^ format_float avg_fan_out ^ ",\n");
  
  (* Find module with highest fan-in (most depended upon) *)
  let max_fan_in_module, max_fan_in, _ =
    List.fold_left
      (fun (max_m, max_in, max_out) (m, fan_in, fan_out) ->
         if fan_in > max_in then (m, fan_in, fan_out) else (max_m, max_in, max_out))
      ("", 0, 0)
      metrics
  in
  
  (* Find module with highest fan-out (depends on most) *)
  let max_fan_out_module, _, max_fan_out =
    List.fold_left
      (fun (max_m, max_in, max_out) (m, fan_in, fan_out) ->
         if fan_out > max_out then (m, fan_in, fan_out) else (max_m, max_in, max_out))
      ("", 0, 0)
      metrics
  in
  
  output_string out_channel ("    \"most_depended_upon\": {\"module\": \"" ^
                             max_fan_in_module ^ "\", \"count\": " ^
                             string_of_int max_fan_in ^ "},\n");
  
  output_string out_channel ("    \"most_dependencies\": {\"module\": \"" ^
                             max_fan_out_module ^ "\", \"count\": " ^
                             string_of_int max_fan_out ^ "},\n");
  
  output_string out_channel ("    \"cycles_count\": " ^ string_of_int (List.length cycles) ^ "\n");
  output_string out_channel "  }\n";
  
  (* JSON closing *)
  output_string out_channel "}\n"

(* String representation of a format *)
let format_to_string = function
  | Dot -> "dot"
  | Json -> "json"

(* Convert string to format *)
let format_of_string = function
  | "dot" -> Some Dot
  | "json" -> Some Json
  | _ -> None

(* Get all supported formats *)
let all_formats = [Dot; Json]

(* Get the default format *)
let default_format = Dot