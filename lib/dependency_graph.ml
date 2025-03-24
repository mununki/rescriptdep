(* dependency_graph.ml - Graph representation of module dependencies
   This module builds and analyzes dependency graphs between modules. *)

(* Create a string map module *)
module StringMap = Map.Make (String)

(* Graph representation as an adjacency list *)
type t = string list StringMap.t

(* Empty graph *)
let empty = StringMap.empty

(* Add a module and its dependencies to the graph *)
let add graph module_name dependencies =
  StringMap.add module_name dependencies graph

(* Build a graph from a list of module_info structures *)
let build_from_module_infos infos =
  List.fold_left
    (fun graph info -> add graph info.Parser.name info.Parser.dependencies)
    empty infos

(* Get the dependencies of a module *)
let get_dependencies graph module_name =
  try StringMap.find module_name graph with Not_found -> []

(* Get all module names in the graph *)
let get_modules graph = StringMap.bindings graph |> List.map fst

(* Find direct dependents of a module (modules that depend on it) *)
let find_dependents graph module_name =
  StringMap.fold
    (fun m deps acc -> if List.mem module_name deps then m :: acc else acc)
    graph []

(* Check if a cycle exists in the dependency graph starting from a module *)
let has_cycle graph start_module =
  let visited = Hashtbl.create (StringMap.cardinal graph) in
  let rec visit path module_name =
    if List.mem module_name path then
      (* Cycle detected *)
      Some
        (List.rev
           (module_name
           :: List.filter (fun m -> m = module_name || List.hd path <> m) path))
    else if Hashtbl.mem visited module_name then
      (* Already visited, no cycle through this path *)
      None
    else (
      Hashtbl.add visited module_name true;
      let new_path = module_name :: path in
      let deps = get_dependencies graph module_name in
      try List.find_map (fun dep -> visit new_path dep) deps
      with Not_found -> None)
  in
  visit [] start_module

(* Find all cycles in the dependency graph *)
let find_all_cycles graph =
  let modules = get_modules graph in
  List.fold_left
    (fun acc module_name ->
      match has_cycle graph module_name with
      | Some cycle when not (List.exists (fun c -> c = cycle) acc) ->
          cycle :: acc
      | _ -> acc)
    [] modules

(* Topological sort of modules (dependencies first) *)
let topological_sort graph =
  let visited = Hashtbl.create (StringMap.cardinal graph) in
  let result = ref [] in

  let rec visit module_name =
    if not (Hashtbl.mem visited module_name) then (
      Hashtbl.add visited module_name true;
      let deps = get_dependencies graph module_name in
      List.iter visit deps;
      result := module_name :: !result)
  in

  let modules = get_modules graph in
  List.iter visit modules;
  List.rev !result

(* Get transitive dependencies of a module *)
let transitive_dependencies graph module_name =
  let visited = Hashtbl.create (StringMap.cardinal graph) in
  let result = ref [] in

  let rec visit m =
    if not (Hashtbl.mem visited m) then (
      Hashtbl.add visited m true;
      if m <> module_name then result := m :: !result;
      let deps = get_dependencies graph m in
      List.iter visit deps)
  in

  visit module_name;
  List.rev !result

(* Create a subgraph containing only specified modules *)
let create_subgraph graph modules =
  let module StringSet = Set.Make (String) in
  let module_set = StringSet.of_list modules in

  StringMap.filter (fun m _ -> StringSet.mem m module_set) graph
  |> StringMap.map (fun deps ->
         List.filter (fun dep -> StringSet.mem dep module_set) deps)

(* Trim the graph to only include modules reachable from the specified roots *)
let trim_to_reachable graph roots =
  let reachable = Hashtbl.create (StringMap.cardinal graph) in

  let rec mark_reachable m =
    if not (Hashtbl.mem reachable m) then (
      Hashtbl.add reachable m true;
      try
        let deps = StringMap.find m graph in
        List.iter mark_reachable deps
      with Not_found -> ())
  in

  List.iter mark_reachable roots;

  let reachable_modules = Hashtbl.fold (fun m _ acc -> m :: acc) reachable [] in

  create_subgraph graph reachable_modules

(* Find the strongly connected components (groups of modules with cycles) *)
let find_strongly_connected_components graph =
  (* Implementation of Tarjan's algorithm *)
  let index = ref 0 in
  let stack = ref [] in
  let indices = Hashtbl.create (StringMap.cardinal graph) in
  let lowlinks = Hashtbl.create (StringMap.cardinal graph) in
  let on_stack = Hashtbl.create (StringMap.cardinal graph) in
  let sccs = ref [] in

  let rec strong_connect v =
    Hashtbl.add indices v !index;
    Hashtbl.add lowlinks v !index;
    index := !index + 1;
    stack := v :: !stack;
    Hashtbl.add on_stack v true;

    let deps = try StringMap.find v graph with Not_found -> [] in
    List.iter
      (fun w ->
        if not (Hashtbl.mem indices w) then (
          strong_connect w;
          Hashtbl.replace lowlinks v
            (min (Hashtbl.find lowlinks v) (Hashtbl.find lowlinks w)))
        else if Hashtbl.mem on_stack w then
          Hashtbl.replace lowlinks v
            (min (Hashtbl.find lowlinks v) (Hashtbl.find indices w)))
      deps;

    if Hashtbl.find lowlinks v = Hashtbl.find indices v then
      let rec pop_scc acc =
        match !stack with
        | [] -> acc
        | hd :: tl ->
            stack := tl;
            Hashtbl.remove on_stack hd;
            if hd = v then hd :: acc else pop_scc (hd :: acc)
      in
      let scc = pop_scc [] in
      if
        List.length scc > 1
        || List.length scc = 1
           &&
           let v = List.hd scc in
           List.mem v (get_dependencies graph v)
      then sccs := scc :: !sccs
  in

  List.iter
    (fun v -> if not (Hashtbl.mem indices v) then strong_connect v)
    (get_modules graph);

  !sccs

(* Calculate module metrics: fan-in (dependents) and fan-out (dependencies) *)
let calculate_metrics graph =
  let modules = get_modules graph in

  List.map
    (fun m ->
      let fan_out = List.length (get_dependencies graph m) in
      let fan_in = List.length (find_dependents graph m) in
      (m, fan_in, fan_out))
    modules

(* Create a focused graph centered around a specific module *)
let create_focused_graph graph center_module =
  (* Check if the module exists *)
  if not (StringMap.mem center_module graph) then
    (* Return empty graph if module doesn't exist *)
    StringMap.empty
  else
    (* 1. The center module itself *)
    let result = StringMap.empty in

    (* 2. Add the center module and its direct dependencies *)
    let center_deps =
      try StringMap.find center_module graph with Not_found -> []
    in
    let result = StringMap.add center_module center_deps result in

    (* 3. Find modules that directly depend on the center module *)
    let direct_dependents =
      StringMap.fold
        (fun m deps acc ->
          if List.mem center_module deps then (m, deps) :: acc else acc)
        graph []
    in

    (* 4. Add each dependent module with ONLY its dependency on the center module *)
    let result =
      List.fold_left
        (fun acc (m, deps) ->
          (* Filter the original dependencies to keep only the center module *)
          let filtered_deps =
            List.filter (fun dep -> dep = center_module) deps
          in
          StringMap.add m filtered_deps acc)
        result direct_dependents
    in

    result
