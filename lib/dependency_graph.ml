(* dependency_graph.ml - Graph representation of module dependencies
   This module builds and analyzes dependency graphs between modules. *)

(* Create a string map module *)
module StringMap = Map.Make (String)

(* Module metadata type *)
type module_metadata = { path : string option }

(* Graph representation including module metadata *)
type t = {
  dependencies : string list StringMap.t;
  metadata : module_metadata StringMap.t;
}

(* Empty graph *)
let empty = { dependencies = StringMap.empty; metadata = StringMap.empty }

(* Add a module and its dependencies to the graph *)
let add graph module_name dependencies path =
  let metadata = { path } in
  {
    dependencies = StringMap.add module_name dependencies graph.dependencies;
    metadata = StringMap.add module_name metadata graph.metadata;
  }

(* Get module metadata map *)
let get_metadata_map graph = graph.metadata

(* Get module metadata record for a module *)
let get_module_metadata graph module_name =
  try StringMap.find module_name graph.metadata
  with Not_found -> { path = None }

(* Get module path *)
let get_module_path graph module_name =
  let metadata = get_module_metadata graph module_name in
  metadata.path

(* Build a graph from a list of module_info structures *)
let build_from_module_infos infos =
  List.fold_left
    (fun graph info ->
      add graph info.Parser.name info.Parser.dependencies info.Parser.file_path)
    empty infos

(* Get the dependencies of a module *)
let get_dependencies graph module_name =
  try StringMap.find module_name graph.dependencies with Not_found -> []

(* Get all module names in the graph *)
let get_modules graph = StringMap.bindings graph.dependencies |> List.map fst

(* Find direct dependents of a module (modules that depend on it) *)
let find_dependents graph module_name =
  StringMap.fold
    (fun m deps acc -> if List.mem module_name deps then m :: acc else acc)
    graph.dependencies []

(* Check if a cycle exists in the dependency graph starting from a module *)
let has_cycle graph start_module =
  let visited = Hashtbl.create (StringMap.cardinal graph.dependencies) in
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
  (* Use a hash table to memoize visited modules and their cycles *)
  let visited = Hashtbl.create (StringMap.cardinal graph.dependencies) in
  let cycle_cache = Hashtbl.create (StringMap.cardinal graph.dependencies) in
  let result = ref [] in

  let rec check_cycle path module_name =
    (* Check if this module is part of a cycle we've already processed *)
    if Hashtbl.mem cycle_cache module_name then
      Hashtbl.find cycle_cache module_name
    else if List.mem module_name path then (
      (* Cycle detected - extract just the cycle part *)
      let cycle_start =
        let rec find_index i = function
          | [] -> 0
          | x :: _ when x = module_name -> i
          | _ :: xs -> find_index (i + 1) xs
        in
        find_index 0 path
      in
      let cycle =
        let rec take_until n = function
          | [] -> []
          | x :: _ when n = 0 -> []
          | x :: xs -> x :: take_until (n - 1) xs
        in
        module_name :: take_until cycle_start path |> List.rev
      in
      Hashtbl.add cycle_cache module_name (Some cycle);
      Some cycle)
    else if Hashtbl.mem visited module_name then
      (* Already visited, no cycle through this path *)
      None
    else (
      Hashtbl.add visited module_name true;
      let new_path = module_name :: path in
      let deps = get_dependencies graph module_name in

      (* Sort dependencies to ensure consistent cycle detection *)
      let sorted_deps = List.sort String.compare deps in

      let cycle_result =
        try List.find_map (fun dep -> check_cycle new_path dep) sorted_deps
        with Not_found -> None
      in

      Hashtbl.add cycle_cache module_name cycle_result;
      cycle_result)
  in

  (* Create a set to track unique cycles *)
  let module CycleSet = Set.Make (struct
    type t = string list

    let compare = compare
  end) in
  let unique_cycles = ref CycleSet.empty in

  let modules = get_modules graph in
  List.iter
    (fun module_name ->
      if not (Hashtbl.mem visited module_name) then
        match check_cycle [] module_name with
        | Some cycle ->
            (* Only add if this exact cycle hasn't been seen before *)
            if not (CycleSet.mem cycle !unique_cycles) then (
              unique_cycles := CycleSet.add cycle !unique_cycles;
              result := cycle :: !result)
        | None -> ())
    modules;

  !result

(* Topological sort of modules (dependencies first) *)
let topological_sort graph =
  let visited = Hashtbl.create (StringMap.cardinal graph.dependencies) in
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
  let visited = Hashtbl.create (StringMap.cardinal graph.dependencies) in
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

(* Create a subgraph containing only specified modules but preserving all their dependencies *)
let create_subgraph_preserve_deps graph modules =
  let module StringSet = Set.Make (String) in
  let module_set = StringSet.of_list modules in

  StringMap.filter (fun m _ -> StringSet.mem m module_set) graph.dependencies

(* Find the strongly connected components (groups of modules with cycles) *)
let find_strongly_connected_components graph =
  (* Implementation of Tarjan's algorithm *)
  let index = ref 0 in
  let stack = ref [] in
  let indices = Hashtbl.create (StringMap.cardinal graph.dependencies) in
  let lowlinks = Hashtbl.create (StringMap.cardinal graph.dependencies) in
  let on_stack = Hashtbl.create (StringMap.cardinal graph.dependencies) in
  let sccs = ref [] in

  let rec strong_connect v =
    Hashtbl.add indices v !index;
    Hashtbl.add lowlinks v !index;
    index := !index + 1;
    stack := v :: !stack;
    Hashtbl.add on_stack v true;

    let deps = try StringMap.find v graph.dependencies with Not_found -> [] in
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

(* Create a filtered graph that excludes standard and internal modules *)
let create_filtered_graph graph =
  (* Get all modules from the graph *)
  let all_modules = get_modules graph in

  (* Filter out standard modules from the graph nodes *)
  let project_modules =
    List.filter
      (fun m -> not (Parse_utils.is_stdlib_or_internal_module m))
      all_modules
  in

  (* Filter graph nodes (keep only project modules), but preserve all their dependencies *)
  let filtered_deps = create_subgraph_preserve_deps graph project_modules in

  (* Create a new graph with the filtered dependencies and original metadata *)
  { dependencies = filtered_deps; metadata = graph.metadata }

(* Create a focused graph centered around a specific module *)
let create_focused_graph graph center_module =
  (* Check if the module exists *)
  if not (StringMap.mem center_module graph.dependencies) then
    (* Return empty graph if module doesn't exist *)
    { dependencies = StringMap.empty; metadata = StringMap.empty }
  else
    (* 1. Get the center module dependencies *)
    let center_deps = get_dependencies graph center_module in

    (* 2. Get modules that depend on the center module (its dependents) *)
    let dependents = find_dependents graph center_module in

    (* 3. Start building a new graph with just the center module *)
    (* Preserve metadata - add center module *)
    let center_metadata = get_module_metadata graph center_module in
    let result =
      {
        dependencies = StringMap.singleton center_module center_deps;
        metadata = StringMap.singleton center_module center_metadata;
      }
    in

    (* 4. Add dependency modules and their metadata *)
    let result =
      List.fold_left
        (fun acc dep ->
          if StringMap.mem dep graph.dependencies then
            (* In a focused graph, we only care about dependencies of the center module,
               not dependencies of dependencies, so we use empty list for deps *)
            let dep_metadata = get_module_metadata graph dep in
            add acc dep [] dep_metadata.path
          else acc)
        result center_deps
    in

    (* 5. Add dependent modules to the graph but only with edges to the center module *)
    (* This ensures that when we call find_dependents on the center module, it returns
       these modules, but they won't show their own dependencies *)
    let result =
      List.fold_left
        (fun acc dependent ->
          if
            (not (StringMap.mem dependent acc.dependencies))
            && dependent <> center_module
          then
            (* Add the dependent with just one dependency - the center module *)
            let dependent_metadata = get_module_metadata graph dependent in
            add acc dependent [ center_module ] dependent_metadata.path
          else acc)
        result dependents
    in

    result
