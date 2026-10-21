(* dependency_graph.ml - Graph representation of module dependencies
   This module builds and analyzes dependency graphs between modules. *)

(* Create a string map module *)
module StringMap = Map.Make (String)

(* Module metadata type *)
type module_metadata = {
  path : string option; (* Source file *)
  cmt_path : string option; (* CMT file the module was read from *)
}

(* Graph representation including module metadata *)
type t = {
  dependencies : string list StringMap.t;
  dependents : string list StringMap.t;
  metadata : module_metadata StringMap.t;
}

(* Empty graph *)
let empty =
  {
    dependencies = StringMap.empty;
    dependents = StringMap.empty;
    metadata = StringMap.empty;
  }

let add_dependent dependents dependency module_name =
  let current =
    try StringMap.find dependency dependents with Not_found -> []
  in
  let updated =
    if List.mem module_name current then current else module_name :: current
  in
  StringMap.add dependency updated dependents

let remove_dependent dependents dependency module_name =
  match StringMap.find_opt dependency dependents with
  | None -> dependents
  | Some current ->
      let updated = List.filter (fun m -> m <> module_name) current in
      if updated = [] then StringMap.remove dependency dependents
      else StringMap.add dependency updated dependents

let build_dependents dependencies =
  StringMap.fold
    (fun module_name deps dependents ->
      List.fold_left
        (fun acc dep -> add_dependent acc dep module_name)
        dependents deps)
    dependencies StringMap.empty

let make dependencies metadata =
  { dependencies; dependents = build_dependents dependencies; metadata }

(* Add a module and its dependencies to the graph *)
let add ?cmt_path graph module_name dependencies path =
  let metadata = { path; cmt_path } in
  let dependencies = List.sort_uniq String.compare dependencies in
  let dependents =
    match StringMap.find_opt module_name graph.dependencies with
    | None -> graph.dependents
    | Some old_dependencies ->
        List.fold_left
          (fun acc dep -> remove_dependent acc dep module_name)
          graph.dependents old_dependencies
  in
  let dependents =
    List.fold_left
      (fun acc dep -> add_dependent acc dep module_name)
      dependents dependencies
  in
  {
    dependencies = StringMap.add module_name dependencies graph.dependencies;
    dependents;
    metadata = StringMap.add module_name metadata graph.metadata;
  }

(* Get module metadata map *)
let get_metadata_map graph = graph.metadata

(* Get module metadata record for a module *)
let get_module_metadata graph module_name =
  try StringMap.find module_name graph.metadata
  with Not_found -> { path = None; cmt_path = None }

(* Get module path *)
let get_module_path graph module_name =
  let metadata = get_module_metadata graph module_name in
  metadata.path

(* Build a graph from a list of module_info structures *)
let build_from_module_infos infos =
  List.fold_left
    (fun graph info ->
      add ?cmt_path:info.Parser.cmt_path graph info.Parser.name
        info.Parser.dependencies info.Parser.file_path)
    empty infos

(* Get the dependencies of a module *)
let get_dependencies graph module_name =
  try StringMap.find module_name graph.dependencies with Not_found -> []

(* Get all module names in the graph *)
let get_modules graph = StringMap.bindings graph.dependencies |> List.map fst

(* Find direct dependents of a module (modules that depend on it) *)
let find_dependents graph module_name =
  match StringMap.find_opt module_name graph.dependents with
  | Some dependents -> List.sort_uniq String.compare dependents
  | None -> []

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
          | _ :: _ when n = 0 -> []
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
  make filtered_deps graph.metadata

(* Create a focused graph centered around a specific module *)
let create_focused_graph graph center_module =
  (* Check if the module exists *)
  if not (StringMap.mem center_module graph.dependencies) then
    (* Return empty graph if module doesn't exist *)
    empty
  else
    (* 1. Get the center module dependencies *)
    let center_deps = get_dependencies graph center_module in

    (* 2. Get modules that depend on the center module (its dependents) *)
    let dependents =
      List.sort_uniq String.compare
        (center_module :: find_dependents graph center_module)
    in

    (* 3. Start building a new graph with just the center module *)
    (* Preserve metadata - add center module *)
    let center_metadata = get_module_metadata graph center_module in
    let result =
      add ?cmt_path:center_metadata.cmt_path empty center_module center_deps
        center_metadata.path
    in

    (* 4. Add dependency modules and their metadata *)
    let result =
      List.fold_left
        (fun acc dep ->
          if StringMap.mem dep graph.dependencies then
            (* In a focused graph, we only care about dependencies of the center module,
               not dependencies of dependencies, so we use empty list for deps *)
            let dep_metadata = get_module_metadata graph dep in
            add ?cmt_path:dep_metadata.cmt_path acc dep [] dep_metadata.path
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
            add ?cmt_path:dependent_metadata.cmt_path acc dependent
              [ center_module ] dependent_metadata.path
          else acc)
        result dependents
    in

    result

(* Find modules with no dependents *)
let find_modules_with_no_dependents graph =
  let modules = get_modules graph in
  List.filter (fun m -> find_dependents graph m = []) modules

(* Find the let binding [value_name] declared at [value_line]. Returns the
   module path of the binding inside its file, whether it is a top-level
   binding, and for a local binding the top-level expression enclosing it. *)
let find_let_binding_at_line_with_path value_name value_line items =
  let found = ref None in
  let is_target (binding : Typed_ast.binding) =
    binding.name = Some value_name && binding.line = value_line
  in
  let rec search_items path items =
    List.iter
      (function
        | Typed_ast.Values bindings ->
            List.iter
              (fun (binding : Typed_ast.binding) ->
                if is_target binding then
                  found := Some (List.rev path, true, None);
                search_expr path binding.expr binding.expr)
              bindings
        | Typed_ast.Module (name, items) -> search_items (name :: path) items
        | Typed_ast.Include items -> search_items path items
        | Typed_ast.Eval expr -> search_expr path expr expr)
      items
  and search_local_bindings path parent bindings =
    List.iter
      (fun (binding : Typed_ast.binding) ->
        if is_target binding then
          found := Some (List.rev path, false, Some parent);
        search_expr path parent binding.expr)
      bindings
  and search_local_items path parent items =
    List.iter
      (function
        | Typed_ast.Values bindings ->
            search_local_bindings path parent bindings
        | Typed_ast.Module (_, items) | Typed_ast.Include items ->
            search_local_items path parent items
        | Typed_ast.Eval expr -> search_expr path parent expr)
      items
  and search_expr path parent = function
    | Typed_ast.Ident _ -> ()
    | Typed_ast.Let (bindings, body) ->
        search_local_bindings path parent bindings;
        search_expr path parent body
    | Typed_ast.Struct items -> search_local_items path parent items
    | Typed_ast.Node children -> List.iter (search_expr path parent) children
  in
  search_items [] items;
  !found

let rec count_in_expression matches = function
  | Typed_ast.Ident path -> if matches path then 1 else 0
  | Typed_ast.Let (bindings, body) ->
      List.fold_left
        (fun acc (binding : Typed_ast.binding) ->
          acc + count_in_expression matches binding.expr)
        (count_in_expression matches body)
        bindings
  | Typed_ast.Struct items -> count_in_items matches items
  | Typed_ast.Node children ->
      List.fold_left
        (fun acc expr -> acc + count_in_expression matches expr)
        0 children

and count_in_items matches items =
  List.fold_left
    (fun acc item ->
      acc
      +
      match item with
      | Typed_ast.Values bindings ->
          List.fold_left
            (fun acc (binding : Typed_ast.binding) ->
              acc + count_in_expression matches binding.expr)
            0 bindings
      | Typed_ast.Module (_, items) | Typed_ast.Include items ->
          count_in_items matches items
      | Typed_ast.Eval expr -> count_in_expression matches expr)
    0 items

(* The namespace of a compilation unit, e.g. "App-MyNs" -> Some "MyNs" *)
let namespace_of_modname modname =
  match String.rindex_opt modname '-' with
  | Some idx when idx + 1 < String.length modname ->
      Some (String.sub modname (idx + 1) (String.length modname - idx - 1))
  | _ -> None

(* Namespaced builds name compilation units "Utils-MyNs" and may reach them
   through the namespace module "MyNs", so normalize the module part of a path
   before comparing it with graph module names. *)
let normalize_value_path ~namespace path =
  let path =
    match (path, namespace) with
    | ns :: (_ :: _ :: _ as rest), Some namespace when ns = namespace -> rest
    | _ -> path
  in
  match path with
  | m :: (_ :: _ as rest) -> Parse_utils.normalize_module_name m :: rest
  | _ -> path

(* Build the predicate that decides whether a resolved value path refers to the
   binding being counted. [value_path] is the submodule path of the binding
   inside [target_module]. *)
let value_path_matcher ~namespace ~local ~in_target_module ~target_module
    ~value_path ~value_name =
  let qualified = value_path @ [ value_name ] in
  fun path ->
    let path = normalize_value_path ~namespace path in
    if local then path = [ value_name ]
    else
      match path with
      | m :: rest when m = target_module && rest = qualified -> true
      | _ -> in_target_module && (path = [ value_name ] || path = qualified)

let find_cmt_path metadata =
  match metadata.cmt_path with
  | Some cmt_path when Sys.file_exists cmt_path -> Some cmt_path
  | _ -> (
      match metadata.path with
      | Some file_path ->
          let cmt_path =
            Parser.DependencyExtractor.get_cmt_path_for_source file_path
          in
          if Sys.file_exists cmt_path then Some cmt_path else None
      | None -> None)

let read_typed_implementation cmt_path =
  let cmt_info = Cmt_format.read_cmt cmt_path in
  ( Typed_ast.implementation cmt_info,
    namespace_of_modname cmt_info.Cmt_format.cmt_modname )

(* Count value usage in dependents of a module (and in the module itself).
   Negative counts report why a module could not be analyzed:
   -2 no implementation, -3 unreadable CMT, -4 no CMT file, -5 no file path. *)
let count_value_usage_in_dependents graph ~module_name ~value_name ~value_line =
  let dependents =
    List.sort_uniq String.compare
      (module_name :: find_dependents graph module_name)
  in
  (* Locate the binding in the target module to learn its submodule path and
     whether it is a top-level binding *)
  let target_binding =
    match value_line with
    | None -> None
    | Some line -> (
        match find_cmt_path (get_module_metadata graph module_name) with
        | Some cmt_path -> (
            try
              match read_typed_implementation cmt_path with
              | Some items, _ ->
                  find_let_binding_at_line_with_path value_name line items
              | None, _ -> None
            with _ -> None)
        | None -> None)
  in
  List.map
    (fun dep ->
      let metadata = get_module_metadata graph dep in
      match find_cmt_path metadata with
      | None ->
          if metadata.path = None && metadata.cmt_path = None then (dep, -5)
          else (dep, -4)
      | Some cmt_path -> (
          try
            match read_typed_implementation cmt_path with
            | None, _ -> (dep, -2)
            | Some items, namespace ->
                let in_target_module = dep = module_name in
                let count_top_level value_path =
                  count_in_items
                    (value_path_matcher ~namespace ~local:false
                       ~in_target_module ~target_module:module_name ~value_path
                       ~value_name)
                    items
                in
                let count =
                  match (value_line, target_binding) with
                  | None, _ -> count_top_level []
                  | Some _, None ->
                      if in_target_module then 0 else count_top_level []
                  | Some _, Some (value_path, true, _) ->
                      count_top_level value_path
                  | Some _, Some (_, false, parent_expr) -> (
                      (* A local binding can only be used inside the top-level
                         expression that encloses it *)
                      match parent_expr with
                      | Some expr when in_target_module ->
                          count_in_expression
                            (value_path_matcher ~namespace ~local:true
                               ~in_target_module ~target_module:module_name
                               ~value_path:[] ~value_name)
                            expr
                      | _ -> 0)
                in
                (dep, count)
          with _ -> (dep, -3)))
    dependents
