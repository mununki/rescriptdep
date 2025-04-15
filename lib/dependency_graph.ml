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

(* Find modules with no dependents *)
let find_modules_with_no_dependents graph =
  let modules = get_modules graph in
  List.filter (fun m -> find_dependents graph m = []) modules

(* Count value usage in dependents of a module *)
let count_value_usage_in_dependents graph ~module_name ~value_name =
  let open Stdlib in
  let open Cmt_format in
  let dependents = find_dependents graph module_name in
  let find_cmt_path file_path =
    let cmt_path =
      Parser.DependencyExtractor.get_cmt_path_for_source file_path
    in
    if Sys.file_exists cmt_path then Some cmt_path else None
  in
  let rec get_head_module_name path =
    match path with
    | Path.Pident id -> Ident.name id
    | Path.Pdot (p, _, _) -> get_head_module_name p
    | _ -> Path.name path
  in
  let rec count_in_expression value_name module_name open_modules current_module
      expr =
    match expr.Typedtree.exp_desc with
    | Typedtree.Texp_ident (path, _, _) -> (
        match path with
        | Path.Pident id ->
            if
              Ident.name id = value_name
              && (List.mem module_name open_modules
                 || current_module = module_name)
            then 1
            else 0
        | Path.Pdot (p, id, _) ->
            if id = value_name && get_head_module_name p = module_name then 1
            else 0
        | _ -> 0)
    | Typedtree.Texp_let (_, vbs, e) ->
        List.fold_left
          (fun acc vb ->
            acc
            + count_in_expression value_name module_name open_modules
                current_module vb.Typedtree.vb_expr)
          (count_in_expression value_name module_name open_modules
             current_module e)
          vbs
    | Typedtree.Texp_function { cases; _ } ->
        List.fold_left
          (fun acc c ->
            acc
            + count_in_expression value_name module_name open_modules
                current_module c.Typedtree.c_rhs)
          0 cases
    | Typedtree.Texp_apply (e, args) ->
        List.fold_left
          (fun acc (_, eo) ->
            acc
            +
            match eo with
            | Some e ->
                count_in_expression value_name module_name open_modules
                  current_module e
            | None -> 0)
          (count_in_expression value_name module_name open_modules
             current_module e)
          args
    | Typedtree.Texp_match (e, cases, cases2, _) ->
        count_in_expression value_name module_name open_modules current_module e
        + List.fold_left
            (fun acc c ->
              acc
              + count_in_expression value_name module_name open_modules
                  current_module c.Typedtree.c_rhs)
            0 (cases @ cases2)
    | Typedtree.Texp_tuple elist | Typedtree.Texp_array elist ->
        List.fold_left
          (fun acc e ->
            acc
            + count_in_expression value_name module_name open_modules
                current_module e)
          0 elist
    | Typedtree.Texp_construct (_, _, elist) ->
        List.fold_left
          (fun acc e ->
            acc
            + count_in_expression value_name module_name open_modules
                current_module e)
          0 elist
    | Typedtree.Texp_variant (_, eo) -> (
        match eo with
        | Some e ->
            count_in_expression value_name module_name open_modules
              current_module e
        | None -> 0)
    | Typedtree.Texp_record { fields; extended_expression; _ } -> (
        let acc =
          Array.fold_left
            (fun acc (_, fld) ->
              match fld with
              | Typedtree.Overridden (_, e) ->
                  acc
                  + count_in_expression value_name module_name open_modules
                      current_module e
              | Typedtree.Kept _ -> acc)
            0 fields
        in
        match extended_expression with
        | Some e ->
            acc
            + count_in_expression value_name module_name open_modules
                current_module e
        | None -> acc)
    | Typedtree.Texp_field (e, _, _) ->
        count_in_expression value_name module_name open_modules current_module e
    | Typedtree.Texp_setfield (e1, _, _, e2) ->
        count_in_expression value_name module_name open_modules current_module
          e1
        + count_in_expression value_name module_name open_modules current_module
            e2
    | Typedtree.Texp_ifthenelse (e1, e2, eo) -> (
        count_in_expression value_name module_name open_modules current_module
          e1
        + count_in_expression value_name module_name open_modules current_module
            e2
        +
        match eo with
        | Some e ->
            count_in_expression value_name module_name open_modules
              current_module e
        | None -> 0)
    | Typedtree.Texp_sequence (e1, e2) ->
        count_in_expression value_name module_name open_modules current_module
          e1
        + count_in_expression value_name module_name open_modules current_module
            e2
    | Typedtree.Texp_while (e1, e2) ->
        count_in_expression value_name module_name open_modules current_module
          e1
        + count_in_expression value_name module_name open_modules current_module
            e2
    | Typedtree.Texp_for (_, _, e1, e2, _, e3) ->
        count_in_expression value_name module_name open_modules current_module
          e1
        + count_in_expression value_name module_name open_modules current_module
            e2
        + count_in_expression value_name module_name open_modules current_module
            e3
    | Typedtree.Texp_send (e, _, eo) -> (
        count_in_expression value_name module_name open_modules current_module e
        +
        match eo with
        | Some e ->
            count_in_expression value_name module_name open_modules
              current_module e
        | None -> 0)
    | Typedtree.Texp_open (open_decl, e) ->
        let open_mod =
          match open_decl.open_expr.mod_desc with
          | Typedtree.Tmod_ident (path, _) -> Path.name path
          | _ -> ""
        in
        count_in_expression value_name module_name (open_mod :: open_modules)
          current_module e
    | _ -> 0
  in
  let rec count_in_structure value_name module_name current_module structure =
    List.fold_left
      (fun acc item ->
        match item.Typedtree.str_desc with
        | Typedtree.Tstr_value (_, vbs) ->
            acc
            + List.fold_left
                (fun acc vb ->
                  acc
                  + count_in_expression value_name module_name [] current_module
                      vb.Typedtree.vb_expr)
                0 vbs
        | Typedtree.Tstr_eval (e, _) ->
            acc + count_in_expression value_name module_name [] current_module e
        | Typedtree.Tstr_module { mb_expr = { mod_desc; _ }; _ } -> (
            match mod_desc with
            | Typedtree.Tmod_structure s ->
                acc + count_in_structure value_name module_name current_module s
            | Typedtree.Tmod_constraint (mexpr, _, _, _) -> (
                match mexpr.mod_desc with
                | Typedtree.Tmod_structure s ->
                    acc
                    + count_in_structure value_name module_name current_module s
                | _ -> acc)
            | _ -> acc)
        | _ -> acc)
      0 structure.Typedtree.str_items
  in
  List.map
    (fun dep ->
      let file_path_opt = get_module_path graph dep in
      if Option.is_some file_path_opt then
        let file_path = Option.get file_path_opt in
        let cmt_path_opt = find_cmt_path file_path in
        if Option.is_some cmt_path_opt then
          let cmt_path = Option.get cmt_path_opt in
          try
            let cmt_info = Cmt_format.read_cmt cmt_path in
            match cmt_info.cmt_annots with
            | Implementation structure ->
                let count =
                  count_in_structure value_name module_name dep structure
                in
                (dep, count)
            | _ -> (dep, -2)
            (* -2: No implementation AST *)
          with _ -> (dep, -3) (* -3: CMT read error *)
        else (dep, -4) (* -4: No .cmt file found *)
      else (dep, -5)
      (* -5: No file path found *))
    dependents
