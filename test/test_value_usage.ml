open Stdlib
open Printf

(* Value usage counts (-vb) for the same sources compiled by ReScript 11 and
   ReScript 12. Reading a CMT file with the wrong typedtree layout used to
   crash the process, so every project must give the same counts. *)

let count_usage bs_dir ~module_name ~value_name ~value_line =
  Rescriptdep.Parser.clear_cache ();
  let graph =
    Rescriptdep.Parser.parse_files_or_dirs ~skip_cache:true [ bs_dir ]
    |> Rescriptdep.Dependency_graph.build_from_module_infos
  in
  let focused_graph =
    Rescriptdep.Dependency_graph.create_focused_graph graph module_name
  in
  Rescriptdep.Dependency_graph.count_value_usage_in_dependents focused_graph
    ~module_name ~value_name ~value_line:(Some value_line)

let format_counts counts =
  counts
  |> List.map (fun (name, count) -> sprintf "%s=%d" name count)
  |> String.concat ", "

(* [math_module] is the module defined by math.res (v11) / mathUtil.res (v12):
   ReScript 12 resolves "Math" to Stdlib.Math. *)
let test_project project ~math_module =
  let bs_dir =
    Test_support.require_existing_path
      ~hint:
        "Compiled ReScript fixture project not found. Run `make test` to \
         install the fixture dependencies and build `lib/bs` first."
      [
        Printf.sprintf "%s/lib/bs" project;
        Printf.sprintf "test/%s/lib/bs" project;
      ]
  in
  printf "=== Value usage in %s ===\n" project;
  let cases =
    [
      (* Top-level value used in its own module and through [open Utils] *)
      ("Utils", "multiply", 2, [ (math_module, 2); ("Utils", 4) ]);
      ("Utils", "add", 1, [ (math_module, 1); ("Utils", 0) ]);
      (* Value inside a submodule, used as U.triple *)
      ("Utils", "triple", 7, [ (math_module, 1); ("Utils", 0) ]);
      (* Value used from a JSX component module *)
      (math_module, "square", 3, [ ("App", 1); (math_module, 0) ]);
      (* Local binding inside a function body *)
      ("Utils", "divider", 12, [ (math_module, 0); ("Utils", 1) ]);
      ("Utils", "whitelist", 16, [ (math_module, 1); ("Utils", 1) ]);
    ]
  in
  List.for_all
    (fun (module_name, value_name, value_line, expected) ->
      let actual =
        count_usage bs_dir ~module_name ~value_name ~value_line
        |> List.sort compare
      in
      let expected = List.sort compare expected in
      if actual = expected then (
        printf "  %s.%s@%d: %s\n" module_name value_name value_line
          (format_counts actual);
        true)
      else (
        printf "  ERROR %s.%s@%d: expected %s, got %s\n" module_name value_name
          value_line (format_counts expected) (format_counts actual);
        false))
    cases

let () =
  let results =
    [
      ("rescript", "Math");
      ("rescript12/basic", "MathUtil");
      ("rescript12/namespaced", "MathUtil");
    ]
    |> List.map (fun (project, math_module) ->
        test_project project ~math_module)
  in
  exit (if List.for_all Fun.id results then 0 else 1)
