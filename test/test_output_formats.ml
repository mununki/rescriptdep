open Stdlib
open Printf

(* Golden tests for the CLI output formats on the fixture projects: the full
   graph in DOT, the focused graph and the modules without dependents in DOT
   and JSON, and a value usage count in JSON. The full JSON graph is covered
   by test_rescriptdep. Paths are made repository-relative before comparing. *)

let run_rescriptdep args =
  let output = Filename.temp_file "rescriptdep_" ".out" in
  let cmd =
    sprintf "dune exec -- rescriptdep %s -o %s"
      (String.concat " " (List.map Filename.quote args))
      (Filename.quote output)
  in
  (Sys.command cmd, output)

let find_bs_dir project =
  Test_support.require_existing_path
    ~hint:
      "Compiled ReScript fixture project not found. Run `make test` to install \
       the fixture dependencies and build `lib/bs` first."
    [ sprintf "%s/lib/bs" project; sprintf "test/%s/lib/bs" project ]

(* Reference file suffix and CLI arguments of each golden output. [focus] is
   the module defined by math.res (v11) / mathUtil.res (v12, v13). *)
let golden_outputs ~focus =
  [
    (".dot", [ "-f"; "dot" ]);
    ("_focus.dot", [ "-m"; focus ]);
    ("_nd.dot", [ "-nd" ]);
    ("_focus.json", [ "-m"; focus; "-f"; "json" ]);
    ("_nd.json", [ "-nd"; "-f"; "json" ]);
    ("_vb.json", [ "-m"; "Utils"; "-vb"; "multiply"; "-vl"; "2"; "-f"; "json" ]);
  ]

let test_project (project, fixture_name, focus) =
  let bs_dir = find_bs_dir project in
  printf "=== Output formats of %s ===\n" project;
  List.map
    (fun (suffix, args) ->
      let label = fixture_name ^ suffix in
      let rc, output = run_rescriptdep (args @ [ bs_dir ]) in
      let result =
        if rc <> 0 then (
          printf "ERROR: rescriptdep exited with %d for %s\n" rc label;
          false)
        else
          Test_support.check_fixture ~label output
            (Filename.concat (Test_support.fixtures_dir ()) label)
      in
      Sys.remove output;
      result)
    (golden_outputs ~focus)
  |> List.for_all Fun.id

let read_file path = In_channel.with_open_bin path In_channel.input_all

(* A compiler version value usage analysis does not support gives -6 for
   every module and still exits with 0. The fixture is copied so the version
   rewatch records in lib/bs/compiler-info.json can be changed. *)
let test_unsupported_compiler_value_usage () =
  printf "=== Value usage with an unsupported compiler ===\n";
  let bs_dir = find_bs_dir "rescript13/basic" in
  let temp_dir = Filename.temp_dir "rescriptdep_vb_" "" in
  let copy = Filename.concat temp_dir "bs" in
  Fun.protect
    (fun () ->
      if
        Sys.command
          (sprintf "cp -R %s %s" (Filename.quote bs_dir) (Filename.quote copy))
        <> 0
      then failwith "Could not copy the rescript13/basic build directory";
      Out_channel.with_open_bin (Filename.concat copy "compiler-info.json")
        (fun oc -> output_string oc {|{ "version": "99.0.0" }|});
      let rc, output =
        run_rescriptdep [ "-m"; "Utils"; "-vb"; "add"; "-f"; "json"; copy ]
      in
      let actual = read_file output in
      Sys.remove output;
      let expected =
        {|{
  "modules": [
    { "name": "MathUtil", "count": -6 },
    { "name": "Utils", "count": -6 }
  ]
}
|}
      in
      if rc = 0 && actual = expected then (
        printf "  Utils.add: MathUtil=-6, Utils=-6\n";
        true)
      else (
        printf "  ERROR: expected rc 0 and\n%s  got rc %d and\n%s" expected rc
          actual;
        false))
    ~finally:(fun () ->
      ignore (Sys.command (sprintf "rm -rf %s" (Filename.quote temp_dir))))

let () =
  let results =
    [
      ("rescript", "rescript", "Math");
      ("rescript12/basic", "rescript12_basic", "MathUtil");
      ("rescript12/namespaced", "rescript12_namespaced", "MathUtil");
      ("rescript13/basic", "rescript13_basic", "MathUtil");
      ("rescript13/namespaced", "rescript13_namespaced", "MathUtil");
    ]
    |> List.map test_project
  in
  let negative = test_unsupported_compiler_value_usage () in
  exit (if List.for_all Fun.id results && negative then 0 else 1)
