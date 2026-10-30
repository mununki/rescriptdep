open Stdlib
open Printf

(* Run rescriptdep CLI and generate json output to temporary file *)
let run_rescriptdep_to_temp project_dir =
  let temp_file = Filename.temp_file "rescriptdep_" ".json" in
  let cmd =
    Printf.sprintf "dune exec -- rescriptdep -f json %s -o %s --no-cache"
      project_dir temp_file
  in
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then
    failwith
      (Printf.sprintf "Failed to run rescriptdep on %s (exit code: %d)"
         project_dir exit_code)
  else temp_file

(* Test rescriptdep on a specific project. [fixture_name] defaults to the
   project name and names the reference JSON in test/fixtures. *)
let test_project_fixtures ?fixture_name project_name =
  let fixture_name = Option.value fixture_name ~default:project_name in
  printf "Testing rescriptdep on %s project...\n" project_name;

  let project_dir =
    Test_support.require_existing_path
      ~hint:
        "Compiled ReScript fixture project not found. Run `make test` to \
         install the fixture dependencies and build `lib/bs` first."
      [
        Printf.sprintf "%s/lib/bs" project_name;
        Printf.sprintf "test/%s/lib/bs" project_name;
      ]
  in
  let reference_output =
    Filename.concat (Test_support.fixtures_dir ()) (fixture_name ^ ".json")
  in

  (* Run rescriptdep and compare its output with the reference *)
  let temp_output = run_rescriptdep_to_temp project_dir in
  let result =
    Test_support.check_fixture ~label:project_name temp_output
      reference_output
  in
  Sys.remove temp_output;
  result

(* Basic smoke test for the rescriptdep functionality *)
let test_rescriptdep () =
  printf "=== Running rescriptdep dependency tests ===\n";

  (* Run every project even when an earlier one fails *)
  let results =
    [
      ("rescript", "rescript");
      ("rescript12/basic", "rescript12_basic");
      ("rescript12/namespaced", "rescript12_namespaced");
      ("rescript13/basic", "rescript13_basic");
      ("rescript13/namespaced", "rescript13_namespaced");
    ]
    |> List.map (fun (project_name, fixture_name) ->
           test_project_fixtures ~fixture_name project_name)
  in
  List.for_all Fun.id results

(* Main execution code *)
let () =
  let success = test_rescriptdep () in
  exit (if success then 0 else 1)
