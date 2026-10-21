open Stdlib
open Printf

(* Normalize machine-specific absolute paths inside JSON fixtures so the tests
   compare repository-relative paths across local and CI environments. *)
let normalize_json_paths input_file =
  let normalized_file = Filename.temp_file "rescriptdep_normalized_" ".json" in
  let ic = open_in input_file in
  let oc = open_out normalized_file in
  Fun.protect
    (fun () ->
      try
        while true do
          let line = input_line ic in
          let normalized_line =
            line |> Str.global_replace (Str.regexp {|[^"]*/test/|}) "test/"
          in
          output_string oc normalized_line;
          output_char oc '\n'
        done
      with End_of_file -> ())
    ~finally:(fun () ->
      close_in_noerr ic;
      close_out_noerr oc);
  normalized_file

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

(* Compare two files and return true if they're identical *)
let compare_files file1 file2 =
  let cmd = Printf.sprintf "diff -q %s %s >/dev/null" file1 file2 in
  Sys.command cmd = 0

(* Test rescriptdep on a specific project. [fixture_name] defaults to the
   project name and names the reference JSON in test/fixtures. *)
let test_project_fixtures ?fixture_name project_name =
  let fixture_name = Option.value fixture_name ~default:project_name in
  printf "Testing rescriptdep on %s project...\n" project_name;

  (* Create paths *)
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
    let fixtures_dir =
      Test_support.require_existing_path
        ~hint:"Reference JSON fixture directory not found."
        [ "fixtures"; "test/fixtures" ]
    in
    Filename.concat fixtures_dir (fixture_name ^ ".json")
  in

  (* Ensure fixtures directory exists *)
  let _ =
    Sys.command
      (Printf.sprintf "mkdir -p %s" (Filename.quote (Filename.dirname reference_output)))
  in

  (* Run rescriptdep to generate current output in temp file *)
  let temp_output = run_rescriptdep_to_temp project_dir in
  let normalized_temp_output = normalize_json_paths temp_output in

  (* If reference output doesn't exist, create it by copying temp output *)
  let result =
    if not (Sys.file_exists reference_output) then (
      printf "Creating initial fixture for %s\n" project_name;
      let _ =
        Sys.command
          (Printf.sprintf "cp %s %s" normalized_temp_output reference_output)
      in
      true)
    else
      (* Compare temp output with reference output *)
      let normalized_reference_output = normalize_json_paths reference_output in
      let is_same =
        compare_files normalized_reference_output normalized_temp_output
      in
      let result =
        if not is_same then (
          printf "ERROR: Output for %s differs from reference fixture!\n"
            project_name;
          let diff_cmd =
            Printf.sprintf "diff -u %s %s" normalized_reference_output
              normalized_temp_output
          in
          let _ = Sys.command diff_cmd in
          false)
        else (
          printf "Output for %s matches reference fixture\n" project_name;
          true)
      in
      Sys.remove normalized_reference_output;
      result
  in

  (* Clean up temp file *)
  Sys.remove temp_output;
  Sys.remove normalized_temp_output;

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
