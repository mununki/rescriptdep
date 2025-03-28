open Stdlib
open Printf

(* Run rescriptdep CLI and generate json output to temporary file *)
let run_rescriptdep_to_temp project_dir =
  let temp_file = Filename.temp_file "rescriptdep_" ".json" in
  let cmd =
    Printf.sprintf "dune exec -- rescriptdep --format json %s -o %s" project_dir
      temp_file
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

(* Test rescriptdep on a specific project *)
let test_project_fixtures project_name =
  printf "Testing rescriptdep on %s project...\n" project_name;

  (* Create paths *)
  let project_dir = Printf.sprintf "test/%s" project_name in
  let reference_output = Printf.sprintf "test/fixtures/%s.json" project_name in

  (* Ensure fixtures directory exists *)
  let _ = Sys.command "mkdir -p test/fixtures" in

  (* Run rescriptdep to generate current output in temp file *)
  let temp_output = run_rescriptdep_to_temp project_dir in

  (* If reference output doesn't exist, create it by copying temp output *)
  let result =
    if not (Sys.file_exists reference_output) then (
      printf "Creating initial fixture for %s\n" project_name;
      let _ =
        Sys.command (Printf.sprintf "cp %s %s" temp_output reference_output)
      in
      true)
    else
      (* Compare temp output with reference output *)
      let is_same = compare_files temp_output reference_output in
      if not is_same then (
        printf "ERROR: Output for %s differs from reference fixture!\n"
          project_name;
        let diff_cmd =
          Printf.sprintf "diff -u %s %s" reference_output temp_output
        in
        let _ = Sys.command diff_cmd in
        false)
      else (
        printf "Output for %s matches reference fixture\n" project_name;
        true)
  in

  (* Clean up temp file *)
  Sys.remove temp_output;

  result

(* Basic smoke test for the rescriptdep functionality *)
let test_rescriptdep () =
  printf "=== Running rescriptdep dependency tests ===\n";

  (* Test each project *)
  let rescript_result = test_project_fixtures "rescript" in
  let rewatch_result = test_project_fixtures "rewatch" in

  (* Return true only if all tests passed *)
  rescript_result && rewatch_result

(* Main execution code *)
let () =
  let success = test_rescriptdep () in
  exit (if success then 0 else 1)
