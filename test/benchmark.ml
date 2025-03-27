open Stdlib
open Rescriptdep.Parser

(* Timing utilities *)
let time_it f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  (result, stop -. start)

(* Get benchmark path from environment variable or use default *)
let benchmark_path =
  try Sys.getenv "RESCRIPTDEP_BENCHMARK_PATH"
  with Not_found ->
    (* Try to find a valid ReScript project directory *)
    if Sys.file_exists "./lib/bs/src" && Sys.is_directory "./lib/bs/src" then
      "./lib/bs/src"
    else if Sys.file_exists "test/fixtures" && Sys.is_directory "test/fixtures"
    then "test/fixtures" (* Use test fixtures as fallback *)
    else "/tmp" (* Default to a directory that should always exist *)

let cache_path = "/tmp/rescriptdep_bench_cache.marshal"

(* Environment variables *)
let () =
  Unix.putenv "RESCRIPTDEP_BENCHMARK" "1";
  Unix.putenv "RESCRIPTDEP_VERBOSE" "1"

let run_benchmark () =
  (* Set up the cache file and ensure it doesn't exist *)
  set_cache_file cache_path;
  if Sys.file_exists cache_path then Sys.remove cache_path;
  clear_cache ();

  (* Run the analysis *)
  Printf.printf "Starting analysis...\n";
  let modules, time =
    time_it (fun () ->
        let modules = parse_files_or_dirs ~verbose:false [ benchmark_path ] in
        Printf.printf "Found %d modules\n" (List.length modules);
        modules)
  in

  Printf.printf "\n========== BENCHMARK RESULTS ==========\n";
  Printf.printf "Target project: %s\n" benchmark_path;
  Printf.printf "Modules analyzed: %d\n" (List.length modules);
  Printf.printf "Total time: %.4f seconds\n" time;
  Printf.printf "=======================================\n";

  modules

let () =
  (* Check if we're running as part of a test suite *)
  let is_test = try Sys.getenv "DUNE_RUNTEST" = "1" with Not_found -> false in

  (* Skip actual benchmark execution if running as part of dune runtest *)
  if is_test then
    Printf.printf
      "Benchmark test: SKIPPED (use RESCRIPTDEP_BENCHMARK_PATH=<path> dune \
       exec test/benchmark.exe to run)\n"
  else (
    (* Make sure the benchmark path exists *)
    if not (Sys.file_exists benchmark_path && Sys.is_directory benchmark_path)
    then (
      Printf.eprintf "Benchmark path %s does not exist or is not a directory\n"
        benchmark_path;
      exit 1);

    Printf.printf "Running benchmark on %s\n" benchmark_path;
    Printf.printf "Cache file: %s\n" cache_path;

    let _ = run_benchmark () in
    ())
