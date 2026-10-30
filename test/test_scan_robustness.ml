open Stdlib
open Printf

(* An unreadable entry in a build directory (here a dangling symlink) must be
   skipped on its own. It used to drop every module of its directory, which
   gave an empty graph with exit code 0. *)

let module_names bs_dir =
  Rescriptdep.Parser.parse_files_or_dirs ~skip_cache:true [ bs_dir ]
  |> List.map (fun (info : Rescriptdep.Parser.module_info) -> info.name)
  |> List.sort_uniq String.compare

let run_command cmd =
  if Sys.command cmd <> 0 then failwith (sprintf "Command failed: %s" cmd)

let () =
  let bs_dir =
    Test_support.require_existing_path
      ~hint:
        "Compiled ReScript fixture project not found. Run `make test` to \
         install the fixture dependencies and build `lib/bs` first."
      [ "rescript/lib/bs"; "test/rescript/lib/bs" ]
  in
  let expected = module_names bs_dir in
  let temp_dir = Filename.temp_dir "rescriptdep_scan_" "" in
  let copy = Filename.concat temp_dir "bs" in
  let ok =
    Fun.protect
      (fun () ->
        run_command
          (sprintf "cp -R %s %s" (Filename.quote bs_dir) (Filename.quote copy));
        Unix.symlink
          (Filename.concat temp_dir "missing-target")
          (Filename.concat (Filename.concat copy "src") "broken_link");
        let actual = module_names copy in
        printf "=== Scan with a dangling symlink ===\n";
        if expected <> [] && actual = expected then (
          printf "  modules: %s\n" (String.concat ", " actual);
          true)
        else (
          printf "  ERROR expected [%s], got [%s]\n"
            (String.concat ", " expected)
            (String.concat ", " actual);
          false))
      ~finally:(fun () ->
        ignore (Sys.command (sprintf "rm -rf %s" (Filename.quote temp_dir))))
  in
  exit (if ok then 0 else 1)
