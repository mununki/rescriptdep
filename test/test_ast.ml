open Stdlib
open Printf

(* Check that the module references read from the header of compiler-generated
   .ast files match the modules each fixture source refers to. *)

let read_module_refs ast_path =
  match
    Rescriptdep.Parser.DependencyExtractor.read_ast_file ~skip_cache:true
      ast_path
  with
  | Some modules -> List.sort_uniq String.compare modules
  | None -> failwith (sprintf "Could not read the AST header of %s" ast_path)

let check_ast_fixture ~candidates ~expected =
  let ast_path =
    Test_support.require_existing_path
      ~hint:
        "AST fixture not found. Run `make test` to install the ReScript test \
         projects and build their `.ast` output first."
      candidates
  in
  let modules = read_module_refs ast_path in
  printf "Module references in %s:\n" ast_path;
  List.iter (printf "  - %s\n") modules;
  if modules <> expected then (
    printf "ERROR: expected %s\n" (String.concat ", " expected);
    false)
  else true

let () =
  if Array.length Sys.argv > 1 then (
    List.iter (printf "%s\n") (read_module_refs Sys.argv.(1));
    exit 0);

  let results =
    [
      ( "rescript/lib/bs/src/app.ast",
        [ "Belt"; "Comp0"; "Logger"; "Math"; "React" ] );
      ( "rescript12/basic/lib/bs/src/app.ast",
        [ "Belt"; "Comp0"; "Logger"; "MathUtil"; "React" ] );
      ("rescript12/namespaced/lib/bs/src/mathUtil.ast", [ "Belt"; "U"; "Utils" ]);
      ( "rescript13/basic/lib/bs/src/app.ast",
        [ "Comp0"; "Int"; "Logger"; "MathUtil"; "React" ] );
      ( "rescript13/namespaced/lib/bs/src/mathUtil.ast",
        [ "Array"; "U"; "Utils" ] );
    ]
    |> List.map (fun (ast_path, expected) ->
        check_ast_fixture ~candidates:[ ast_path; "test/" ^ ast_path ] ~expected)
  in
  exit (if List.for_all Fun.id results then 0 else 1)
