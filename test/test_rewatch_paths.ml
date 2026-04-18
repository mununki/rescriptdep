open Stdlib

let write_ast_fixture ast_path modules =
  let oc = open_out_bin ast_path in
  Fun.protect
    (fun () ->
      output_binary_int oc (List.length modules);
      output_char oc '\n';
      List.iter (fun module_name -> output_string oc (module_name ^ "\n")) modules)
    ~finally:(fun () -> close_out_noerr oc)

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun entry -> remove_tree (Filename.concat path entry));
      Unix.rmdir path)
    else Sys.remove path

let with_temp_project f =
  let root = Filename.temp_file "rescriptdep_rewatch_paths_" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect (fun () -> f root) ~finally:(fun () -> remove_tree root)

let ensure_dir path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o755

let write_empty_file path =
  let oc = open_out_bin path in
  close_out oc

let test_resolves_rewatch_case_mismatch () =
  with_temp_project (fun root ->
      let src_dir = Filename.concat root "src" in
      let lib_dir = Filename.concat root "lib" in
      let bs_dir = Filename.concat lib_dir "bs" in
      let bs_src_dir = Filename.concat bs_dir "src" in
      ensure_dir src_dir;
      ensure_dir lib_dir;
      ensure_dir bs_dir;
      ensure_dir bs_src_dir;

      let source_file = Filename.concat src_dir "app.res" in
      let cmt_file = Filename.concat bs_src_dir "App.cmt" in
      let ast_file = Filename.concat bs_src_dir "app.ast" in

      write_empty_file source_file;
      write_empty_file cmt_file;
      write_ast_fixture ast_file [ "Logger"; "Math" ];

      let resolved_cmt =
        Rescriptdep.Parser.DependencyExtractor.get_cmt_path_for_source
          source_file
      in
      assert (Sys.file_exists resolved_cmt);
      assert
        (String.lowercase_ascii (Filename.basename resolved_cmt) = "app.cmt");

      let resolved_ast =
        Rescriptdep.Parser.DependencyExtractor.get_ast_path resolved_cmt
      in
      assert (Sys.file_exists resolved_ast);
      assert
        (String.lowercase_ascii (Filename.basename resolved_ast) = "app.ast");

      let deps =
        Rescriptdep.Parser.DependencyExtractor.batch_check_modules_usage
          source_file [ "Logger"; "Math"; "Utils" ]
      in
      assert (deps = [ "Logger"; "Math" ]))

let () = test_resolves_rewatch_case_mismatch ()
