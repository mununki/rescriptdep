open Stdlib

let assert_true condition message =
  if not condition then failwith message

(* Write an AST file the way the compiler does: the byte length of the
   header, "\nModuleA\nModuleB\n...", then the source path and the
   marshalled AST. *)
let write_ast_fixture ast_path modules =
  let header =
    "\n" ^ String.concat "" (List.map (fun m -> m ^ "\n") modules)
  in
  let oc = open_out_bin ast_path in
  output_binary_int oc (String.length header);
  output_string oc header;
  output_string oc "/project/src/Consumer.res\n";
  (* Binary AST payload that must not be read as module references *)
  output_string oc "\132\149\166\190Dep02\nGarbage\n";
  close_out oc

let test_normalize_module_name () =
  assert_true
    (Rescriptdep.Parse_utils.normalize_module_name "NewNamespace.NS_alias"
    = "NS_alias")
    "Expected namespaced module name to normalize to its plain module name";
  assert_true
    (Rescriptdep.Parse_utils.normalize_module_name "DOMAPI-WebAPI" = "DOMAPI")
    "Expected generated namespaced module names to normalize to their source module name";
  assert_true
    (Rescriptdep.Parse_utils.normalize_module_name "dep01" = "Dep01")
    "Expected plain module name normalization to keep existing behavior"

let test_is_valid_module_name () =
  assert_true
    (Rescriptdep.Parse_utils.is_valid_module_name "NewNamespace.NS_alias")
    "Expected namespaced module name to be treated as a valid module";
  assert_true
    (Rescriptdep.Parse_utils.is_valid_module_name "DOMAPI-WebAPI")
    "Expected generated namespaced module names to be treated as valid modules";
  assert_true
    (not (Rescriptdep.Parse_utils.is_valid_module_name "not-a-module"))
    "Expected invalid module name to remain invalid"

let test_extract_dependencies_from_namespaced_imports () =
  let cmt_info =
    {
      Cmt_format.cmt_modname = "Consumer";
      cmt_annots = Implementation (Obj.magic ());
      cmt_value_dependencies = [];
      cmt_comments = [];
      cmt_args = [||];
      cmt_sourcefile = None;
      cmt_builddir = "";
      cmt_loadpath = [];
      cmt_source_digest = None;
      cmt_initial_env = Env.empty;
      cmt_imports =
        [
          ("NewNamespace.NS_alias", None);
          ("Dep01", None);
          ("Js", None);
        ];
      cmt_interface_digest = None;
      cmt_use_summaries = false;
    }
  in
  let dependencies =
    Rescriptdep.Parser.DependencyExtractor.extract_dependencies_from_cmt_info
      cmt_info
  in
  assert_true
    (List.mem "NewNamespace.NS_alias" dependencies)
    "Expected namespaced imports not to be dropped during extraction";
  let canonical_dependencies =
    dependencies
    |> List.map Rescriptdep.Parse_utils.normalize_module_name
    |> List.sort_uniq String.compare
  in
  assert_true
    (canonical_dependencies = [ "Dep01"; "NS_alias" ])
    "Expected namespaced imports to canonicalize to plain module graph keys"

let test_focus_with_namespaced_input () =
  let graph =
    Rescriptdep.Dependency_graph.empty
    |> fun graph ->
    Rescriptdep.Dependency_graph.add graph "NS_alias" [ "Dep01" ]
      (Some "/tmp/NS_alias.res")
    |> fun graph ->
    Rescriptdep.Dependency_graph.add graph "Dep01" [] (Some "/tmp/Dep01.res")
  in
  let focused_graph =
    Rescriptdep.Dependency_graph.create_focused_graph graph
      (Rescriptdep.Parse_utils.normalize_module_name "NewNamespace.NS_alias")
  in
  let modules =
    Rescriptdep.Dependency_graph.get_modules focused_graph |> List.sort compare
  in
  assert_true
    (modules = [ "Dep01"; "NS_alias" ])
    "Expected focus mode to resolve namespaced input to the existing module"

let test_dependency_graph_dependents_index_updates () =
  let graph =
    Rescriptdep.Dependency_graph.empty
    |> fun graph ->
    Rescriptdep.Dependency_graph.add graph "Consumer" [ "Dep01" ] None
    |> fun graph -> Rescriptdep.Dependency_graph.add graph "Dep01" [] None
  in
  assert_true
    (Rescriptdep.Dependency_graph.find_dependents graph "Dep01" = [ "Consumer" ])
    "Expected dependents index to include direct dependent";

  let graph =
    Rescriptdep.Dependency_graph.add graph "Consumer" [ "Dep02" ] None
  in
  assert_true
    (Rescriptdep.Dependency_graph.find_dependents graph "Dep01" = [])
    "Expected dependents index to drop stale dependencies after module update";
  assert_true
    (Rescriptdep.Dependency_graph.find_dependents graph "Dep02" = [ "Consumer" ])
    "Expected dependents index to include updated dependency"

let test_ast_path_for_namespaced_cmt () =
  assert_true
    (Rescriptdep.Parser.DependencyExtractor.get_ast_path
       "/project/lib/bs/src/app-MyNamespace.cmt"
    = "/project/lib/bs/src/app.ast")
    "Expected namespaced CMT files to resolve to the unsuffixed AST file";
  assert_true
    (Rescriptdep.Parser.DependencyExtractor.get_ast_path
       "/project/lib/bs/src/app.cmt"
    = "/project/lib/bs/src/app.ast")
    "Expected plain CMT files to resolve to the AST file next to them"

let test_read_ast_header_only () =
  let ast_path = Filename.temp_file "rescriptdep-header" ".ast" in
  Fun.protect
    (fun () ->
      write_ast_fixture ast_path [ "Dep01"; "NS_alias" ];
      Rescriptdep.Parser.Cache.clear ();
      assert_true
        (Rescriptdep.Parser.DependencyExtractor.read_ast_file ast_path
        = Some [ "Dep01"; "NS_alias" ])
        "Expected only the AST header to be read as module references")
    ~finally:(fun () -> try Sys.remove ast_path with _ -> ())

let test_dependencies_filtered_by_namespaced_ast () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "rescriptdep-ns" in
  (try Sys.mkdir dir 0o755 with Sys_error _ -> ());
  let cmt_path = Filename.concat dir "consumer-MyNamespace.cmt" in
  let ast_path = Filename.concat dir "consumer.ast" in
  Fun.protect
    (fun () ->
      write_ast_fixture ast_path [ "Dep01" ];
      Rescriptdep.Parser.Cache.clear ();
      let cmt_info =
        {
          Cmt_format.cmt_modname = "Consumer-MyNamespace";
          cmt_annots = Implementation (Obj.magic ());
          cmt_value_dependencies = [];
          cmt_comments = [];
          cmt_args = [||];
          cmt_sourcefile = None;
          cmt_builddir = "";
          cmt_loadpath = [];
          cmt_source_digest = None;
          cmt_initial_env = Env.empty;
          cmt_imports =
            [
              ("Dep01-MyNamespace", None);
              ("Dep02-MyNamespace", None);
              ("MyNamespace", None);
              ("Stdlib_Array", None);
            ];
          cmt_interface_digest = None;
          cmt_use_summaries = false;
        }
      in
      let dependencies =
        Rescriptdep.Parser.DependencyExtractor.extract_dependencies_from_cmt_info
          ~cmt_path cmt_info
      in
      assert_true
        (dependencies = [ "Dep01-MyNamespace" ])
        "Expected namespaced imports to be filtered by the unsuffixed AST file")
    ~finally:(fun () -> try Sys.remove ast_path with _ -> ())

let test_batch_check_matches_canonical_and_qualified_ast_entries () =
  let source_file = Filename.temp_file "rescriptdep-namespace" ".res" in
  let ast_path = Filename.remove_extension source_file ^ ".ast" in
  Fun.protect
    (fun () ->
      write_ast_fixture ast_path [ "NS_alias" ];
      Rescriptdep.Parser.Cache.clear ();
      let deps_from_canonical_ast =
        Rescriptdep.Parser.DependencyExtractor.batch_check_modules_usage
          source_file [ "NewNamespace.NS_alias" ]
      in
      assert_true
        (deps_from_canonical_ast = [ "NewNamespace.NS_alias" ])
        "Expected namespaced imports to match canonical AST entries";

      write_ast_fixture ast_path [ "NewNamespace.NS_alias" ];
      Rescriptdep.Parser.Cache.clear ();
      let deps_from_qualified_ast =
        Rescriptdep.Parser.DependencyExtractor.batch_check_modules_usage
          source_file [ "NewNamespace.NS_alias" ]
      in
      assert_true
        (deps_from_qualified_ast = [ "NewNamespace.NS_alias" ])
        "Expected namespaced imports to match qualified AST entries";

      write_ast_fixture ast_path [ "DOMAPI" ];
      Rescriptdep.Parser.Cache.clear ();
      let deps_from_generated_suffix =
        Rescriptdep.Parser.DependencyExtractor.batch_check_modules_usage
          source_file [ "DOMAPI-WebAPI" ]
      in
      assert_true
        (deps_from_generated_suffix = [ "DOMAPI-WebAPI" ])
        "Expected generated namespaced module names to match canonical AST entries")
    ~finally:(fun () ->
      (try Sys.remove source_file with _ -> ());
      (try Sys.remove ast_path with _ -> ()))

let () =
  test_normalize_module_name ();
  test_is_valid_module_name ();
  test_extract_dependencies_from_namespaced_imports ();
  test_focus_with_namespaced_input ();
  test_dependency_graph_dependents_index_updates ();
  test_ast_path_for_namespaced_cmt ();
  test_read_ast_header_only ();
  test_dependencies_filtered_by_namespaced_ast ();
  test_batch_check_matches_canonical_and_qualified_ast_entries ();
  print_endline "Namespace handling tests passed"
