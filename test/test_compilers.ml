open Stdlib

(* Compiler identification and typed tree decoder guards. These tests build
   synthetic CMT files and typed tree values instead of compiled fixtures, so
   they also run under `dune runtest`. *)

let assert_true condition message = if not condition then failwith message

(* A cmt_infos record laid out like ReScript 11 writes it *)
let rescript11_cmt_info =
  Obj.repr
    {
      Cmt_format.cmt_modname = "Sample";
      cmt_annots = Implementation (Obj.repr ());
      cmt_value_dependencies = [];
      cmt_comments = [];
      cmt_args = [||];
      cmt_sourcefile = None;
      cmt_builddir = "";
      cmt_loadpath = [];
      cmt_source_digest = None;
      cmt_initial_env = Env.empty;
      cmt_imports = [];
      cmt_interface_digest = None;
      cmt_use_summaries = false;
    }

(* ReScript 12 and 13 append cmt_extra_info to the record *)
let rescript12_cmt_info =
  let size = Obj.size rescript11_cmt_info in
  let extended = Obj.new_block 0 (size + 1) in
  for i = 0 to size - 1 do
    Obj.set_field extended i (Obj.field rescript11_cmt_info i)
  done;
  Obj.set_field extended size (Obj.repr ());
  extended

(* Write a CMT file into a fresh lib/bs-like directory, with the
   compiler-info.json rewatch writes when [version] is given *)
let with_cmt_file ?version ~magic cmt_info f =
  let dir = Filename.temp_dir "rescriptdep-compiler" "" in
  let path = Filename.concat dir "sample.cmt" in
  let info_path = Filename.concat dir "compiler-info.json" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove path with _ -> ());
      (try Sys.remove info_path with _ -> ());
      try Sys.rmdir dir with _ -> ())
    (fun () ->
      Option.iter
        (fun version ->
          let oc = open_out info_path in
          Printf.fprintf oc {|{ "version": "%s", "bsc_path": "bsc.exe" }|}
            version;
          close_out oc)
        version;
      let oc = open_out_bin path in
      output_string oc magic;
      output_value oc cmt_info;
      close_out oc;
      f path)

(* The compiler name and whether value usage analysis can decode its typed
   tree, or [None] when the CMT file is rejected *)
let identify ?version ~magic cmt_info =
  with_cmt_file ?version ~magic cmt_info (fun path ->
      match Rescriptdep.Compilers.read_cmt path with
      | compiler, _ ->
          Some
            ( compiler.Rescriptdep.Compilers.name,
              Option.is_some compiler.Rescriptdep.Compilers.implementation )
      | exception Rescriptdep.Compilers.Unsupported _ -> None)

let test_identification () =
  assert_true
    (identify ~magic:"Caml1999T022" rescript11_cmt_info
    = Some ("ReScript 11", true))
    "Expected 13-field T022 CMT files to be read as ReScript 11";
  assert_true
    (identify ~magic:"Caml1999T022" rescript12_cmt_info
    = Some ("ReScript 12", true))
    "Expected 14-field T022 CMT files to be read as ReScript 12";
  assert_true
    (identify ~magic:"Caml1999T037" rescript12_cmt_info
    = Some ("ReScript 13", true))
    "Expected 14-field T037 CMT files to be read as ReScript 13";
  assert_true
    (identify ~magic:"Caml1999T037" rescript11_cmt_info = None)
    "Expected unknown magic number and layout combinations to be rejected";
  assert_true
    (identify ~magic:"Caml1999T099" rescript12_cmt_info = None)
    "Expected unknown CMT format versions to be rejected"

(* ReScript 12 and the 13.0 alphas up to alpha.5 write the same CMT format,
   so the compiler-info.json version picks the decoder *)
let test_identification_by_version () =
  let check ~magic version expected =
    assert_true
      (identify ~version ~magic rescript12_cmt_info = Some expected)
      (Printf.sprintf "Unexpected compiler identification for %s" version)
  in
  check ~magic:"Caml1999T022" "12.3.1" ("ReScript 12", true);
  check ~magic:"Caml1999T022" "12.9.0" ("ReScript 12", true);
  check ~magic:"Caml1999T022" "13.0.0-alpha.3" ("ReScript 12", true);
  check ~magic:"Caml1999T022" "13.0.0-alpha.5" ("ReScript 12", true);
  check ~magic:"Caml1999T022" "13.0.0-alpha.4" ("ReScript 13.0.0-alpha.4", false);
  check ~magic:"Caml1999T037" "13.0.0-alpha.6" ("ReScript 13", true);
  check ~magic:"Caml1999T037" "13.0.0-alpha.7" ("ReScript 13.0.0-alpha.7", false)

let test_unsupported_compiler_fails_the_graph () =
  with_cmt_file ~magic:"Caml1999T099" rescript12_cmt_info (fun path ->
      match
        Rescriptdep.Parser.parse_files_or_dirs ~skip_cache:true [ path ]
      with
      | _ ->
          failwith
            "Expected CMT files of unknown compilers to fail the analysis \
             instead of producing an empty graph"
      | exception Rescriptdep.Compilers.Unsupported _ -> ())

let test_value_usage_reports_unsupported_compiler () =
  with_cmt_file ~version:"13.0.0-alpha.4" ~magic:"Caml1999T022"
    rescript12_cmt_info (fun path ->
      let graph =
        Rescriptdep.Dependency_graph.add ~cmt_path:path
          Rescriptdep.Dependency_graph.empty "Sample" [] None
      in
      assert_true
        (Rescriptdep.Dependency_graph.count_value_usage_in_dependents graph
           ~module_name:"Sample" ~value_name:"value" ~value_line:None
        = [ ("Sample", -6) ])
        "Expected value usage to report -6 for compilers it cannot decode")

(* The constructor counts used by the unknown-tag guard must match the
   vendored typedtree definitions: the last constructor of each variant has
   the last known tag *)
let test_constructor_counts () =
  let tag value = Obj.tag (Obj.repr value) in
  let last_block (_, blocks) = blocks - 1 in
  let opaque = Obj.repr () in
  let unknown = Obj.magic () in
  assert_true
    (tag (Typedtree_v11.Texp_extension_constructor (unknown, unknown))
    = last_block Rescriptdep.Compiler_v11.expression_desc_constructors)
    "ReScript 11 expression_desc count is out of date";
  assert_true
    ((Obj.obj (Obj.repr Typedtree_v11.Texp_unreachable) : int)
    = fst Rescriptdep.Compiler_v11.expression_desc_constructors - 1)
    "ReScript 11 expression_desc constant count is out of date";
  assert_true
    (tag (Typedtree_v11.Tstr_attribute "")
    = last_block Rescriptdep.Compiler_v11.structure_item_desc_constructors)
    "ReScript 11 structure_item_desc count is out of date";
  assert_true
    (tag (Typedtree_v11.Tmod_unpack (unknown, unknown))
    = last_block Rescriptdep.Compiler_v11.module_expr_desc_constructors)
    "ReScript 11 module_expr_desc count is out of date";
  assert_true
    (tag (Typedtree_v12.Texp_for_await_of (unknown, opaque, unknown, unknown))
    = last_block Rescriptdep.Compiler_v12.expression_desc_constructors)
    "ReScript 12 expression_desc count is out of date";
  assert_true
    ((Obj.obj (Obj.repr Typedtree_v12.Texp_continue) : int)
    = fst Rescriptdep.Compiler_v12.expression_desc_constructors - 1)
    "ReScript 12 expression_desc constant count is out of date";
  assert_true
    (tag (Typedtree_v12.Tstr_attribute opaque)
    = last_block Rescriptdep.Compiler_v12.structure_item_desc_constructors)
    "ReScript 12 structure_item_desc count is out of date";
  assert_true
    (tag (Typedtree_v12.Tmod_unpack (unknown, opaque))
    = last_block Rescriptdep.Compiler_v12.module_expr_desc_constructors)
    "ReScript 12 module_expr_desc count is out of date";
  assert_true
    (tag (Typedtree_v13.Texp_template { segments = opaque; values = [] })
    = last_block Rescriptdep.Compiler_v13.expression_desc_constructors)
    "ReScript 13 expression_desc count is out of date";
  assert_true
    ((Obj.obj (Obj.repr Typedtree_v13.Texp_continue) : int)
    = fst Rescriptdep.Compiler_v13.expression_desc_constructors - 1)
    "ReScript 13 expression_desc constant count is out of date";
  assert_true
    (tag (Typedtree_v13.Tstr_attribute opaque)
    = last_block Rescriptdep.Compiler_v13.structure_item_desc_constructors)
    "ReScript 13 structure_item_desc count is out of date";
  assert_true
    (tag (Typedtree_v13.Tmod_unpack (unknown, opaque))
    = last_block Rescriptdep.Compiler_v13.module_expr_desc_constructors)
    "ReScript 13 module_expr_desc count is out of date"

(* An expression whose constructor tag is beyond the known ones, as a later
   release appending constructors would write it *)
let test_unknown_constructors_are_skipped () =
  let unknown_desc = Obj.new_block 40 1 in
  let v11_expression : Typedtree_v11.expression =
    {
      exp_desc = Obj.obj unknown_desc;
      exp_loc = Obj.magic ();
      exp_extra = [];
      exp_type = Obj.magic ();
      exp_env = ();
      exp_attributes = "";
    }
  in
  let v12_expression : Typedtree_v12.expression =
    {
      exp_desc = Obj.obj unknown_desc;
      exp_loc = Obj.magic ();
      exp_extra = Obj.repr ();
      exp_type = Obj.repr ();
      exp_env = Obj.repr ();
      exp_attributes = Obj.repr ();
    }
  in
  assert_true
    (Rescriptdep.Compiler_v11.expr v11_expression
    = Rescriptdep.Typed_ast.Node [])
    "Expected the ReScript 11 decoder to skip unknown constructors";
  let v13_expression : Typedtree_v13.expression =
    {
      exp_desc = Obj.obj unknown_desc;
      exp_loc = Obj.magic ();
      exp_extra = Obj.repr ();
      exp_type = Obj.repr ();
      exp_env = Obj.repr ();
      exp_attributes = Obj.repr ();
    }
  in
  assert_true
    (Rescriptdep.Compiler_v12.expr v12_expression
    = Rescriptdep.Typed_ast.Node [])
    "Expected the ReScript 12 decoder to skip unknown constructors";
  assert_true
    (Rescriptdep.Compiler_v13.expr v13_expression
    = Rescriptdep.Typed_ast.Node [])
    "Expected the ReScript 13 decoder to skip unknown constructors"

let () =
  test_identification ();
  test_identification_by_version ();
  test_unsupported_compiler_fails_the_graph ();
  test_value_usage_reports_unsupported_compiler ();
  test_constructor_counts ();
  test_unknown_constructors_are_skipped ();
  print_endline "Compiler support tests passed"
