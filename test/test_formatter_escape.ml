open Stdlib
open Printf

(* Paths with a double quote or a backslash used to produce invalid JSON and
   DOT. The graph is built in memory, so no fixture project is needed. *)

let app_path = {|/work/quote"dir/src/App.res|}
let util_path = {|C:\proj\back\slash\Util.res|}

let module_info name path dependencies =
  {
    Rescriptdep.Parser.name;
    dependencies;
    interface_digest = None;
    implementation_digest = None;
    file_path = Some path;
    cmt_path = None;
  }

let graph =
  Rescriptdep.Dependency_graph.build_from_module_infos
    [ module_info "App" app_path [ "Util" ]; module_info "Util" util_path [] ]

let render format =
  let file = Filename.temp_file "rescriptdep_escape_" ".out" in
  Out_channel.with_open_bin file (fun oc ->
      Rescriptdep.Formatter.output_graph format graph oc);
  let text = In_channel.with_open_bin file In_channel.input_all in
  Sys.remove file;
  text

let check name ok =
  printf "  %s: %s\n" (if ok then "ok" else "ERROR") name;
  ok

(* Run the checks in order and report every one of them *)
let check_all checks =
  List.map (fun (name, ok) -> check name (Lazy.force ok)) checks
  |> List.for_all Fun.id

let contains text sub =
  let n = String.length text and m = String.length sub in
  let rec loop i = i + m <= n && (String.sub text i m = sub || loop (i + 1)) in
  loop 0

(* The JSON must parse and give back the original paths *)
let test_json () =
  let open Yojson.Safe.Util in
  match Yojson.Safe.from_string (render Rescriptdep.Formatter.Json) with
  | exception Yojson.Json_error msg -> check ("JSON parses: " ^ msg) false
  | json ->
      let modules = json |> member "modules" |> to_list in
      let find name =
        List.find (fun m -> m |> member "name" |> to_string = name) modules
      in
      let path m = m |> member "path" |> to_string in
      let first key m = m |> member key |> to_list |> List.hd in
      let app = find "App" and util = find "Util" in
      check_all
        [
          ( "JSON module paths round-trip",
            lazy (path app = app_path && path util = util_path) );
          ( "JSON dependency path round-trips",
            lazy (path (first "dependencies" app) = util_path) );
          ( "JSON dependent path round-trips",
            lazy (path (first "dependents" util) = app_path) );
        ]

let test_dot () =
  let dot = render Rescriptdep.Formatter.Dot in
  check_all
    [
      ( "DOT escapes double quotes",
        lazy (contains dot {|tooltip="/work/quote\"dir/src/App.res"|}) );
      ( "DOT escapes backslashes",
        lazy (contains dot {|tooltip="C:\\proj\\back\\slash\\Util.res"|}) );
    ]

let () =
  printf "=== Escaping in the DOT and JSON writers ===\n";
  let json_ok = test_json () in
  let dot_ok = test_dot () in
  exit (if json_ok && dot_ok then 0 else 1)
