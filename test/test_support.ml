open Stdlib

let executable_dir () =
  let executable = Sys.executable_name in
  let absolute_executable =
    if Filename.is_relative executable then Filename.concat (Sys.getcwd ()) executable
    else executable
  in
  Filename.dirname absolute_executable

let rec find_upwards dir relative_path =
  let candidate = Filename.concat dir relative_path in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_upwards parent relative_path

let search_roots () =
  let roots = [ Sys.getcwd (); executable_dir () ] in
  List.sort_uniq String.compare roots

let resolve_existing_path candidates =
  let roots = search_roots () in
  let rec loop = function
    | [] -> None
    | candidate :: rest ->
        if Sys.file_exists candidate then Some candidate
        else
          (match List.find_map (fun root -> find_upwards root candidate) roots with
          | Some path -> Some path
          | None -> loop rest)
  in
  loop candidates

let require_existing_path ?hint candidates =
  match resolve_existing_path candidates with
  | Some path -> path
  | None ->
      let message =
        match hint with
        | Some hint -> hint
        | None ->
            Printf.sprintf "Could not find any of these paths: %s"
              (String.concat ", " candidates)
      in
      failwith message

(* Replace machine-specific absolute paths with repository-relative ones
   ("/abs/repo/test/x" -> "test/x") so outputs compare across local and CI
   environments. Returns a temporary file with the normalized copy. *)
let normalize_paths input_file =
  let normalized_file = Filename.temp_file "rescriptdep_normalized_" ".txt" in
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

(* The directory holding the reference outputs *)
let fixtures_dir () =
  require_existing_path ~hint:"Reference fixture directory not found."
    [ "fixtures"; "test/fixtures" ]

(* Compare [output_file] with the reference [fixture] after normalizing the
   paths in both, printing a diff when they differ. References are written
   only when RESCRIPTDEP_UPDATE_FIXTURES=1 is explicitly set. *)
let check_fixture ~label output_file fixture =
  let normalized_output = normalize_paths output_file in
  let result =
    if Sys.getenv_opt "RESCRIPTDEP_UPDATE_FIXTURES" = Some "1" then (
      Printf.printf "Updating reference fixture for %s\n" label;
      In_channel.with_open_bin normalized_output (fun ic ->
          Out_channel.with_open_bin fixture (fun oc ->
              output_string oc (In_channel.input_all ic)));
      true)
    else if not (Sys.file_exists fixture) then (
      Printf.printf
        "ERROR: missing reference %s (set RESCRIPTDEP_UPDATE_FIXTURES=1 to create it)\n"
        fixture;
      false)
    else
      let normalized_fixture = normalize_paths fixture in
      let is_same =
        Sys.command
          (Printf.sprintf "diff -q %s %s >/dev/null"
             (Filename.quote normalized_fixture)
             (Filename.quote normalized_output))
        = 0
      in
      if is_same then
        Printf.printf "Output for %s matches reference fixture\n" label
      else (
        Printf.printf "ERROR: Output for %s differs from reference fixture!\n"
          label;
        flush stdout;
        ignore
          (Sys.command
             (Printf.sprintf "diff -u %s %s"
                (Filename.quote normalized_fixture)
                (Filename.quote normalized_output))));
      Sys.remove normalized_fixture;
      is_same
  in
  Sys.remove normalized_output;
  result
