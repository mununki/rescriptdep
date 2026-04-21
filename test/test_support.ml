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
