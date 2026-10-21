(* compilers.ml - ReScript compiler releases whose build artifacts are supported

   The dependency graph only reads fields of cmt_infos that every release below
   lays out the same way, plus the .ast header, whose format is shared too.
   Only the typed tree used by value usage analysis differs between releases,
   so each entry says how to recognize the release and how to decode its typed
   tree. Releases are recognized from the CMT file itself and, where several
   write the same CMT format, from the compiler version rewatch records. *)

open Stdlib

type t = {
  name : string;
  cmt_magic : string;  (** Magic number of the typed tree section *)
  cmt_infos_fields : int;  (** Number of fields of the cmt_infos record *)
  versions : string -> bool;
      (** Compiler versions, as in lib/bs/compiler-info.json, of the release *)
  implementation : (Obj.t -> Typed_ast.item list) option;
      (** [None] when value usage analysis does not support the release *)
}

let major version =
  match String.index_opt version '.' with
  | Some idx -> String.sub version 0 idx
  | None -> version

(* For each CMT format, the first entry is used when the compiler version is
   unknown *)
let all =
  [
    {
      name = "ReScript 11";
      cmt_magic = "Caml1999T022";
      cmt_infos_fields = 13;
      versions = (fun version -> major version = "11");
      implementation = Some Compiler_v11.implementation;
    };
    {
      name = "ReScript 12";
      cmt_magic = "Caml1999T022";
      cmt_infos_fields = 14;
      (* 13.0.0-alpha.1 to alpha.3 and alpha.5 write the ReScript 12 CMT format
         with a typed tree Typedtree_v12 reads. alpha.4 inserted constructors
         in the middle of expression_desc, so it is left out. *)
      versions =
        (fun version ->
          major version = "12"
          || List.mem version
               [
                 "13.0.0-alpha.1";
                 "13.0.0-alpha.2";
                 "13.0.0-alpha.3";
                 "13.0.0-alpha.5";
               ]);
      implementation = Some Compiler_v12.implementation;
    };
    {
      name = "ReScript 13";
      cmt_magic = "Caml1999T037";
      cmt_infos_fields = 14;
      versions = (fun version -> version = "13.0.0-alpha.6");
      implementation = Some Compiler_v13.implementation;
    };
  ]

exception Unsupported of string

let cmt_infos_fields (cmt_info : Cmt_format.cmt_infos) =
  Obj.size (Obj.repr cmt_info)

(* rewatch (ReScript 12 and later) records the compiler version in
   lib/bs/compiler-info.json *)
let rec find_compiler_info dir =
  let candidate = Filename.concat dir "compiler-info.json" in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_compiler_info parent

let read_compiler_version info_path =
  try
    match Yojson.Basic.from_file info_path with
    | `Assoc fields -> (
        match List.assoc_opt "version" fields with
        | Some (`String version) -> Some version
        | _ -> None)
    | _ -> None
  with _ -> None

(* CMT files are read from several domains at once *)
let version_cache : (string, string option) Hashtbl.t = Hashtbl.create 16
let version_cache_mutex = Mutex.create ()

(* The version of the compiler that built the CMT file at [path], if known *)
let compiler_version path =
  let dir = Filename.dirname path in
  let dir =
    if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir
    else dir
  in
  Mutex.protect version_cache_mutex (fun () ->
      match Hashtbl.find_opt version_cache dir with
      | Some version -> version
      | None ->
          let version =
            Option.bind (find_compiler_info dir) read_compiler_version
          in
          Hashtbl.replace version_cache dir version;
          version)

(* Read a CMT file and identify the compiler that wrote it. Files in formats
   no supported release writes are rejected before any field of their infos
   is read. A release that writes a known format but has an unknown version
   is supported by the dependency graph but not by value usage analysis. *)
let read_cmt path =
  let cmt_magic, cmt_info = Cmt_format.read_cmt_with_magic path in
  let fields = cmt_infos_fields cmt_info in
  match
    List.filter
      (fun compiler ->
        compiler.cmt_magic = cmt_magic && compiler.cmt_infos_fields = fields)
      all
  with
  | [] ->
      raise
        (Unsupported
           (Printf.sprintf
              "%s was written by an unsupported ReScript compiler (CMT format \
               %s with %d fields). Supported compilers: %s"
              path cmt_magic fields
              (String.concat ", " (List.map (fun c -> c.name) all))))
  | default :: _ as candidates -> (
      match compiler_version path with
      | None -> (default, cmt_info)
      | Some version -> (
          match
            List.find_opt (fun compiler -> compiler.versions version) candidates
          with
          | Some compiler -> (compiler, cmt_info)
          | None ->
              ( {
                  default with
                  name = "ReScript " ^ version;
                  implementation = None;
                },
                cmt_info )))

(* Decode the typed implementation of a CMT file, or [None] for interfaces and
   partial (error-recovery) annotations *)
let implementation compiler (cmt_info : Cmt_format.cmt_infos) =
  match cmt_info.cmt_annots with
  | Cmt_format.Implementation tree -> (
      match compiler.implementation with
      | Some decode -> Some (decode tree)
      | None ->
          raise
            (Unsupported
               (Printf.sprintf "Value usage analysis does not support %s yet"
                  compiler.name)))
  | _ -> None
