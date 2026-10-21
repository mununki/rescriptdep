(* compilers.ml - ReScript compiler releases whose build artifacts are supported

   The dependency graph only reads fields of cmt_infos that every release below
   lays out the same way, plus the .ast header, whose format is shared too.
   Only the typed tree used by value usage analysis differs between releases,
   so each entry says how to recognize the release from the CMT file itself
   and how to decode its typed tree. *)

open Stdlib

type t = {
  name : string;
  cmt_magic : string;  (** Magic number of the typed tree section *)
  cmt_infos_fields : int;  (** Number of fields of the cmt_infos record *)
  implementation : (Obj.t -> Typed_ast.item list) option;
      (** [None] when value usage analysis does not support the release *)
}

let all =
  [
    {
      name = "ReScript 11";
      cmt_magic = "Caml1999T022";
      cmt_infos_fields = 13;
      implementation = Some Compiler_v11.implementation;
    };
    {
      name = "ReScript 12";
      cmt_magic = "Caml1999T022";
      cmt_infos_fields = 14;
      implementation = Some Compiler_v12.implementation;
    };
    {
      (* The typed tree still changes between the 13.0 alpha releases *)
      name = "ReScript 13";
      cmt_magic = "Caml1999T037";
      cmt_infos_fields = 14;
      implementation = None;
    };
  ]

exception Unsupported of string

let cmt_infos_fields (cmt_info : Cmt_format.cmt_infos) =
  Obj.size (Obj.repr cmt_info)

let find ~cmt_magic cmt_info =
  let fields = cmt_infos_fields cmt_info in
  List.find_opt
    (fun compiler ->
      compiler.cmt_magic = cmt_magic && compiler.cmt_infos_fields = fields)
    all

(* Read a CMT file and identify the compiler that wrote it. Files written by
   unknown releases are rejected before any field of their infos is read. *)
let read_cmt path =
  let cmt_magic, cmt_info = Cmt_format.read_cmt_with_magic path in
  match find ~cmt_magic cmt_info with
  | Some compiler -> (compiler, cmt_info)
  | None ->
      raise
        (Unsupported
           (Printf.sprintf
              "%s was written by an unsupported ReScript compiler (CMT format \
               %s with %d fields). Supported compilers: %s"
              path cmt_magic
              (cmt_infos_fields cmt_info)
              (String.concat ", " (List.map (fun c -> c.name) all))))

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
