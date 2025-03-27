open Stdlib

(** Parser module for CMT files and module dependencies *)

type module_info = {
  name : string;
  dependencies : string list;
  interface_digest : Digest.t option;
  implementation_digest : Digest.t option;
  file_path : string option;
}
(** Module info representation *)

exception Invalid_cmt_file of string
(** Exception for invalid cmt files *)

val parse_cmt_file : ?verbose:bool -> ?skip_cache:bool -> string -> module_info
(** Parse a single cmt file and extract module information
    @param verbose Enable verbose output
    @param skip_cache Skip using the cache
    @param path Path to the cmt file
    @return Module info structure *)

val parse_files_or_dirs :
  ?verbose:bool -> ?skip_cache:bool -> string list -> module_info list
(** Parse a list of files or directories and extract module information
    @param verbose Enable verbose output
    @param skip_cache Skip using the cache
    @param paths List of files or directories to parse
    @return List of module info structures *)

val clear_cache : unit -> unit
(** Clear the module info cache *)

val set_cache_file : string -> unit
(** Set the cache file path
    @param path Path to the cache file *)

val set_skip_cache : bool -> unit
(** Set the skip cache flag
    @param flag Whether to skip using the cache *)
