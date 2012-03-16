
open Unix
open ExtLib

val string_of_file : string -> string Lwt.t
val list_of_file : string -> string list
val string_of_file_noendline : string -> string
val create_file : string -> bool
val list_of_directory : string -> string list
val string_to_file : string -> string -> unit
val create_directory : string -> unit
