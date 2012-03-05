
open Unix
open ExtLib

val string_of_file : string -> string Lwt.t
val list_of_file : string -> string list
val create_file : string -> unit
val list_of_directory : string -> string list
val string_to_file : string -> string -> unit
val create_directory : string -> unit
