
type doc

val add_document_in_db : string -> string -> unit
val add_document : string -> string -> bool

val get_document_name : doc -> string
val get_document_author : doc -> string
val get_document_by_name : string -> doc
val get_documents_by_author : string -> doc list
val get_documents_by_authors : string list -> doc list
val get_public_documents : unit -> doc list

val update_document : doc -> string -> unit
