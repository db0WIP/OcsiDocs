
type doc

val add_document : string -> string -> unit
val get_document_name : doc -> string
val get_document_author : doc -> string
val get_document_by_name : string -> doc
val get_documents_by_author : string -> doc list
val get_documents_by_authors : string list -> doc list
val get_public_documents : unit -> doc list
