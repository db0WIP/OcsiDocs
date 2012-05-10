
(* ************************************************************************* *)
(* *****                           Documents Module                    ***** *)
(* ************************************************************************* *)

type doc

(* *****                       Get document informations               ***** *)

val get_document_name : doc -> string
val get_document_author : doc -> string
val get_document_raw_path : string -> string -> string
val get_document_path : doc -> string
val get_document_content : doc -> string
val get_document_content_list : doc -> string list

(* *****                       Get document by information            ***** *)

val get_document_by_name : string -> doc
val get_documents_by_author : string -> doc list
val get_documents_by_authors : string list -> doc list
val get_public_documents : unit -> doc list

(* *****                       Change document content                 ***** *)

val update_document : doc -> string -> unit
val update_document_from_list : doc -> string list -> unit

(* *****                       Tools                                   ***** *)

val allow_document_name : string -> bool

(* *****                       Add document                            ***** *)

val add_document_in_db : string -> string -> unit
val add_document : string -> string -> bool
