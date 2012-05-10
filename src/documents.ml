
(* ************************************************************************* *)
(* *****                           Documents Module                    ***** *)
(* ************************************************************************* *)

type doc = (string * string)

let documents = ref []
  
(* *****                       Get document informations               ***** *)

let get_document_name (name, _) = name
let get_document_author (_, author) = author

let get_document_raw_path name author =
  ("docs/" ^ author ^ "/" ^ name)

let get_document_path doc =
  get_document_raw_path (get_document_name doc) (get_document_author doc)

let get_document_content doc =
  Ofile.string_of_file2 (get_document_path doc)

let get_document_content_list doc =
  Ofile.list_of_file (get_document_path doc)

(* *****                       Get document by information            ***** *)

let get_document_by_name name =
  List.find (fun (n, _) -> n = name) !documents

let get_documents_by_author author =
  List.filter (fun (_, a) -> a = author) !documents

let get_documents_by_authors authors =
  List.flatten (List.map get_documents_by_author authors)

let get_public_documents () =
  get_documents_by_author "public"

(* *****                       Change document content                 ***** *)

let update_document doc new_content =
  Ofile.string_to_file new_content (get_document_path doc)

let update_document_from_list doc l =
  Ofile.list_to_file l (get_document_path doc)

(* *****                       Tools                                   ***** *)

let allow_document_name name = true

(* *****                       Add document                            ***** *)

let add_document_in_db name author =
  documents := (name, author)::!documents

let add_document name author =
  if (allow_document_name name
      && Ofile.create_file (get_document_raw_path name author))
  then let _ = add_document_in_db name author in
       let _ = Ofile.string_to_file "Edit me now!\n" (get_document_raw_path name author) in
       true
  else false
