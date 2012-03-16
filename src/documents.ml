
type doc = (string * string)

let documents = ref []

let allow_document_name name = true
  
let add_document_in_db name author =
  documents := (name, author)::!documents

let add_document name author =
  if (allow_document_name name
      && Ofile.create_file ("docs/" ^ author ^ "/" ^ name))
  then let _ = add_document_in_db name author in true
  else false

let get_document_name (name, _) = name
let get_document_author (_, author) = author

let get_document_by_name name =
  List.find (fun (n, _) -> if (n = name) then true else false) !documents

let get_documents_by_author author =
  List.filter (fun (_, a) -> if (a = author) then true else false) !documents

let get_documents_by_authors authors =
  List.flatten (List.map get_documents_by_author authors)

let get_public_documents () =
  get_documents_by_author "public"

let update_document doc new_content =
  Ofile.string_to_file new_content ("docs/" ^ (get_document_author doc)
				    ^ "/" ^ (get_document_name doc))
