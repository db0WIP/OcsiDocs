
type doc = (string * string)

let documents = ref [("toto.txt", "db0");
                     ("tata.txt", "Korfuri");
                     ("tutu.txt", "Thor");
                     ("titi.txt", "Vincent");
                     ("tete.txt", "Sofia");
                     ("tyty.txt", "db0");
                    ]

let add_document name author = documents := (name, author)::!documents

let get_document_name (name, _) = name
let get_document_author (_, author) = author

let get_document_by_name name =
  List.find (fun (n, _) -> if (n = name) then true else false) !documents

let get_documents_by_author author =
  List.filter (fun (_, a) -> if (a = author) then true else false) !documents

let get_documents_by_authors authors =
  List.flatten (List.map get_documents_by_author authors)


