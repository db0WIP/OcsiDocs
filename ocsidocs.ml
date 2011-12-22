
open Eliom_pervasives
open HTML5.M
open Eliom_services
open Eliom_parameters

(* ************************************************************************* *)
(*                          Services declarations                            *)
(* ************************************************************************* *)

let main_service =
  Eliom_services.service
  ~path:[""]
  ~get_params:unit
  ()

let editdoc_service =
  Eliom_services.service
  ~path:["documents"]
  ~get_params:(suffix (string "doc_name")) ()

(* todo: Try to implement as an action *)
let createdoc_service =
  Eliom_services.post_service
  ~fallback:main_service
  ~post_params:(string "doc_name")
  ()

(* ************************************************************************* *)
(*                          Files Manipulations                              *)
(* ************************************************************************* *)

let read_file filename = 
  lwt chan = Lwt_io.open_file ~mode:Lwt_io.input filename
  in Lwt_io.read_line chan

(*let read_file2 filename =
  let filein = open_in filename in
    let 
    input_line filein
*)



(* todo: raise exception
  let filein = open_in filename in
    try input_line filein
      let line = input_line filein in
        close_in filein;
        line
    with e ->
    close_in_noerr filein;
    raise e
*)
(*
(* Using Sys.readdir. *)
(*let totolol () =
  Array.iter
    (fun file ->
       let path = Filename.concat dirname file in
       (* do something with path *)
       ())
    (Sys.readdir dirname)
 
(*-----------------------------*)
 
(* Using Unix.opendir, readdir, and closedir. Note that the "." and ".."
   directories are included in the result unlike with Sys.readdir. *)
#load "unix.cma";;
 
let () =
  let dir =
    try Unix.opendir dirname
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "can't opendir %s: %s\n"
        dirname (Unix.error_message e);
      exit 255 in
  try
    while true do
      let file = Unix.readdir dir in
      let path = Filename.concat dirname file in
      (* do something with path *)
      ()
    done
  with End_of_file ->
    Unix.closedir dir
 
(*-----------------------------*)
 
(* Get a list of full paths to plain files. *)
let plainfiles dir =
  List.filter
    (fun path ->
       match Unix.lstat path with
         | {Unix.st_kind=Unix.S_REG} -> true
         | _ -> false)
    (List.map
       (Filename.concat dir)
       (Array.to_list (Sys.readdir dir)))
*)
*)

(* ************************************************************************* *)
(*                            Display Functions                              *)
(* ************************************************************************* *)

(* *****                        HTML Header                            ***** *)

let html_header () =
  head
    (title (pcdata "OcsiDocs"))
    [ Eliom_output.Html5_forms.css_link
      ~uri:(Eliom_output.Html5.make_uri (Eliom_services.static_dir ())
      ["css";"bootstrap.css"]) ();
       Eliom_output.Html5_forms.css_link
       ~uri:(Eliom_output.Html5.make_uri (Eliom_services.static_dir ())
        ["css";"style.css"]) ();]


(* *****                         Menu Bar                              ***** *)

let menu_bar () =
  div ~a:[a_class ["topbar"]]
    [div ~a:[a_class ["fill"]]
      [div ~a:[a_class ["container"]]
        [Eliom_output.Html5.a
                ~a:[a_class ["brand"]]
                ~service:main_service [pcdata "OcsiDocs"] ()
(* todo: here will be the menu*)
(* todo: here will be the login form*)
        ]
      ]
    ]


(* *****                       Create Doc Form                         ***** *)

let createdoc_form () =
  Eliom_output.Html5.post_form
  ~service:createdoc_service
  (fun (doc_name) ->
    [p ~a:[a_class ["center_form"]]
       [pcdata "Document name :";
        br (); br ();
        Eliom_output.Html5.string_input
	  ~a:[a_class ["span3"]]
          ~input_type:`Text
          ~name:doc_name
	  ();
        br (); br ();
	Eliom_output.Html5.string_input
	  ~input_type:`Submit
          ~a:[a_class ["btn secondary"]]
	  ~value:"Add a new document"
	  ()
	]]) ()


(* *****                 List of Documents and proprietarys            ***** *)

let documents = ref [("toto.txt", "db0");
                     ("tata.txt", "Korfuri");
                     ("tutu.txt", "Thor");
                     ("titi.txt", "Vincent");
                     ("tete.txt", "Sofia");
                     ("tyty.txt", "db0");
                     ]


(* *****                       Documents List                          ***** *)

let doclist () =
  ul (List.map (fun (name, _) -> 
                  li [Eliom_output.Html5.a
                        ~service:editdoc_service [pcdata name] name])
               !documents)


(* *****                           Main Page                           ***** *)

let main_page () =
  div ~a:[a_class ["span10"]]
   [div ~a:[a_class ["row"]]
     [div ~a:[a_class ["span5"]]
       [br (); br ();
        img ~alt:"Ocsigen"
            ~src:(Eliom_output.Xhtml.make_uri
		  ~service:(static_dir ())
	    ["img";"ocsidocs_small.png"])
	    ();
       ];
       div ~a:[a_class ["span4"]]
        [br (); br (); br ();
	 h3 ~a:[a_class ["center"]]
	 [pcdata "Your documents in the cloud with OcsiDocs !"];
	 br (); br ();
	 p ~a:[a_class ["center"]]
	 [pcdata "OcsiDocs is an online";
	 br ();
	 pcdata "collaborating text editor."];
	 createdoc_form ();
     ];
    ];
   ]


(* *****                           Edition Page                        ***** *)

let editdoc doc_name =
  lwt doc_content = read_file ("docs/"^doc_name) in
  Lwt.return
    (
      div ~a:[a_class ["span10"]]
	[h1 [pcdata doc_name];
	 h5 [pcdata "Owner : "];
	 p [pcdata (List.assoc doc_name !documents)];
	 h5 [pcdata "Content : "];
	 p [pcdata doc_content]]
    )


(* *****                           Left Column                         ***** *)

let left_column () =
  div ~a:[a_class ["span4"]]
    [h4 [pcdata "Availables Documents"];
     doclist ()]


(* *****                             Footer                            ***** *)

let footer () =
  div ~a:[a_class ["footer"]]
    [p [pcdata "(c) OcsiDocs 2011 - Made with ";
        Eliom_output.Html5.a
          (Eliom_services.external_service
             ~prefix:"http://ocsigen.org"
             ~path:[""]
             ~get_params:(suffix (all_suffix "suff"))
             ())
          [pcdata "Ocsigen"] [""];
        pcdata ", web server and powerfull framework in ";
        Eliom_output.Html5.a
          (Eliom_services.external_service
             ~prefix:"http://caml.inria.fr"
             ~path:[""]
             ~get_params:(suffix (all_suffix "suff"))
             ())
          [pcdata "OCaml"] [""];
        br ();
        pcdata "This project is open source : ";
        Eliom_output.Html5.a
          (Eliom_services.external_service
             ~prefix:"https://github.com"
             ~path:["db0company"]
             ~get_params:(suffix (all_suffix "suff"))
             ())
          [pcdata "Source code available on GitHub"] ["OcsiDocs"];
       ]]


(* ************************************************************************* *)
(*                          Services definitions                             *)
(* ************************************************************************* *)

let _ =

  Eliom_output.Html5.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (html
	 (html_header ())
	  (body [menu_bar ();
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		    [div ~a:[a_class ["row"]]
		    [main_page ();
		     left_column ()]
		  ];
		 ];
		footer ()
		])));

  Eliom_output.Html5.register
    ~service:editdoc_service
    (fun doc_name () ->
      lwt editdocform = editdoc doc_name in
      Lwt.return
        (html
	 (html_header ())
	  (body [menu_bar ();
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		    [div ~a:[a_class ["row"]]
		     [editdocform;
		     left_column ()]]
		  ];
		footer ()
		];
	       )));

  Eliom_output.Redirection.register
      ~service:createdoc_service
      ~options:`Permanent
      (fun () doc_name ->
	print_endline "sdfdfd";
        if (*createdoc doc_name*)true
        then Lwt.return
	  (Eliom_services.preapply
	    ~service:editdoc_service
	    doc_name
	  )
        else Lwt.return main_service)

