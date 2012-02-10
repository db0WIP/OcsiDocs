
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

(*let error_service =
  Eliom_services.service
  ~path:["error"]
  ~get_params:unit
    ()*)

(* *****                        Users Services                         ***** *)

let connection_service =
  Eliom_services.post_service
    ~fallback:main_service
    ~post_params:(string "name" ** string "password")
    ()

let disconnection_service =
  Eliom_services.post_coservice'
    ~post_params:unit
    ()

let new_user_form_service =
  Eliom_services.service
    ~path:["create account"]
    ~get_params:unit
    ()

let account_confirmation_service =
  Eliom_services.post_coservice
    ~fallback:new_user_form_service
    ~post_params:(string "name" ** string "password")
    ()

(* *****                    Documents Services                         ***** *)

let editdoc_service =
  Eliom_services.service
    ~path:["docs"]
    ~get_params:(suffix (string "doc_name"))
    ()

let createdoc_service =
  Eliom_services.post_service
    ~fallback:main_service
    ~post_params:(string "doc_name")
    ()


(* ************************************************************************* *)
(*                             Data Bases                                    *)
(* ************************************************************************* *)

(* *****                 List of Documents and owners                  ***** *)

let users = ref [("db0", "lolilol")]

let documents = ref [("toto.txt", "db0", true);
                     ("tata.txt", "Korfuri", true);
                     ("tutu.txt", "Thor", true);
                     ("titi.txt", "Vincent", true);
                     ("tete.txt", "Sofia", true);
                     ("tyty.txt", "db0", false);
                    ]

(* *****                           Users                               ***** *)

let username = Eliom_references.eref ~scope:Eliom_common.session None

let wrong_pwd = Eliom_references.eref ~scope:Eliom_common.request false

(* *****                         Exceptions                            ***** *)

exception Document_not_found of string

(* ************************************************************************* *)
(*                              Tools Functions                              *)
(* ************************************************************************* *)

let check_pwd name pwd =
  try List.assoc name !users = pwd
  with Not_found -> false


let is_connected () =
  lwt u = Eliom_references.get username in
  lwt wp = Eliom_references.get wrong_pwd in
  Lwt.return
    (match u with
      | Some s -> true
      | None -> false
    )

let get_document_name (name, _, _) = name
let get_document_author (_, author, _) = author
let get_document_visibility (_, _, visibility) = visibility

let rec get_document_by_name name = function
  | []		-> raise (Document_not_found name)
  | h::t	->
    if ((get_document_name h) = name)
    then h
    else get_document_by_name name t

(* ************************************************************************* *)
(*                          Files Manipulations                              *)
(* ************************************************************************* *)

let read_file filename = 
  lwt chan = Lwt_io.open_file ~mode:Lwt_io.input filename
  in Lwt_io.read_line chan


let createdoc filename =
    lwt chan = Lwt_io.open_file
      ~mode:Lwt_io.input
      ~flags:[Unix.O_WRONLY; Unix.O_EXCL; Unix.O_CREAT]
      ~perm:0o600
      filename
    in Lwt.return true
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

(* connection box *)
let disconnect_box () =
  Eliom_output.Html5.post_form disconnection_service
    (fun _ -> [fieldset
		  [Eliom_output.Html5.string_input
                      ~input_type:`Submit ~value:"Log out" ()]]) ()

let connection_box () =
  lwt u = Eliom_references.get username in
  lwt wp = Eliom_references.get wrong_pwd in
  Lwt.return
    (match u with
      | Some s -> div
	~a:[a_class ["pull-right"]]
	[pcdata "You are connected as "; pcdata s;
                       disconnect_box () ]
      | None ->
        let l =
          [Eliom_output.Html5.post_form
	      ~service:connection_service
	      ~a:[a_class ["pull-right"]]
              (fun (name1, name2) ->
		[Eliom_output.Html5.string_input
		       ~input_type:`Text
		       ~a:[a_class ["input-small"]]
		       ~name:name1 ();
		 pcdata " ";
                     Eliom_output.Html5.string_input
		       ~input_type:`Password
		       ~a:[a_class ["input-small"]]
		       ~name:name2 ();
		 pcdata " ";
                     Eliom_output.Html5.string_input
		       ~input_type:`Submit 
		       ~a:[a_class ["btn primary"]]
		       ~value:"Connect" ()
                    ]) ()]
        in
        if wp
        then div ((p [em [pcdata "Wrong user or password"]])::l)
        else div l
    )

let create_account_form () =
  Eliom_output.Html5.post_form ~service:account_confirmation_service
    (fun (name1, name2) ->
      [h1 [pcdata "Create an account"];
	    fieldset
	      [label (*~a:[Eliom_output.Html5.a_for name1]*) [pcdata "login: "];
               Eliom_output.Html5.string_input ~input_type:`Text ~name:name1 ();
               br ();
               label (*~a:[Eliom_output.Html5.a_for name2]*) [pcdata "password: "];
               Eliom_output.Html5.string_input ~input_type:`Password ~name:name2 ();
               br ();
               Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Connect" ()
              ]]
      ) ()

(* *****                         Menu Bar                              ***** *)

let menu_bar () =
  lwt cf = connection_box () in
  Lwt.return
    (
      div ~a:[a_class ["topbar"]]
	[div ~a:[a_class ["fill"]]
	    [div ~a:[a_class ["container"]]
		[Eliom_output.Html5.a
		    ~a:[a_class ["brand"]]
		    ~service:main_service [pcdata "OcsiDocs"] ();
		 (* todo: list map on a list*)
		 ul [li [Eliom_output.Html5.a
                            ~service:new_user_form_service [pcdata "Create account"] ()]];
		 cf
		]
	    ]
	]
    )
    
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


(* *****                       Documents List                          ***** *)

let doclist () =
  ul (List.map (fun (name, _, _) -> (* todo: only my files or public files *)
    li [Eliom_output.Html5.a
           ~service:editdoc_service [pcdata name] name])
        !documents)


(* *****                           Main Page                           ***** *)

let main_page () =
  lwt ic = is_connected () in
  Lwt.return
    (
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
	     h1 [pcdata
		    (match ic with
		      | true -> "lol"
		      | _ -> "mdr"
		    )];
       (*      h1 [pcdata (match (is_connected ()) with
	       | true -> "Tu es connecte"
	       | false ->  "Tu es pas connecte")];*)
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
    )


(* *****                           Edition Page                        ***** *)

let editdoc doc_name =
  lwt doc_content = read_file ("docs/"^doc_name) in
  Lwt.return
    (
      div ~a:[a_class ["span10"]]
	[h1 [pcdata doc_name];
	 h5 [pcdata "Owner : "];
	 p [pcdata (get_document_author (get_document_by_name doc_name !documents))];
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
      lwt mb = menu_bar ()
     and mp = main_page () in
      Lwt.return
        (html
	 (html_header ())
	  (body [mb;
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		    [div ~a:[a_class ["row"]]
		    [mp;
		     left_column ()]
		  ];
		 ];
		footer ()
		])));

(* *****                        Users Services                         ***** *)

  Eliom_output.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_references.set username (Some name)
      else Eliom_references.set wrong_pwd true);

  Eliom_output.Action.register
    ~service:disconnection_service
    (fun () () -> Eliom_state.discard ~scope:Eliom_common.session ());

  Eliom_output.Html5.register
    ~service:new_user_form_service
    (fun doc_name () ->
      lwt mb = menu_bar () in
      Lwt.return
        (html
	 (html_header ())
	  (body [mb;
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		      [div ~a:[a_class ["row"]]
			  [div ~a:[a_class ["span10"]]
			      [create_account_form ()];
			   left_column ()]]
		  ];
		 footer ()
		];
	  )));

  Eliom_output.Html5.register
    ~service:account_confirmation_service
    (fun () (name, pwd) ->
      let create_account_service =
        Eliom_output.Action.register_coservice
          ~fallback:main_service
          ~get_params:Eliom_parameters.unit
          ~timeout:60.
          (fun () () ->
            users := (name, pwd)::!users;
            Lwt.return ())
      in
      lwt mb = menu_bar () in
    Lwt.return
        (html
	 (html_header ())
	  (body [mb;
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		      [div ~a:[a_class ["row"]]
			  [div ~a:[a_class ["span10"]]
			      [h1 [pcdata "Confirm account creation for "; pcdata name];
                     p [Eliom_output.Html5.a ~service:create_account_service [pcdata "Yes"] ();
                        pcdata " ";
                        Eliom_output.Html5.a ~service:main_service [pcdata "No"] ()]];
			   left_column ()]]
		  ];
		 footer ()
		];
	  )));

(* *****                    Documents Services                         ***** *)

  Eliom_output.Html5.register
    ~service:editdoc_service
    (fun doc_name () ->
      lwt editdocform = editdoc doc_name in
      lwt mb = menu_bar () in
      Lwt.return
        (html
	 (html_header ())
	  (body [mb;
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
        if ((createdoc ("docs/"^doc_name)) == Lwt.return true)
        then Lwt.return
	  (Eliom_services.preapply
	    ~service:editdoc_service
	    doc_name
	  )
(*        else Lwt.return error_service) (* todo: give error type *)*)
        else Lwt.return main_service) (* todo: give error type *)
  
