
open Eliom_pervasives
open HTML5.M
open Eliom_services
open Eliom_parameters

module My_appl =
  Eliom_output.Eliom_appl (struct
    let application_name = "ocsidocs"
  end)


{client{

(*  let _ = Dom_html.window##alert(Js.string "Salut")*)
 }}

{shared{
 }}

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

let editdocpage_service =
  Eliom_services.service
    ~path:["docs"]
    ~get_params:(suffix (string "author" ** string "doc_name"))
    ()

let editdoc_service =
  Eliom_services.post_service
    ~fallback:editdocpage_service
    ~post_params:(set string "content")
    ()

let createdoc_service =
  Eliom_services.post_service
    ~fallback:main_service
    ~post_params:(string "doc_name")
    ()

(* ************************************************************************* *)
(*                             Data Bases                                    *)
(* ************************************************************************* *)

(* *****                         Init documents db                     ***** *)

let init_users_db () =
  let get_password user = Ofile.string_of_file_noendline ("docs/" ^ user ^ "/.password") in
  List.iter
    (fun user ->
      if (user <> "public")
      then Users.add_user_in_db user (get_password user)
      else ())
    (Ofile.list_of_directory ("docs")
)

let init_document_db () =
  List.iter
    (fun user -> 
      List.iter
	(fun filename ->
	  Documents.add_document_in_db filename user
	)
	(Ofile.list_of_directory ("docs/" ^ user))
    )
    (Ofile.list_of_directory "docs")

(* *****                         Exceptions                            ***** *)

exception Document_not_found of string

exception Not_Connected

(* *****                           User logged info                    ***** *)

let username = Eliom_references.eref ~scope:Eliom_common.session None

let wrong_pwd = Eliom_references.eref ~scope:Eliom_common.request false

(* ************************************************************************* *)
(*                         Use  Tools Functions                              *)
(* ************************************************************************* *)

let is_connected () =
  lwt u = Eliom_references.get username in
  Lwt.return
    (match u with
      | Some s -> true
      | None -> false
    )

let get_username () =
  lwt u = Eliom_references.get username in
  Lwt.return
    (match u with
      | Some s -> s
      | None -> "public"
    )

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
	[em [pcdata "You are connected as "; pcdata s];
         disconnect_box () ]
      | None ->
        let l =
          [Eliom_output.Html5.post_form
	      ~service:connection_service
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
        then div ~a:[a_class ["pull-right"]] ((em [pcdata "Wrong user or password"])::l)
        else div ~a:[a_class ["pull-right"]] l
    )

let create_account_form () =
  Eliom_output.Html5.post_form ~service:account_confirmation_service
    (fun (name1, name2) ->
      [h1 [pcdata "Create an account"];
	    fieldset
	      [label [pcdata "login: "];
               Eliom_output.Html5.string_input ~input_type:`Text ~name:name1 ();
               br ();
               label [pcdata "password: "];
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
	[
	 div ~a:[a_class ["fill"]]
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
  lwt ic = is_connected () in
  lwt username = get_username () in
  let userlist =
    match ic with
      | true	-> ["public"; username]
      | false	-> ["public"]
  in
  Lwt.return
    (
      div
      [ul (List.map
	    (fun doc ->
	      let name = Documents.get_document_name doc
	      and author = Documents.get_document_author doc in
	      let link = ("[" ^ author ^ "] " ^ name) in
	      li [Eliom_output.Html5.a
		     ~service:editdocpage_service [pcdata link] (author, name)])
            (Documents.get_documents_by_authors userlist))]
    )

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

let editdocform author doc = 
  let s = p [pcdata "You are editing this document and I know it because I'ms using js_of_ocaml like a boss"] in
  let rec textarealist n content_name = function
    | []	-> []
    | str::t	-> 
      (p ~a:[a_class ["numerotation"]] [pcdata (string_of_int n)])
      ::(Eliom_output.Html5.string_input
	   ~input_type:`Text
	   ~a:[a_class ["textedit"];
	       a_onclick {{ Dom.appendChild (Dom_html.document##body)
			    (Eliom_client.Html5.of_p %s) }}]
	   ~value:str
	   ~name:content_name
	   ())
	::(br ())
	::(textarealist (n + 1) content_name t)
  in
  Lwt.return
    (div [
      (Eliom_output.Html5.post_form
	 ~service:editdoc_service
	 (fun (content_name) ->
	   [
            Eliom_output.Html5.string_input
	      ~input_type:`Submit 
	      ~a:[a_class ["btn btn-inverse"]]
	      ~value:"Save" ();
	    br ();
	    br ();
	    br ()]@
	     textarealist 1 content_name (Documents.get_document_content_list doc)
	    @[br ();
            Eliom_output.Html5.string_input
	      ~input_type:`Submit 
	      ~a:[a_class ["btn btn-inverse"]]
	      ~value:"Save" ()
	   ]) (author, Documents.get_document_name doc)
      )])

let editdocpage author doc_name =
  let doc = Documents.get_document_by_name doc_name in
  lwt edf = editdocform author doc in
  Lwt.return
    (
      div ~a:[a_class ["span10"]]
	[h1 [pcdata doc_name];
	 h5 [pcdata "Owner : "];
	 p [pcdata (Documents.get_document_author doc)];
	 h5 [pcdata "Content : "];
	 div ~a:[a_class ["editdoc"]]
	   [edf]
	]
    )


(* *****                           Left Column                         ***** *)

let left_column () =
  lwt dc = doclist () in
  Lwt.return
    (
      div ~a:[a_class ["span4"]]
	[h4 [pcdata "Availables Documents"];
	 dc]
    )

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

let define_services () =

  My_appl.register
    ~service:main_service
    (fun () () ->
      lwt mb = menu_bar ()
     and mp = main_page ()
     and lc = left_column ()
     in
      Lwt.return
        (html
	 (html_header ())
	  (body [mb;
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		    [div ~a:[a_class ["row"]]
		    [mp;
		     lc]
		  ];
		 ];
		footer ()
		])));

(* *****                        Users Services                         ***** *)

  Eliom_output.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if Users.check_pwd name password
      then Eliom_references.set username (Some name)
      else Eliom_references.set wrong_pwd true);

  Eliom_output.Action.register
    ~service:editdoc_service
    (fun (author, doc_name) ->
    (fun new_content ->
      Lwt.return
	(Documents.update_document_from_list (Documents.get_document_by_name doc_name) new_content)
    ));

  Eliom_output.Action.register
    ~service:disconnection_service
    (fun () () -> Eliom_state.discard ~scope:Eliom_common.session ());

  My_appl.register
    ~service:new_user_form_service
    (fun doc_name () ->
      lwt mb = menu_bar ()
     and lc = left_column ()
     in
      Lwt.return
        (html
	 (html_header ())
	  (body [mb;
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		      [div ~a:[a_class ["row"]]
			  [div ~a:[a_class ["span10"]]
			      [create_account_form ()];
			   lc]]
		  ];
		 footer ()
		];
	  )));

  My_appl.register
    ~service:account_confirmation_service
    (fun () (name, pwd) ->
      let create_account_service =
        Eliom_output.Action.register_coservice
          ~fallback:main_service
          ~get_params:Eliom_parameters.unit
          ~timeout:60.
          (fun () () ->
            (Users.add_user name pwd);
            Lwt.return ())
      in
      lwt mb = menu_bar ()
     and lc = left_column ()
     in
    Lwt.return
        (html
	 (html_header ())
	  (body [mb;
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		      [div ~a:[a_class ["row"]]
			  [div ~a:[a_class ["span10"]]
			      (* todo : the username must not start by . or be public *)
			      [h1 [pcdata "Confirm account creation for "; pcdata name];
                     p [Eliom_output.Html5.a ~service:create_account_service [pcdata "Yes"] ();
                        pcdata " ";
                        Eliom_output.Html5.a ~service:main_service [pcdata "No"] ()]];
			   lc]]
		  ];
		 footer ()
		];
	  )));

(* *****                    Documents Services                         ***** *)

  My_appl.register
    ~service:editdocpage_service
    (fun (author, doc_name) () ->
      lwt editdocpageform = editdocpage author doc_name in
      lwt mb = menu_bar ()
     and lc = left_column () in
      Lwt.return
        (html
	 (html_header ())
	  (body [mb;
		 div ~a:[a_class ["container"]]
		  [div ~a:[a_class ["content"]]
		    [div ~a:[a_class ["row"]]
		     [editdocpageform;
		     lc]]
		  ];
		footer ()
		];
	       )));

  Eliom_output.Redirection.register
      ~service:createdoc_service
      ~options:`Permanent
      (fun () doc_name ->
	lwt username = get_username () in
	if ((Documents.add_document doc_name username) == true)
	then Lwt.return
	  (Eliom_services.preapply
	    ~service:editdocpage_service (username, doc_name)
	  )
(*        else Lwt.return error_service) (* todo: give error type *)*)
	else Lwt.return main_service) (* todo: give error type *)
(* todo: name must not start with a . *)  


(* ************************************************************************* *)
(*                             Initialisation                                *)
(* ************************************************************************* *)

let main () =
  begin
    init_users_db ();
    init_document_db ();
    define_services ();
    Users.dump_users ()
  end

let _ = main ()

