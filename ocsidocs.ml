
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
  Eliom_services.post_service
  ~fallback:main_service
  ~post_params:(string "doc_name" ** string "doc_content")
  ()

(* ************************************************************************* *)
(*                                Functions                                  *)
(* ************************************************************************* *)

let users = ref [("Calvin", "123"); ("Hobbes", "456")]

(* menu bar *)
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

(* edit doc form *)

let editdoc_form () =
  Eliom_output.Html5.post_form
  ~service:editdoc_service
  (fun (doc_name, doc_content) ->
    [p ~a:[a_class ["center_form"]]
       [pcdata "Document name :";
        br ();
        br ();
        Eliom_output.Html5.string_input
          ~input_type:`Text
          ~name:doc_name
	  ();
        br ();
        br ();
(*	pcdata "Document content : ";
	Eliom_output.Html5.string_input
	  ~input_type:`Text
	  ~name:doc_content
	  ();
	br ();
*)
	Eliom_output.Html5.string_input
	  ~input_type:`Submit
	  ~value:"Add a new document"
	  ()
	]]) ()

(* main page *)
let main_page () =
  div ~a:[a_class ["container"]]
    [
      div ~a:[a_class ["content"]]
        [
          div ~a:[a_class ["row"]]
            [
              div ~a:[a_class ["span10"]]
                [editdoc_form ()];
              div ~a:[a_class ["span4"]]
                [h3 [pcdata "stuff"]]
            ]
        ]
    ]

(* footer *)
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
	 (head
	  (title (pcdata "OcsiDocs"))
	  [ Eliom_output.Html5_forms.css_link
	  ~uri:(Eliom_output.Html5.make_uri (Eliom_services.static_dir ())
					    ["css";"bootstrap.css"]) ();
	    Eliom_output.Html5_forms.css_link
	    ~uri:(Eliom_output.Html5.make_uri (Eliom_services.static_dir ())
					      ["css";"style.css"]) ();
					      ])
	  (body [menu_bar ();
		 main_page ();
		 footer ()
		 ])));

  Eliom_output.Html5.register
    ~service:editdoc_service
    (fun () (doc_name, doc_content) ->
       Lwt.return
        (html (head (title (pcdata "OcsiDocs : Document ")) [])
	      (body [
		     h1 [pcdata doc_name];
                     p [pcdata doc_content]
		     
		    ])))
         
