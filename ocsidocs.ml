
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

let createdoc_service =
  Eliom_services.post_service
  ~fallback:main_service
  ~post_params:(string "doc_name")
  ()

(* ************************************************************************* *)
(*                                Functions                                  *)
(* ************************************************************************* *)

(* HTML Header *)
let html_header () =
         head
	  (title (pcdata "OcsiDocs"))
	  [ Eliom_output.Html5_forms.css_link
	  ~uri:(Eliom_output.Html5.make_uri (Eliom_services.static_dir ())
					    ["css";"bootstrap.css"]) ();
	    Eliom_output.Html5_forms.css_link
	    ~uri:(Eliom_output.Html5.make_uri (Eliom_services.static_dir ())
					      ["css";"style.css"]) ();]

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

(* create doc form *)
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

(* list of documents and proprietarys *)
let documents = ref [("toto.txt", "db0");
                     ("tata.txt", "Korfuri");
                     ("tutu.txt", "Thor");
                     ("titi.txt", "Vincent");
                     ("tete.txt", "Sofia");
                     ("tyty.txt", "db0");
                     ]

(* Doc list *)
let doclist () =
  ul (List.map (fun (name, _) -> 
                  li [Eliom_output.Html5.a
                        ~service:main_service [pcdata name] ()])
               !documents)

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

(* main page *)
let main_page () =
  div ~a:[a_class ["container"]]
    [
      div ~a:[a_class ["content"]]
        [
          div ~a:[a_class ["row"]]
            [
              div ~a:[a_class ["span10"]]
                [
                  div ~a:[a_class ["row"]]
                  [
                  div ~a:[a_class ["span5"]]
                    [
                     br (); br ();
                     img
                       ~alt:"Ocsigen"
                       ~src:(Eliom_output.Xhtml.make_uri
                       ~service:(static_dir ()) ["img";"ocsidocs_small.png"])
                       ();
		     ];
		  div ~a:[a_class ["span4"]]
                    [
                      br (); br (); br ();
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
		 ];
              div ~a:[a_class ["span4"]]
                [h4 [pcdata "Availables Files"];
                 doclist ()]
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
	 (html_header ())
	  (body [menu_bar ();
		 main_page ();
		 footer ()
		 ])));

  Eliom_output.Html5.register
    ~service:createdoc_service
    (fun () (doc_name) ->
       Lwt.return
        (html (head (title (pcdata "OcsiDocs : Document ")) [])
	      (body [
		     h1 [pcdata doc_name];
		    ])))
         
