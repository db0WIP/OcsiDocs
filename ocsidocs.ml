
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

(* edit doc form *)

let editdoc_form () =
  Eliom_output.Html5.post_form
  ~service:editdoc_service
  (fun (doc_name, doc_content) ->
    [p [pcdata "Document name :";
        Eliom_output.Html5.string_input
          ~input_type:`Text
          ~name:doc_name
	  ();
        br ();
	pcdata "Document content :";
	Eliom_output.Html5.string_input
	  ~input_type:`Text
	  ~name:doc_content
	  ();
	br ();
	Eliom_output.Html5.string_input
	  ~input_type:`Submit
	  ~value:"Ajouter un document"
	  ()
	]]) ()


(* ************************************************************************* *)
(*                          Services definitions                             *)
(* ************************************************************************* *)

let _ =

  Eliom_output.Html5.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (html (head (title (pcdata "OcsiDocs")) [])
	      (body [h1 [pcdata "OcsiDocs"];
                     p [pcdata "hello world"];
		     editdoc_form ()])));


  Eliom_output.Html5.register
    ~service:editdoc_service
    (fun () (doc_name, doc_content) ->
       Lwt.return
        (html (head (title (pcdata "OcsiDocs : Document ")) [])
	      (body [
		     h1 [pcdata doc_name];
                     p [pcdata doc_content]
		     
		    ])))
         
