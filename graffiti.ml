
open Eliom_pervasives
open HTML5.M
open Eliom_services
open Eliom_parameters
open Eliom_output.Html5

let main_service =
  register_service ~path:[""] ~get_params:unit
    (fun () () -> Lwt.return (html (head (title (pcdata "")) [])
				   (body [h1 [pcdata "OcsiDocs"]])))
