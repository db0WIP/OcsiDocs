(* ocamlc -I +extlib extLib.cma unix.cma file.ml *)


open Unix
open ExtLib

let string_of_file file =
  Lwt.return (Std.input_file ~bin:false file)

let list_of_file file =
  Std.input_list (open_in file)

let string_of_file_noendline file =
  List.hd (list_of_file file)

let create_file filename =
  match (try Some (open_out filename)
         with Sys_error s -> None)
  with
    | None	-> false
    | Some s	-> let _ = close_out s in true

let list_of_directory dirname =
  let rec list_directory_aux dir acc =
    try
      let file = readdir dir
      in list_directory_aux dir (file::acc)
    with End_of_file -> acc
  in let handle = opendir dirname
     in let filelist = list_directory_aux handle []
	in List.filter (fun filename -> filename.[0] != '.') filelist

let string_to_file s filename =
  let outchan = open_out filename in
  begin
    output_string outchan s;
    flush outchan;
    close_out outchan
  end

let create_directory path =
  mkdir path 0o777

(*let string_to_file s filename =
  let outlol = open_out_gen [Open_creat;Open_append;Open_wronly] 0o664 filename in
  begin
    print_endline s;
    output_string outlol s
  end*)

(*let list_to_file list file =
  let rec aux list file = match list with
    | [] -> ()
    | h::r ->
      begin
        string_to_file h file;
            aux r file
      end
  in aux list file
*)
