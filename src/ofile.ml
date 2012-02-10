(* ocamlc -I +extlib extLib.cma unix.cma file.ml *)


open Unix
open ExtLib

let string_of_file file =
  Std.input_file ~bin:false file

let list_of_file file =
  Std.input_list (open_in file)

let create_file filename =
  close_out (open_out filename)

let rec list_directory_aux dir acc =
  try
    let file = Unix.readdir dir in
      list_directory_aux dir (file::acc)
  with End_of_file -> acc

let list_of_directory dirname =
  let handle = Unix.opendir dirname
  in list_directory_aux handle []
