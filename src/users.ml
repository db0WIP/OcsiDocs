
type user = (string * string)

(* *****                 List of Users				       ***** *)

let users = ref []

let check_pwd name pwd =
  try List.assoc name !users = pwd
  with Not_found -> false

let add_user_in_db name pwd =
  users := (name, pwd)::!users

let add_user name pwd =
  begin
(*    Ofile.create_directory "docs/"^name;
    Ofile.string_to_file pwd*)
  end

let dump_users () =
  List.iter
    (fun item ->
      begin
	print_string "name : ";
	print_endline (fst item);
	print_string "pass : ";
	print_endline (snd item)
      end
    )
    !users

