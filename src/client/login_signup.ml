open Async
open ANSITerminal
open Client

let rec read_usern r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      read_usern r w
  | `Ok line -> check_username r w line

and check_username r w str =
  let t1 =
    match String.index_opt str ' ' with
    | Some _ -> true
    | None -> false
  in
  if t1 then (
    print_endline "Error: username must contain no blank spaces!";
    read_usern r w)
  else if String.length str < 4 then (
    print_endline
      "Error: username must contain at least four characters!";
    read_usern r w)
  else (
    print_endline "Nice username!";
    Writer.write_line w str;
    return ())

and check_server r w = return ()

and create_user res r w = return ()

let rec main r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      main r w
  | `Ok line -> read_input r w line

and read_input r w str =
  if str = "Log In" then welcome_messages 1
  else if str = "Sign Up" then welcome_messages 2
  else print_endline "Please enter a valid command";
  main r w
