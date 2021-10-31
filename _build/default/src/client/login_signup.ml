open Async
open ANSITerminal

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
  else if String.length str = 0 then (
    print_endline "Error: username must contain characters!";
    read_usern r w)
  else (
    Writer.write_line w "Nice username!";
    check_server r w)

and check_server r w =
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error reading server\n";
      read_usern r w
  | `Ok line -> create_user line r w

and create_user res r w = return ()