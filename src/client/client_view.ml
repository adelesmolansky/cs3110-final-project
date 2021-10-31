open Async
open ANSITerminal
open Client

(* [st] is the initial state of the client. *)
let st = "ref (init_state ())"

let welcome_messages msg =
  if msg == 0 then
    print_endline
      "Welcome to Camel Chat!\n\
       Read the options to decide what to do next:\n\
      \ If you already have an account, type Log In to login. \n\
      \ To create a new account, type Sign Up\n"
  else if msg == 1 then
    print_endline
      "Welcome back! Let's log you into Camel Chat. Please enter your \
       username"
  else if msg == 2 then
    print_endline
      "Welcome new user! Let's sign you up for Camel Chat. Please \
       enter a username. You must have a unique username of at least \
       four characters and no special symbols."
  else if msg == 3 then
    print_endline
      "Welcome to Camel Chat. You can now send messages to your \
       friends. ADD MORE DETAILS HERE!"
  else print_endline ""

let rec read r =
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Server error, please try again. \n";
      exit 0
  | `Ok line -> return ()

let rec send_msg w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      return ()
  | `Ok line -> return ()

let read_write_loop r w =
  don't_wait_for (send_msg w);
  don't_wait_for (read r);
  ()

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

let rec read_login_or_signup r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      read_login_or_signup r w
  | `Ok line -> read_input r w line

and read_input r w str =
  if str = "Log In" then (
    welcome_messages 1;
    read_usern r w)
  else if str = "Sign Up" then (
    welcome_messages 2;
    read_usern r w)
  else (
    print_endline "Please enter a valid command";
    read_login_or_signup r w)

let login_signup _ r w =
  read_login_or_signup r w >>= fun () ->
  welcome_messages 3;
  read_write_loop r w;
  Deferred.never ()

let tcp host port =
  let addr =
    Async.Tcp.Where_to_connect.of_host_and_port { host; port }
  in
  Async.Tcp.with_connection addr login_signup

let reg = [ Foreground White ]

let main () =
  welcome_messages 1;
  Command.async ~summary:""
    (Command.Param.return (fun () -> tcp "0.0.0.0" 9999))
  |> Command.run

let () = main ()
