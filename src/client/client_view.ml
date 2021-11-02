open Async
open ANSITerminal
open Client

type message =
  | INIT
  | LOG_IN
  | SIGN_UP
  | ENTER_CHAT

type method_to_enter_chat = NEW_USER | EXISTING_USER

(* [client_st] is the initial state of the client. *)
let client_st = ref (init_state ())

let welcome_messages = function
  | INIT ->
      print_endline
        "Welcome to Camel Chat!\n\
         Read the options to decide what to do next:\n\
        \ If you already have an account, type Log In to login. \n\
        \ To create a new account, type Sign Up\n"
  | LOG_IN ->
      print_endline
        "Welcome back! Let's log you into Camel Chat. Please enter \
         your username"
  | SIGN_UP ->
      print_endline
        "Welcome new user! Let's sign you up for Camel Chat. Please \
         enter a username. You must have a unique username of at least \
         four characters and no special symbols."
  | ENTER_CHAT ->
      print_endline
        "Welcome to Camel Chat. You can now send messages to your \
         friends. ADD MORE DETAILS HERE!"

(* [read r] keeps reading the pipe [r] from the server until the server
   is successfully connected *)
let rec read r =
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Server error, please try again. \n";
      exit 0
  | `Ok line -> return ()

(* [send_msg w] converts the standard input to server input by trimming
   white space and then recursively calls send_msg to send the inputs to
   the server. *)
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

let login_process r w uname = ""

let signup_process r w uname = ""

(* [read_usern r w next_step] checks if the user has properly entered a
   username and recursively calls read_user until the username rules are
   met. *)
let rec read_usern r w next_step =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      read_usern r w next_step
  | `Ok line -> check_username r w line next_step

(* check for valid username *)
and check_username r w str next_step =
  let t1 =
    match String.index_opt str ' ' with
    | Some _ -> true
    | None -> false
  in
  if t1 then (
    print_endline "Error: username must contain no blank spaces!";
    read_usern r w next_step)
  else if String.length str < 4 then (
    print_endline
      "Error: username must contain at least four characters!";
    read_usern r w next_step)
  else (
    print_endline "Nice username!";
    match next_step with
    | EXISTING_USER -> login_process r w str
    | NEW_USER -> signup_process r w str

and check_server r w = return ()

and create_user res r w = return ()

(* [read_login_or_signup r w] checks if the user wants to log in or sign
   up and recursively calls read_login_or_signup until the user has made
   a proper decision. *)
let rec read_login_or_signup r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      read_login_or_signup r w
  | `Ok line -> read_input r w line

and read_input r w str =
  if str = "Log In" then (
    welcome_messages LOG_IN;
    read_usern r w EXISTING_USER)
  else if str = "Sign Up" then (
    welcome_messages SIGN_UP;
    read_usern r w NEW_USER)
  else (
    print_endline "Please enter a valid command";
    read_login_or_signup r w)

let login_signup _ r w =
  read_login_or_signup r w >>= fun () ->
  welcome_messages ENTER_CHAT;
  read_write_loop r w;
  Deferred.never ()

let tcp host port =
  let addr =
    Async.Tcp.Where_to_connect.of_host_and_port { host; port }
  in
  Async.Tcp.with_connection addr login_signup

let reg = [ Foreground White ]

let main () =
  welcome_messages INIT;
  Command.async ~summary:""
    (Command.Param.return (fun () -> tcp "0.0.0.0" 9999))
  |> Command.run

let () = main ()
