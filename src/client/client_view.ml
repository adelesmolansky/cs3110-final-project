open Async
open ANSITerminal
open Client

type message =
  | INIT
  | LOG_IN
  | SIGN_UP
  | ENTER_CHAT

type method_to_enter_chat =
  | NEW_USER
  | EXISTING_USER

type acronym_commands =
  | ADD_ACRONYM
  | ADD_PHRASE
  | VIEW_ALL
  | DELETE

(* [client] is the initial state of the client. *)
let client = ref (Client.init_state ())

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

let acronym_messages = function
  | ADD_ACRONYM ->
      print_endline "Please type the acronym that you want to create"
  | ADD_PHRASE ->
      print_endline
        "Please type the corresponding phrase for the acronym that you \
         just entered"
  | VIEW_ALL ->
      print_endline "Here are a list of all your saved acronyms"
  | DELETE ->
      print_endline
        "Please enter the acronym that you would like to delete"

(* [read r] keeps reading the pipe [r] from the server until the server
   is successfully connected *)
let rec read r =
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Server error, please try again. \n";
      exit 0
  | `Ok line ->
      print_endline line;
      read r

(* [send_msg w] converts the standard input to server input by trimming
   white space and then recursively calls send_msg to send the inputs to
   the server. *)
let rec send_msg w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      return ()
  | `Ok line ->
      Writer.write_line w ("00011" ^ line);
      send_msg w

let read_write_loop r w =
  don't_wait_for (read r);
  don't_wait_for (send_msg w);
  ()

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
  else
    match next_step with
    | EXISTING_USER -> login_process r w str
    | NEW_USER -> signup_process r w str

and read_password r w uname next_step =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error: cannot read input";
      read_usern r w next_step
  | `Ok line -> check_password r w uname line next_step

and check_password r w uname pass next_step =
  let t1 =
    match String.index_opt pass ' ' with
    | Some _ -> true
    | None -> false
  in
  if t1 then (
    print_endline "Error: password must contain no blank spaces!";
    read_usern r w next_step)
  else if String.length pass < 4 then (
    print_endline
      "Error: password must contain at least four characters!";
    read_usern r w next_step)
  else
    match next_step with
    | EXISTING_USER -> password_process r w uname pass
    | NEW_USER -> new_password_process r w uname pass

and login_process r w uname =
  Writer.write_line w ("00001" ^ uname);
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: Server connection";
      return ()
  | `Ok line ->
      if line = "USER_EXISTS" then (
        print_endline "Please enter your password";
        read_password r w uname EXISTING_USER)
      else (
        (* NOT_A_USER *)
        print_endline "Sorry, that is not a user in our database";
        read_usern r w EXISTING_USER)

and password_process r w uname pass =
  Writer.write_line w ("00100" ^ uname ^ ":" ^ pass);
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: Server connection";
      return ()
  | `Ok line ->
      if line = "true" then (
        print_endline "Log In successful";
        return ())
      else (
        print_endline "Sorry, that is not the correct password";
        read_usern r w EXISTING_USER)

and signup_process r w uname =
  Writer.write_line w ("00010" ^ uname);
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: Server connection";
      return ()
  | `Ok line ->
      if line = "CREATE_NEW_USER" then (
        print_endline "Please enter a new password";
        read_password r w uname NEW_USER)
      else (
        (* NEED_UNIQUE_UNAME *)
        print_endline "Sorry, that is already a user in our database";
        read_usern r w NEW_USER)

and new_password_process r w uname pass =
  Writer.write_line w ("00101" ^ uname ^ ":" ^ pass);
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: Server connection";
      return ()
  | `Ok line ->
      if line = "true" then (
        client := { uname; pwd = pass; is_in_chat = true };
        print_endline "Sign Up successful";
        return ())
      else (
        print_endline
          "Sorry, that is not a valid password. Please try again";
        read_usern r w NEW_USER)

(* [read_login_or_signup r w] checks if the user wants to log in or sign
   up and recursively calls read_login_or_signup until the user has made
   a proper decision. *)
let rec read_login_or_signup r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      read_login_or_signup r w
  | `Ok line -> read_login_signup_input r w line

and read_login_signup_input r w str =
  match str with
  | "Log In" ->
      welcome_messages LOG_IN;
      read_usern r w EXISTING_USER
  | "Sign Up" ->
      welcome_messages SIGN_UP;
      read_usern r w NEW_USER
  | _ ->
      print_endline "Please enter a valid command";
      read_login_or_signup r w

(* [login_signup r w] initializes the login and sign up process for a
   client. A client will login or signup and then enter the general chat
   room *)
let login_signup _ r w =
  read_login_or_signup r w >>= fun () ->
  welcome_messages ENTER_CHAT;
  read_write_loop r w;
  Deferred.never ()

(* let rec is_acronym (str : string) (acronyms_list : (string * string)
   list) = match acronyms_list with | [] -> false | (a, _ ) :: t -> if a
   = str then true else is_acronym str t *)

(* [read_new_acronym r w] gets calls when a user has entered a new
   acronym that they want to add to their collection *)
let rec read_new_acronym r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      read_new_acronym r w
  | `Ok line -> check_new_acronym r w line

and check_new_acronym r w str =
  Writer.write_line w ("00110" ^ !client.uname ^ ":" ^ str);
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: Server connection";
      return ()
  | `Ok line ->
      if line = "NEW_ACRONYM" then
        let has_blanks =
          match String.index_opt str ' ' with
          | Some _ -> true
          | None -> false
        in
        if has_blanks then (
          print_endline "Error: acronym must contain no blank spaces!";
          read_new_acronym r w)
        else (
          print_endline
            "Please enter the corresponding phrase for your new acronym";
          acronym_process r w line)
      else (
        (* NOT_A_USER *)
        print_endline
          "You have already created that acronym. Please enter a new \
           acronym that you would like to create";
        read_new_acronym r w)

(* ADELE - TODO! *)
and acronym_process r w acronym = read_new_acronym r w

(* [acronym_add_handler r w] instructs the user on what to do next if a
   user just entered the #add command *)
let acronym_add_handler r w =
  acronym_messages ADD_ACRONYM;
  read_new_acronym r w

(* [acronym_view_handler r w] connects to the server to get the clients
   list of acronyms and then outputs it to the client *)
let rec acronym_view_handler r w =
  Writer.write_line w ("00001" ^ !client.uname);
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: Server connection";
      return ()
  | `Ok line ->
      print_endline "TODO: SEND ACRONYM LIST";
      return ()

let acronym_delete_handler r w =
  acronym_messages DELETE;
  return ()

(* [main_acronym_handler server_msg r w] reads a message from the server
   after a user entered a command to interact with the acronyms and
   matches the [server_msg] to the next step *)
let main_acronym_handler server_msg r w =
  match server_msg with
  | "CREATE_NEW_ACRONYM" -> acronym_add_handler r w
  | "SEND_ALL" ->
      acronym_messages VIEW_ALL;
      acronym_view_handler r w
  | "DELETE_ACRONYM" -> acronym_delete_handler r w
  | _ ->
      print_endline "Error";
      return ()

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