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
  | ADD_SUCCESS
  | VIEW_ALL
  | DELETE
  | DELETE_SUCCESS

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
  | ADD_SUCCESS -> print_endline "You successfully added a new acronym!"
  | VIEW_ALL ->
      print_endline "Here are a list of all your saved acronyms"
  | DELETE ->
      print_endline
        "Please enter the acronym that you would like to delete"
  | DELETE_SUCCESS ->
      print_endline "You successfully deleted an acronym!"

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ (acr, ph) ] -> acc ^ pp_elt acr ^ " = " ^ pp_elt ph
      | (acr1, ph1) :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else
            loop (n + 1)
              (acc ^ pp_elt acr1 ^ " = " ^ pp_elt ph1 ^ "; ")
              t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

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
      if line = "true" then get_cur_acrs r w uname
      else (
        print_endline
          "Sorry, that is not the correct password. Please enter your \
           username and password again";
        read_usern r w EXISTING_USER)

(* TODO: CONVERT STRING TO LIST AND ADD TO THE CLIENTS ACRONYMS *)
and get_cur_acrs r w uname =
  Writer.write_line w ("00111" ^ uname);
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: Server connection";
      return ()
  | `Ok line ->
      print_endline "Log In successful";
      (* FOR NOW JUST PRINT THE USERS ACRONYMS BUT IDEALLY WE ADD IT TO
         THE CLIENTS LOCAL LIST OF ACRONYMS *)
      print_endline line;
      return ()

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
        client :=
          {
            username = uname;
            pwd = pass;
            is_in_chat = true;
            acronyms = !client.acronyms;
          };
        print_endline "Sign Up successful";
        return ())
      else (
        print_endline
          "Sorry, that password is not valid. Please enter a valid \
           password and try again";
        read_password r w uname NEW_USER)

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

(* [add_acronym acr phrase] adds the new acronym phrase pair to the
   clients list *)
let rec add_acronym acr phrase =
  client :=
    { !client with acronyms = (acr, phrase) :: !client.acronyms }

let rec is_new_acr acronym lst =
  match lst with
  | [] -> true
  | (a, _) :: t -> if a = acronym then false else is_new_acr acronym t

let rec check_valid_acr acronym =
  match String.index_opt acronym ' ' with
  | Some _ -> false
  | None -> true

(* [read_new_acronym r w] gets calls when a user has entered a new
   acronym that they want to add to their collection *)
let rec read_new_acronym r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      read_new_acronym r w
  | `Ok line -> check_new_acronym r w line

and check_new_acronym r w acronym =
  if check_valid_acr acronym then (
    match is_new_acr acronym !client.acronyms with
    | true ->
        print_endline
          "Please enter the corresponding phrase for your new acronym";
        read_phrase r w !client.username acronym
    | false ->
        print_endline
          "You have already created that acronym. Please enter a new \
           acronym that you would like to create";
        read_new_acronym r w)
  else (
    print_endline "Error: acronym must contain no blank spaces!";
    read_new_acronym r w)

and read_phrase r w uname acronym =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error: cannot read input";
      read_phrase r w uname acronym
  | `Ok line ->
      add_acronym acronym line;
      add_pair_to_server r w uname acronym line

and add_pair_to_server r w uname acronym phrase =
  Writer.write_line w
    ("00110" ^ !client.username ^ ":" ^ acronym ^ ":" ^ phrase);
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: Server connection";
      return ()
  | `Ok line ->
      print_endline
        "Acronym and phrase added to server for temporary storage.";
      return ()

(* [removed_acronym acronym acr_lst acc] returns a new association list
   with the users acronyms that has removed the acronym pair that the
   user wants to delete *)
let rec removed_acronym acronym acr_lst acc =
  match acr_lst with
  | [] -> acc
  | (a, p) :: t ->
      if acronym = a then acc @ t
      else removed_acronym acronym t ((a, p) :: acc)

(* [get_phrase acronym acr_lst] returns the associated phrase to acroynm
   in the association list with the users acronyms *)
let rec get_phrase acronym acr_lst =
  match acr_lst with
  | [] -> failwith "Acronym does not exist"
  | (a, p) :: t -> if acronym = a then p else get_phrase acronym t

(* [read_new_acronym r w] gets calls when a user has entered a new
   acronym that they want to add to their collection *)
let rec read_delete_acronym r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      print_endline "Error reading stdin\n";
      read_delete_acronym r w
  | `Ok line -> check_old_acronym r w line

and check_old_acronym r w acronym =
  match is_new_acr acronym !client.acronyms with
  | false ->
      let phrase = get_phrase acronym !client.acronyms in
      let str =
        "You are deleting the acronym - phrase pair" ^ acronym ^ " = "
        ^ phrase
      in
      print_endline str;
      client :=
        {
          !client with
          acronyms = removed_acronym acronym !client.acronyms [];
        };
      print_endline
        "The acronym and corresponding phrase have been deleted has \
         been deleted";
      read_phrase r w !client.username acronym
  | true ->
      print_endline
        "This acroynm does not exist. Please enter an acronym that you \
         have created in the past.";
      read_delete_acronym r w
(* TODO: ADD A WAY TO TRANFER TO ADDING A NEW ACROYNM INSTEAD *)

let acronym_delete_handler r w =
  acronym_messages DELETE;
  read_delete_acronym r w >>= fun () ->
  acronym_messages DELETE_SUCCESS;
  return ()

let acronym_view_handler r w =
  acronym_messages VIEW_ALL;
  print_endline (pp_list pp_string !client.acronyms)

(* [acronym_add_handler r w] instructs the user on what to do next if a
   user just entered the #add command *)
(* WE NEED A WAY TO CIRCLE BACK TO READING *)
let rec acronym_add_handler r w =
  acronym_messages ADD_ACRONYM;
  read_new_acronym r w >>= fun () ->
  acronym_messages ADD_SUCCESS;
  return ()

(* [read r] keeps reading the pipe [r] from the server until the server
   is successfully connected *)
let rec read r w =
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Server error, please try again. \n";
      exit 0
  | `Ok line -> (
      match line with
      | "CREATE_NEW_ACRONYM" -> acronym_add_handler r w
      | "SEND_ALL" ->
          acronym_view_handler r w;
          read r w
      | "DELETE_ACRONYM" -> acronym_delete_handler r w
      | _ ->
          print_endline line;
          read r w)

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
  don't_wait_for (read r w);
  don't_wait_for (send_msg w);
  ()

(* [login_signup r w] initializes the login and sign up process for a
   client. A client will login or signup and then enter the general chat
   room *)
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