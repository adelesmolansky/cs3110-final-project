open Async
open Async_unix
open ANSITerminal

type acronyms = (string * string) list

let server = ref (Server.init_server ())

type msg_to_writer =
  | LOGIN_UNAME_EXISTS
  | LOGIN_NEW_UNAME
  | SIGNUP_NEW_UNAME
  | SIGNUP_UNAME_EXISTS
  | LOGIN_PWD_MATCH
  | LOGIN_PWD_NO_MATCH
  | SIGNUP_ADD_USER
  | ACRONYM_ADD_START
  | ACRONYM_ADD
  | ACRONYM_EXISTS
  | ACRONYM_NEW_PHRASE
  | ACRONYM_VIEW
  | ACRONYM_DELETE_START
  | ACRONYM_DELETE_PAIR
  | ACRONYM_DELETE_NO_MATCH

let send_to_writer = function
  | LOGIN_UNAME_EXISTS -> "USER_EXISTS"
  | LOGIN_NEW_UNAME -> "NOT_A_USER"
  | SIGNUP_NEW_UNAME -> "CREATE_NEW_USER"
  | SIGNUP_UNAME_EXISTS -> "NEED_UNIQUE_UNAME"
  | LOGIN_PWD_MATCH -> "true"
  | LOGIN_PWD_NO_MATCH -> "false"
  | SIGNUP_ADD_USER -> "true"
  | ACRONYM_ADD_START -> "CREATE_NEW_ACRONYM"
  | ACRONYM_ADD -> "ASK_FOR_PHRASE"
  | ACRONYM_EXISTS -> "ACRONYM_EXISTS"
  | ACRONYM_NEW_PHRASE -> "ADD_PAIR"
  | ACRONYM_VIEW -> "SEND_ALL"
  | ACRONYM_DELETE_START -> "DELETE_ACRONYM"
  | ACRONYM_DELETE_PAIR -> "DELETED_PAIR"
  | ACRONYM_DELETE_NO_MATCH -> "DELETE_FAIL"

(* [look_up wr] returns the option of the username of the client
   associated with the Writer.t wr*)
let look_up wr =
  let x = !server.uname_and_pwds in
  let rec loop write lst =
    match x with
    | [] -> None
    | (u, p, w) :: t -> if w == wr then Some u else loop wr t
  in
  loop wr x

(* [new_user str] returns true if the username [str] does not exist in
   the current list of users and passwords, and false otherwise *)
let check_username str =
  let x = !server.uname_and_pwds in
  let rec check lst str =
    match lst with
    | [] -> true
    | (u, p, w) :: h -> if u = str then false else check h str
  in
  check x str

(* [check_password uname pass] returns true if the given
   username-password pair at in the server state, and false otherwise.*)
let check_password uname pass =
  let x = !server.uname_and_pwds in
  let rec check lst un pa =
    match lst with
    | [] -> false
    | (u, p, w) :: h -> if u = un && p = pa then true else check h un pa
  in
  check x uname pass

let rec find_acr_list uname map =
  match map with
  | [] -> failwith "User does not exist"
  | (x, lst) :: xs -> if x = uname then lst else find_acr_list uname xs

let rec check_new_acroynm_h (acr : string) (acr_list : acronyms) =
  match acr_list with
  | [] -> false
  | (acr', _) :: t ->
      if acr = acr' then true else check_new_acroynm_h acr t

(* [check_new_acroynm uname str] returns true if the given acronym is in
   the server state for the given user and false otherwise.*)
let check_new_acroynm uname str =
  let x = !server.acronyms in
  check_new_acroynm_h str (find_acr_list uname x)

(* [change_pass uname pass wr] changes the password of the client
   associated with Writer.t wr.*)
let change_pass uname pass wr =
  let x = !server.uname_and_pwds
  and check tri =
    match tri with
    | u1, _, w -> wr != w
  in
  (uname, pass, wr) :: List.filter check x

(* [send_all_message writer str] loops through all the clients in the
   server state, throwing out the client with the Writer.t writer.
   During this loop, this function writes to each client the string
   str.*)
let send_all_message writer str =
  let x = !server.uname_and_pwds in
  let rec send wr lst str =
    match lst with
    | [] -> return ()
    | (u, p, w) :: t ->
        print_endline p;
        if writer == w then send wr t str
        else (
          Writer.write_line w str;
          send wr t str)
  in
  send writer x str

(* [connection_reader addr r w] is a resursive function that completes
   async actions. This function is constantly waiting for a client to
   write to its Reader.t. Once something is written, connection_reader
   reads the string, parses it for a 5 bit code, does that code's
   asccociated action, and then calls itself with the same arguments.*)
let rec connection_reader addr r w =
  print_endline "Ready to read";
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: reading from server\n";
      return ()
  | `Ok line -> (
      print_endline ("Received: " ^ line);
      let sys = int_of_string (String.sub line 0 5) in
      let user_input = String.sub line 5 (String.length line - 5) in
      match sys with
      (* Code 00001 is for log in *)
      | 00001 ->
          if check_username user_input = true then (
            Writer.write_line w (send_to_writer LOGIN_NEW_UNAME);
            connection_reader addr r w)
          else (
            Writer.write_line w (send_to_writer LOGIN_UNAME_EXISTS);
            connection_reader addr r w)
      (* Code 00010 is for sign up *)
      | 00010 ->
          if check_username user_input = true then (
            Writer.write_line w (send_to_writer SIGNUP_NEW_UNAME);
            server :=
              {
                !server with
                uname_and_pwds =
                  Server.new_user_pwd user_input "" w !server;
              };
            connection_reader addr r w)
          else (
            Writer.write_line w (send_to_writer SIGNUP_UNAME_EXISTS);
            connection_reader addr r w)
      (* Code 00011 sends general messages*)
      | 00011 -> read_user_input addr user_input r w
      (* Code 00100 checks to see if a username - password pair are
         correct*)
      | 00100 ->
          let lst = String.split_on_char ':' user_input in
          let uname = List.nth lst 0 and pass = List.nth lst 1 in
          if check_password uname pass = true then (
            Writer.write_line w (send_to_writer LOGIN_PWD_MATCH);
            connection_reader addr r w)
          else (
            Writer.write_line w (send_to_writer LOGIN_PWD_NO_MATCH);
            connection_reader addr r w)
      (* Code 00101 adds a password to the server state *)
      | 00101 ->
          let lst = String.split_on_char ':' user_input in
          let uname = List.nth lst 0 and pass = List.nth lst 1 in
          server :=
            { !server with uname_and_pwds = change_pass uname pass w };
          Writer.write_line w (send_to_writer SIGNUP_ADD_USER);
          connection_reader addr r w
      (* Code 00110 is to add a new acronym *)
      | 00110 ->
          let pair = String.split_on_char ':' user_input in
          let uname = List.nth pair 0 and acr = List.nth pair 1 in
          if check_new_acroynm uname acr = true then (
            Writer.write_line w (send_to_writer ACRONYM_ADD);
            connection_reader addr r w)
          else (
            Writer.write_line w (send_to_writer LOGIN_UNAME_EXISTS);
            connection_reader addr r w)
      | _ -> connection_reader addr r w)

and read_user_input addr user_input r w =
  match user_input with
  | "#add" ->
      Writer.write_line w (send_to_writer ACRONYM_ADD_START);
      connection_reader addr r w
  | "#view" ->
      Writer.write_line w (send_to_writer ACRONYM_VIEW);
      connection_reader addr r w
  | "#delete" ->
      Writer.write_line w (send_to_writer ACRONYM_DELETE_START);
      connection_reader addr r w
  (* Handle normal input *)
  | _ ->
      let u =
        match look_up w with
        | None -> ""
        | Some v -> v
      in
      ignore (send_all_message w (u ^ ": " ^ user_input));
      connection_reader addr r w

let create_tcp port =
  let host_and_port =
    Async.Tcp.Server.create ~on_handler_error:`Raise
      (Async.Tcp.Where_to_listen.of_port port) (fun addr r w ->
        connection_reader addr r w)
  in
  ignore
    (host_and_port
      : (Socket.Address.Inet.t, int) Async.Tcp.Server.t Deferred.t);
  Deferred.never ()

let run port =
  ignore (create_tcp port);
  Deferred.never ()

let reg = [ Foreground White ]

let main () =
  print_endline "Starting the chat server ... ";
  print_endline "Server is running";
  Command.async ~summary:"" (Command.Param.return (fun () -> run 9999))
  |> Command.run

let () = main ()