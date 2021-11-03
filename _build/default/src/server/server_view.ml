open Async
open Async_unix
open ANSITerminal

let server = ref (Server.init_server ())

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

let check_password uname pass =
  let x = !server.uname_and_pwds in
  let rec check lst un pa =
    match lst with
    | [] -> false
    | (u, p, w) :: h -> if u = un && p = pa then true else check h un pa
  in
  check x uname pass

let change_pass uname pass w =
  let x = !server.uname_and_pwds
  and check tri =
    match tri with
    | u1, _, _ -> u1 != uname
  in
  (uname, pass, w) :: List.filter check x

let send_all_message writer str =
  let x = !server.uname_and_pwds in
  let rec send wr lst str =
    match lst with
    | [] -> return ()
    | (u, p, w) :: h ->
        if wr == w then send wr h str
        else (
          Writer.write_line w str;
          send wr h str)
  in
  send writer x str

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
          if check_username mess = true then (
            Writer.write_line w "false";
            connection_reader addr r w)
          else (
            Writer.write_line w "UNAME_EXISTS";
            connection_reader addr r w)
      (* Code 00010 is for sign up *)
      | 00010 ->
          if check_username mess = true then (
            Writer.write_line w "true";
            server :=
              {
                !server with
                uname_and_pwds =
                  Server.new_user_pwd user_input "" w !server;
              };
            connection_reader addr r w)
          else (
            Writer.write_line w "UNAME_EXISTS";
            connection_reader addr r w)
      (* Code 00011 sends messages*)
      | 00011 ->
          ignore (send_all_message w mess);
          connection_reader addr r w
      (* Code 00100 checks to see if a username - password pair are
         correct*)
      | 00100 ->
          let lst = String.split_on_char ':' mess in
          let uname = List.nth lst 0 and pass = List.nth lst 1 in
          if check_password uname pass = true then (
            Writer.write_line w "true";
            connection_reader addr r w)
          else (
            Writer.write_line w "false";
            connection_reader addr r w)
      | 00101 ->
          let lst = String.split_on_char ':' mess in
          let uname = List.nth lst 0 and pass = List.nth lst 1 in
          server :=
            { !server with uname_and_pwds = change_pass uname pass w };
          Writer.write_line w "true";
          connection_reader addr r w
      | _ -> connection_reader addr r w)

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