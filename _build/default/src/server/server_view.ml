open Async
open Async_unix
open ANSITerminal

let server = ref (Server.init_server ())

let check_state str =
  let x = !server.uname_and_pwds in
  let rec check_username lst str =
    match lst with
    | [] -> true
    | (u, p, w) :: h -> if u = str then false else check_username h str
  in
  check_username x str

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
  print_endline "Client has been added";
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: reading from server\n";
      return ()
  | `Ok line -> (
      print_endline ("Received: " ^ line);
      let sys = int_of_string (String.sub line 0 5) in
      let mess = String.sub line 5 (String.length line - 5) in
      match sys with
      (* Code 00001 is for checking a username*)
      | 00001 ->
          if check_state mess = true then (
            Writer.write_line w "false";
            connection_reader addr r w)
          else (
            Writer.write_line w "true";
            connection_reader addr r w)
      (* Code 00010 is for sign in*)
      | 00010 ->
          if check_state mess = true then (
            Writer.write_line w "true";
            server :=
              {
                !server with
                uname_and_pwds = Server.new_user_pwd mess "" w !server;
              };
            connection_reader addr r w)
          else (
            Writer.write_line w "false";
            connection_reader addr r w)
      | 00011 ->
          ignore (send_all_message w mess);
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