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

let rec connection_reader addr r w =
  print_endline "Client has been added";
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: reading from server\n";
      return ()
  | `Ok line -> (
      print_endline ("Received" ^ line);
      let sys = int_of_string (String.sub line 0 4)
      and mess = String.sub line 5 (String.length line) in
      match sys with
      (* Code 00001 is for checking a username*)
      | 00001 ->
          if check_state mess = false then (
            Writer.write w "false";
            connection_reader addr r w)
          else (
            Writer.write w "true";
            connection_reader addr r w)
      (* Code 00010 is for sign in*)
      | 00010 ->
          if check_state mess = false then (
            Writer.write w "true";
            server :=
              {
                !server with
                uname_and_pwds = Server.new_user_pwd mess "" w !server;
              };
            connection_reader addr r w)
          else (
            Writer.write w "false";
            connection_reader addr r w)
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