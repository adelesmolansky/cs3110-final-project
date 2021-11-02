open Async
open Async_unix
open ANSITerminal

let rec connection_reader addr r w =
  let () = print_endline "Client has connected" in
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Error: reading from server\n";
      return ()
  | `Ok line ->
      print_endline ("received: " ^ line ^ "\n");
      Writer.write_line w line;
      connection_reader addr r w

let rec connection_writer addr r w str = Writer.write_line w str

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
  print_endline "Starting the chat server ... \n";
  print_endline "Server running";
  Command.async ~summary:"" (Command.Param.return (fun () -> run 9999))
  |> Command.run

let () = main ()
