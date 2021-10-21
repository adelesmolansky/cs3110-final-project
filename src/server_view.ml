open Async
open Async_unix
open ANSITerminal

let rec connection_handler addr r w =
  let () = print_string [] "Client" in
  let rec repeat r w =
    Reader.read_line r >>= function
    | `Eof ->
        print_string [] "Error: cannot connect to server";
        return ()
    | `Ok line ->
        print_string [] "Received input";
        repeat r w
  in
  repeat r w

let create_tcp port =
  let host_and_port =
    Async.Tcp.Server.create ~on_handler_error:`Raise
      (Async.Tcp.Where_to_listen.of_port port) (fun addr r w ->
        connection_handler addr r w)
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
  print_string reg "Starting the chat server ... \n";
  print_string reg "> ";
  Command.async ~summary:"" (Command.Param.return (fun () -> run 9999))
  |> Command.run

let () = main ()
