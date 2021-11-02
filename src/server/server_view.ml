open Async
open Async_unix
open ANSITerminal
<<<<<<< HEAD
=======
open Server
>>>>>>> f662158268a8f4f5ec774108589f56f067d72621

let rec connection_handler addr r w =
  let () = print_endline "Client \n" in
  let rec repeat r w =
    Reader.read_line r >>= function
    | `Eof ->
        print_endline "Error: reading from server\n";
        return ()
    | `Ok line ->
        print_endline ("received: " ^ line ^ "\n");

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
  print_endline "Starting the chat server ... \n";
  print_endline "> ";
  Command.async ~summary:"" (Command.Param.return (fun () -> run 9999))
  |> Command.run

let () = main ()