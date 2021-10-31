open Async
open ANSITerminal
open Client

(* [st] is the initial state of the client. *)
let st = "ref (init_state ())"

let rec read r =
  Reader.read_line r >>= function
  | `Eof ->
      print_endline "Server error, please try again. \n";
      exit 0
  | `Ok line -> return ()

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

let chat _ r w =
  read_usern r w >>= fun () ->
  print_endline "Welcome!";
  read_write_loop r w;
  Deferred.never ()

let tcp host port =
  let addr =
    Async.Tcp.Where_to_connect.of_host_and_port { host; port }
  in
  Async.Tcp.with_connection addr chat

let reg = [ Foreground White ]

let main () =
  welcome_messages 1;
  print_endline "Please enter your username,\n";
  print_endline ">> ";
  Command.async ~summary:""
    (Command.Param.return (fun () -> tcp "0.0.0.0" 9999))
  |> Command.run

let () = main ()
