open Async
open ANSITerminal

(* [st] is the initial state of the client. *)
let st = "ref (init_state ())"

let caml = ""

let help_message = ""

let chat = raise (Failure "Unimplemented")

let tcp host port =
  let addr =
    Async.Tcp.Where_to_connect.of_host_and_port { host; port }
  in
  ignore (Async.Tcp.with_connection addr chat);
  Deferred.never ()

(* Looking for a Socket.Address.t *)
(* A function that will take in the created Writer.t and Reader.t that
   will finally read Terminal inputs *)

let reg = [ Foreground White ]

(** Command.async Command.Param *)
let main () =
  print_string reg "Welcome to Caml Chat!\n";
  print_string reg "Please enter your username,\n";
  print_string [ Foreground Blue ] ">> "

let () = main ()