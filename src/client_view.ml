open Async
open ANSITerminal

let caml = ""

let help_message = ""

let stuff = Tcp.connect_sock
let tcp = let addr = Async.Tcp.with_connection (* Looking for a Socket.Address.t *) (* A function that will take in the created Writer.t and Reader.t that will finally read Terminal inputs *)

let reg = [ Foreground White ]

let main () =
  print_string reg "Welcome to Caml Chat!\n";
  print_string reg "Please enter your username,\n";
  print_string [ Foreground Blue ] ">> ";
  Command.async Command.Spec


let () = main ()