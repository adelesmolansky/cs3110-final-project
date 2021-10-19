open Async
open Server
open ANSITerminal

(* [st] is the initial state of the client. *)
let st = ref (init_state ())

let caml = ""

let help_message = ""

let main () = print_string [ Background Blue ] "Please help me!"

let () = main ()