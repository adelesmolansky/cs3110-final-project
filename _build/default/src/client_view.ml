open Async
open Client

(* [st] is the initial state of the client. *)
let st = ref (init_state ())

let caml = ""

let help_message = ""