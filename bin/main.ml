(** [start_messaging uid] starts the messaging for the unique user with
    uid. *)
let start_messaging uid =
  raise (Failure "Unimplemented: Main.start_messaging")

(** [main ()] prompts for the messaging to start, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the OCaml messaging app. \n";
  print_endline "Please enter your name.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | uid -> start_messaging uid

(* Execute the game engine. *)
let () = main ()
