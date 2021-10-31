(* [server_state] is the current state of the server *)
type server_state = { uid : int }

(* [output] is the output from the server to the user with [uid] *)
type output = {
  uid : int;
  output : string;
}

(* [input] is the message that the server receives from a user with
   [uid] *)
type input = {
  uid : int;
  input : string;
}

(* [output_string] is the string output that a user sees after typing a
   command *)
type output_string = string

(* [current_state] is the state of the server. *)
type current_state = {
  state : server_state;
  output : output option;
  output_string : output_string;
}

(* [init_server] initializes the server *)
val init_server : unit -> server_state

(* [init_state] is the initial state of the current_state. *)
val init_state : unit -> current_state

(* [input_of_string s] is the corresponding input given string [s] *)
val input_of_string : string -> input

(* [string_of_output out] is the corresponding string of [out] *)
val string_of_output : output -> string
