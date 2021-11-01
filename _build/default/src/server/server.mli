(* [uname_and_pwds] is the list of usernames and passwords *)
type uname_and_pwds = (string * string) list

(* [server_state] is the current state of the server *)
type server_state = {
  uname_and_pwds : uname_and_pwds;
  curr_users : string list;
}

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

(* [insert_al k v lst] is an association list that binds key [k] to
   value [v] and otherwise is the same as [lst] *)
val insert_al : string -> string -> uname_and_pwds -> uname_and_pwds

(* [insert_l v lst] is a list that adds value [v] to [lst] *)
val insert_l : string -> string list -> string list

(* [new_user_pwd] adds a username and password to the association list
   that stores all current usernames and passwords*)
val new_user_pwd : string -> string -> server_state -> uname_and_pwds

(* [new_user_in_room] adds a username to the list of current users*)
val new_user_in_room : string -> server_state -> string list
