open Core
open Async

type server_state = { uid : int }

type output = {
  uid : int;
  output : string;
}

type input = {
  uid : int;
  input : string;
}

type output_string = string

type current_state = {
  state : server_state;
  output : output option;
  output_string : output_string;
}

let init_server = raise (Failure "Unimplemented: server.init_server")

let init_state = raise (Failure "Unimplemented: server.init_state")

let input_of_string =
  raise (Failure "Unimplemented: server.input_of_string")

let string_of_output =
  raise (Failure "Unimplemented: server.string_of_output")
