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

let init_server = { uid = -1 }

let init_state =
  {
    state = init_server;
    output = Some { uid = -1; output = "N/A" };
    output_string = "N/A";
  }

let input_of_string str = { uid = -1; input = str }

let string_of_output = function
  | { uid = _; output = o } -> o
