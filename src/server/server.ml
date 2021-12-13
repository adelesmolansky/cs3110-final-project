open Core
open Async

type username = string

type uname_and_pwds = (username * string * Writer.t) list

type acronyms = (string * string) list

type acronyms_map = (username * acronyms) list

type server_state = {
  uname_and_pwds : uname_and_pwds;
  curr_users : string list;
  acronyms : acronyms_map;
}

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

let init_server () =
  { uname_and_pwds = []; curr_users = []; acronyms = [] }

let init_state () =
  {
    state = init_server ();
    output = Some { uid = -1; output = "N/A" };
    output_string = "N/A";
  }

let input_of_string str = { uid = -1; input = str }

let string_of_output = function
  | { uid = _; output = o } -> o

let insert_al k v w lst = (k, v, w) :: lst

let insert_l v lst = v :: lst

let new_user_pwd uname pwd w st =
  insert_al uname pwd w st.uname_and_pwds

let new_user_in_room uname st = insert_l uname st.curr_users