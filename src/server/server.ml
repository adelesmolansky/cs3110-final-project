open Core
open Async

type server_state = {
  uname_and_pwds : (string * int) list;
  curr_users : int list;
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

let init_server () = { uname_and_pwds = []; curr_users = [] }

let init_state () =
  {
    state = init_server ();
    output = Some { uid = -1; output = "N/A" };
    output_string = "N/A";
  }

let input_of_string str = { uid = -1; input = str }

let string_of_output = function
  | { uid = _; output = o } -> o

let insert_al k v lst = (k, v) :: lst

let insert_l v lst = v :: lst

let new_user_pwd uname pwd st = insert_al uname pwd st.uname_and_pwds

let new_user_in_room uname st = insert_l uname st.curr_users