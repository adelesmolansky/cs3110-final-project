type single_client_state = {
  uname : string;
  pwd : string;
  is_in_chat : bool;
}

let init_state () = { uname = ""; pwd = ""; is_in_chat = false }
