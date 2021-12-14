type client_state = {
  username : string;
  pwd : string;
  is_in_chat : bool;
  acronyms : (string * string) list;
}

let init_state () =
  {
    username = "";
    pwd = "";
    is_in_chat = false;
    acronyms = [ ("btw", "by the way") ];
  }
