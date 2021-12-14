(* [client_state] is the current state of the cleint *)
type client_state = {
  username : string;
  pwd : string;
  is_in_chat : bool;
  acronyms : (string * string) list;
}

(* [init_client] returns the initial [client_state] for a client. *)
val init_state : unit -> client_state
