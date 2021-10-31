(* [client_state] is the current state of the cleint *)
type client_state = { uid : int }

(* [init_client] returns the initial [client_state] for a client. *)
val init_state : unit -> client_state
