open Async
open Server

(* Initializes a new server client connection. Copy data from the reader
   to the writer. *)
let rec init_connection buffer r w =
  Reader.read r buffer >>= function
  | `Eof ->
      return
        (printf "Error connecting to the server\n";
         return ())
  | `Ok bytes_read ->
      printf "Connecting to the server success\n";
      Writer.write w (Bytes.to_string buffer) ~len:bytes_read;
      Writer.flushed w >>= fun () -> init_connection buffer r w

(** Starts a TCP server, which listens on the specified port, invoking
    init_connection every time a client connects. *)
let run () =
  let host_and_port =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8765) (fun _addr r w ->
        let buffer = Bytes.create (16 * 1024) in
        init_connection buffer r w)
  in
  ignore
    (host_and_port
      : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

(* Call [run], and then start the scheduler *)
let () =
  run ();
  print_string "Starting the chat server ... \n";
  print_string "> ";
  never_returns (Scheduler.go ())
