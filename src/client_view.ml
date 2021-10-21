open Async
open ANSITerminal

(* [st] is the initial state of the client. *)
let st = "ref (init_state ())"

let caml = ""

let help_message = ""

let rec read r =
  Reader.read_line r >>= function
  | `Eof ->
      printf [ Foreground Red ] "Server error, please try again. \n";
      exit 0
  | `Ok line -> return ()

let rec send_msg w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof ->
      printf [ Foreground Red ] "Error reading stdin\n";
      return ()
  | `Ok line -> return ()

let rec read_usern r w =
  let input = Lazy.force Reader.stdin in
  Reader.read_line input >>= function
  | `Eof ->
      printf [] "Error reading stdin\n";
      read_usern r w
  | `Ok line -> check_username r w line

and check_username r w str =
  let t1 =
    match String.index_opt str ' ' with
    | Some _ -> true
    | None -> false
  in
  if t1 then (
    print_string [ Foreground Red ]
      "Error: username must contain no blank spaces!";
    read_usern r w)
  else if String.length str = 0 then (
    print_string [ Foreground Red ]
      "Error: username must contain characters!";
    read_usern r w)
  else (
    Writer.write_line w "Nice username!";
    check_server r w)

and check_server r w =
  Reader.read_line r >>= function
  | `Eof ->
      printf [] "Error reading server\n";
      read_usern r w
  | `Ok line -> create_user line r w

and create_user res r w = return ()

let read_write_loop r w =
  don't_wait_for (send_msg w);
  don't_wait_for (read r);
  ()

let chat _ r w =
  read_usern r w >>= fun () ->
  print_string [ Foreground Blue ] "Welcome!";
  read_write_loop r w;
  Deferred.never ()

let tcp host port =
  let addr =
    Async.Tcp.Where_to_connect.of_host_and_port { host; port }
  in
  Async.Tcp.with_connection addr chat

let reg = [ Foreground White ]

let main () =
  print_string reg "Welcome to Caml Chat!\n";
  print_string reg "Please enter your username,\n";
  print_string [ Foreground Blue ] ">> ";
  Command.async ~summary:""
    (Command.Param.return (fun () -> tcp "0.0.0.0" 9999))
  |> Command.run

let () = main ()
