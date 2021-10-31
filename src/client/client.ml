type client_state = {
  uid : int;
  logged_in : bool;
}

let init_state () = { uid = -1; logged_in = false }

let welcome_messages msg =
  if msg == 0 then
    print_endline
      "Welcome to Camel Chat!\n\
       Read the options to decide what to do next:\n\
      \ If you already have an account, type Log In to login. \n\
      \ To create a new account, type Sign Up\n"
  else if msg == 1 then
    print_endline
      "Welcome back! Let's log you into Camel Chat. Please enter your \
       username"
  else if msg == 2 then
    print_endline
      "Welcome new user! Let's sign you up for Camel Chat. Please \
       enter a username. You must have a unique username of at least \
       four characters and no special symbols."
  else if msg == 3 then
    print_endline
      "Welcome to Camel Chat. You can now send messages to your \
       friends. ADD MORE DETAILS HERE!"
  else print_endline ""
