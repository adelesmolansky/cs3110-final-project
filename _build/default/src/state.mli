type state = {
  connections : (int * (Async.Writer.t * Async.Reader.t)) list;
  users : (int * (string * string)) list;
}
