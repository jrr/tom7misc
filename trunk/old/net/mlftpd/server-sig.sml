

signature SERVER =
sig

  (* run the server forever (some forked copies will return) *)
  val run : unit -> unit

end
