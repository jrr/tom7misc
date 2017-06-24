(* The entire map of the game, which consists of many
   screens. *)
signature WORLD =
sig

  exception World of string

  (* This struture is a singleton; there's just one global
     world at a time. *)

  (* Load from disk. *)
  val load : unit -> unit
  (* Save to disk. *)
  val save : unit -> unit

  (* Get the screen at (x, y), if it exists. *)
  val getmaybe : int * int -> Screen.screen option

  val getorcreate : int * int -> Screen.screen

  (* Sets the screen at x,y to the argument. *)
  val setscreen : int * int * Screen.screen -> unit

end