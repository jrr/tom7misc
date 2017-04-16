signature RENDER =
sig

  type pixels = Word32.word Array.array
  (* Repeating color values for drawing marquee lines, etc. *)
  type segment = Word32.word Vector.vector

  val drawareas : pixels * Screen.areas * segment -> unit
  val drawareacolors : pixels * Screen.areas -> unit

  (* drawobjects (buf, screen, frozen)

     If frozen is SOME key, then highlight objects in that configuration. *)
  val drawobjects : pixels * Screen.screen * Screen.Obj.key option -> unit

  (* drawmap (pixels, worldx, worldy) *)
  val drawmap : pixels * int * int -> unit

end
