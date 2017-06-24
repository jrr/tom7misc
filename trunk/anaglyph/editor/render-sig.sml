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

  (* Debugging: Draw the points in "configuration space."
     These are the points where if the player is at (x, y), (x, y)
     also lands within some object. Expensive!

     Also note that this currently assumes a 1x1 player, so
     it's not even really configuration space. *)
  val drawmask : pixels * Screen.screen -> unit

end
