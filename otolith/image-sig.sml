signature IMAGE =
sig

  exception Image of string

  type image = int * int * Word32.word Array.array
  (* XXX should have different animation modes,
     wait on frame, etc. Should maybe be described by
     some metadata file? *)
  type anim = image Vector.vector

  val loadimage : string -> image
  (* loadanim (base, ext, lo, hi) *)
  val loadanim : string * string * int * int -> anim

  val numframes : anim -> int
  val getframe : anim * int -> image

  val fliph : image -> image

end