(* Imperative 2D bounding box *)
signature BOUNDS =
sig

  exception Empty

  type bounds
  (* Starts with no points. *)
  val nobounds : unit -> bounds
  val boundpoint : bounds -> real * real -> unit

  (* Raises Empty if no points have ever been added. *)
  val getbounds : bounds -> { minx : real, maxx : real,
                              miny : real, maxy : real }

  (* The offset of the point within the bounding box.
     Probably should only use this after you're done adding all the
     points to the bounds. *)
  val offsetx : bounds -> real -> real
  val offsety : bounds -> real -> real
  val width : bounds -> real
  val height : bounds -> real
  (* Modifies the left; leaves the right the same. Either may be empty. *)
  val union : bounds -> bounds -> unit

  (* Add a margin of fixed size around the entire bounds, in absolute units.
     Must be non-empty. *)
  val addmargin : bounds -> real -> unit
  (* Add a margin that's a fraction of the longest dimension. If empty, does
     nothing. *)
  val addmarginfrac : bounds -> real -> unit

  (* A common thing to do is collect some points into a bounding box,
     which we then want to represent as a graphic of a different
     scale and origin, like a 1000x1000 pixel box whose bottom left is
     0,0 (call these "screen coordinates").

     This type is a transformation conveniently derived from
     the bounds and desired screen coordinates. (It's called a scaler
     but it also involves at least a translation. TODO: Allow flipping
     the coordinate system vertically, too.) The scaler is immutable,
     even if the bounds it was derived from is modified. *)
  type scaler
  (* Maps a point from the original coordinates (i.e., what was inserted
     into bounds) to screen coordinates. *)
  val scale : scaler -> real * real -> real * real
  (* Make the bounding box as large as possible without modifying its
     aspect ratio. *)
  (* TODO: val scaletofit : bounds -> real * real -> scaler *)
  (* Make the bounding box fit the screen, stretching as necessary. *)
  val stretch : bounds -> real * real -> scaler

end