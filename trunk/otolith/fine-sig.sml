(* Subpixels used in physics. *)
signature FINE =
sig
  eqtype fine
  (* Raw access. *)
  val fromint : int -> fine
  val toint : fine -> int

  (* Place in center (almost) of coarse pixel. *)
  val fromcoarse : int -> fine
  (* Round to nearest coarse pixel. *)
  val tocoarse : fine -> int

  val + : fine * fine -> fine
  val - : fine * fine -> fine
  val * : fine * fine -> fine
  val div : fine * fine -> fine
  val < : fine * fine -> bool
  val <= : fine * fine -> bool
  val > : fine * fine -> bool
  val >= : fine * fine -> bool
  val ~ : fine -> fine

  (* Advance to the first fine coordinate that has a coarse
     coordinate larger than the argument's. *)
  val barely_next_pixel : fine -> fine
  (* Same, but to the next smaller fine coordinate that has
     a coarse coordinate smaller than the argument's. *)
  val barely_prev_pixel : fine -> fine

  val MULT : fine
  val CENTER : fine
  (* A whole coarse pixel in fine units (same as MULT). *)
  val PIXEL : fine
end
