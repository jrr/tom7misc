structure Fine :> FINE =
struct
  type fine = int

  (* Could maybe be functorized over the multiple? *)
  val MULT = 256
  val CENTER = MULT div 2
  val PIXEL = MULT

  fun fromint i = i
  fun toint i = i

  fun fromcoarse i = i * MULT + CENTER
  fun tocoarse f = (f + (CENTER - 1)) div MULT

  fun barely_next_pixel i =
    let
      (* What coarse pixel do we want? *)
      val c = tocoarse i + 1

      (* Smallest value such that s div MULT = c *)
      val s = c * MULT
    in
      (* tocoarse adds CENTER - 1 before dividing, so, subtract *)
      s - (CENTER - 1)
    end

  fun barely_prev_pixel i =
    let
      val c = tocoarse i - 1
      (* Biggest value such that b div MULT = c *)
      val b = (c + 1) * MULT - 1
    in
      b - (CENTER - 1)
    end

  open Int
end
