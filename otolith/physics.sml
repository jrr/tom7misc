structure Physics :> PHYSICS =
struct

  exception Physics of string

  datatype shape =
    (* width, height; centered at x,y. Odd dimensions
       are best (XXX require?), so that the center is a pixel. *)
    Rect of int * int

  (* XXX more... *)
  datatype body =
    B of { x : int ref,
           y : int ref,
           shape : shape ref }

  fun setxy (B { x, y, ... }) (xx, yy) =
    let in
      x := xx;
      y := yy
    end
  fun getxy (B { x, y, ... }) = (!x, !y)

  fun setshape (B { shape, ... }) ss = shape := ss
  fun getshape (B { shape, ... }) = !shape

  fun bodies () = raise Physics "unimplemented"
  fun addbody body = raise Physics "unimplemented"
  fun delbody body = raise Physics "unimplemented"

  val locusf : (unit -> int * int) ref =
    ref (fn () => (Constants.WIDTH div 2, Constants.HEIGHT div 2))
  fun getlocus () = (!locusf)()
  fun setlocus f = locusf := f

  fun movebodies screen = raise Physics "unimplemented"
end