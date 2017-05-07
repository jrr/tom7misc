structure Bresenham :> BRESENHAM =
struct
  structure W = Word32
  val w2i = W.toIntX
  val i2w = W.fromInt

  type coord = int * int
  type state = { x0 : int, y0 : int, frac : int }

  (* XXX INLINE *)
  fun pair_swap (x, y) = (y, x)
  fun pair_map f (x, y) = (f x, f y)
  fun pair_map2 f (x1, y1) (x2, y2) = (f (x1, x2), f (y1, y2))

     fun build ((x0, y0), (x1, _), (dx, dy), (stepx, stepy), post) =
    let
      val frac0 = dy - Int.quot (dx, 2)
      fun step { x0, y0, frac } =
        if x0 = x1
        then NONE
        else
          let
            val (y0, frac) = if frac >= 0
                             then (y0 + stepy, frac - dx)
                             else (y0, frac)
            val x0 = x0 + stepx
            val frac = frac + dy
          in SOME ({ x0 = x0, y0 = y0, frac = frac }, post (x0, y0))
          end
    in ({ step = step, state = { x0 = x0, y0 = y0, frac = frac0 } },
        post (x0, y0))
    end

  fun line p0 p1 =
    let
      val d = pair_map2 op- p1 p0
      fun abs c = if c < 0 then (~c, ~1) else (c, 1)
      val ((dx', stepx), (dy', stepy)) = pair_map abs d
      val step = (stepx, stepy)
      val dx'' = dx' * 2
      val dy'' = dy' * 2
      val d'' = (dx'', dy'')

      fun id x = x
      val build_args =
        if dx'' > dy''
        then (p0, p1, d'', step, id)
        else (pair_swap p0, pair_swap p1, pair_swap d'',
              pair_swap step, pair_swap)
    in build build_args
    end

  fun num_points (x0, y0) (x1, y1) =
    let
      val dx = x1 - x0
      val dy = y1 - y0
    in
      Int.max (Int.abs dx, Int.abs dy) + 1
    end

  fun points (x0, y0) (x1, y1) =
    let
      val ({ step, state }, start) = line (x0, y0) (x1, y1)

      fun get state =
        case step state of
          SOME (state, coord) => coord :: get state
        | NONE => nil
    in
      start :: get state
    end

  fun all pred p0 p1 =
    let
      val ({ step, state }, v) = line p0 p1
      fun loop state =
        case step state of
          NONE => true
        | SOME (state', v) => pred v andalso loop state'
    in pred v andalso loop state
    end

  fun exists pred p0 p1 =
    let
      val ({ step, state }, v) = line p0 p1
      fun loop state =
        case step state of
          NONE => false
        | SOME (state', v) => pred v orelse loop state'
    in pred v orelse loop state
    end

  fun app f p0 p1 =
    let
      val ({ step, state }, v) = line p0 p1
      fun loop state =
        case step state of
          NONE => ()
        | SOME (state', v) => (f v; loop state')
    in
      f v;
      loop state
    end

end
