structure Physics :> PHYSICS =
struct

  exception Physics of string

  structure Areas = Screen.Areas
  structure Obj = Screen.Obj

  datatype shape =
    (* width, height; centered at x,y. Odd dimensions
       are best (XXX require?), so that the center is a pixel. *)
    Rect of int * int

  structure Fine :>
  sig
    eqtype fine
    (* Raw access. *)
    val fromint : int -> fine
    val toint : fine -> int

    (* Place in center of coarse pixel. *)
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

    val MULT : fine
    val CENTER : fine
  end =
  struct
    type fine = int

    val MULT = 256
    val CENTER = MULT div 2

    fun fromint i = i
    fun toint i = i

    fun fromcoarse i = i * MULT + CENTER
    fun tocoarse f = (f + (CENTER - 1)) div MULT

    open Int
  end
  type fine = Fine.fine

  datatype lr = Left | Right

  (* XXX more... *)
  datatype body =
    B of { x : fine ref,
           y : fine ref,
           dx : fine ref,
           dy : fine ref,

           lrwish : lr option ref,
           jumpwish : bool ref,
           (* Used for body equality and error checking. Could instead be
              like a doubly-linked list cell, which would make deleting
              constant-time. *)
           inscene : bool ref,
           shape : shape ref }

  fun newbody() = B { x = ref (Fine.fromcoarse 128),
                      y = ref (Fine.fromcoarse 128),
                      dx = ref (Fine.fromint 0),
                      dy = ref (Fine.fromint 0),
                      lrwish = ref NONE,
                      jumpwish = ref false,
                      inscene = ref false,
                      shape = ref (Rect (9, 9)) }

  fun setxy (B { x, y, ... }) (xx, yy) =
    let in
      x := Fine.fromcoarse xx;
      y := Fine.fromcoarse yy
    end
  fun getxy (B { x, y, ... }) = (Fine.tocoarse (!x),
                                 Fine.tocoarse (!y))

  fun setdxy (B { dx, dy, ... }) (dxx, dyy) =
    let in
      dx := Fine.fromint dxx;
      dy := Fine.fromint dyy
    end
  fun getdxy (B { dx, dy, ... }) = (Fine.toint (!dx),
                                    Fine.toint (!dy))

  fun setlrwish (B { lrwish, ... }) lr = lrwish := lr
  fun getlrwish (B { lrwish, ... }) = !lrwish

  fun setjumpwish (B { jumpwish, ... }) jump = jumpwish := jump
  fun getjumpwish (B { jumpwish, ... }) = !jumpwish

  fun setshape (B { shape, ... }) ss = shape := ss
  fun getshape (B { shape, ... }) = !shape

  val bodies : body list ref = ref nil

  fun addbody (body as B { inscene = ref true, ... }) =
    raise Physics "body already in scene in addbody"
    | addbody (body as B { inscene, ... }) =
      let in
        inscene := true;
        bodies := body :: !bodies
      end

  fun delbody (body as B { inscene = ref false, ... }) =
    raise Physics "body not in scene in delbody"
    | delbody (body as B { inscene, ... }) =
      let in
        inscene := false;
        bodies := List.filter (fn other => other <> body) (!bodies)
      end

  val locusf : (unit -> int * int) ref =
    ref (fn () => (Constants.WIDTH div 2, Constants.HEIGHT div 2))
  fun getlocus () = (!locusf)()
  fun setlocus f = locusf := f

  (* Collision of an axis-aligned edge with something in the world
     (currently, just triangular objects). The following describes the
     behavior for the bottom edge; the other edges are analogous.

     Contact here means exact (coarse) pixel adjacency: We move the
     body one pixel down (axis-aligned), and then ask if, in that
     configuration, it is overlapping any triangle. In the case that
     the triangle is not parameterized (or the body's position does
     not influence the locus), this is just asking whether there is a
     solid pixel immediately below the edge.

     Ignoring coincident edges (XXX is this wise?) there are four types
     of contact:

     1. Air       |  body  |         (No contact.)
                  +--------+

     2. Single    |  body  |
                  +--------+  ...    (One of the feet contacts
                     ...---```        a single edge. This includes
               ---```                 the case of standing on a
                                      completely flat edge, where
                                      the contact foot is then
                                      chosen arbitrarily.)

     3. Valley    |  body  |         (Simultaneous contact with
                 \+--------+/         two edges. Not clear whether
                  \        /          this is actually separate
                   \      /           from Single, except that
                                      intuitively, we should be
                                      able to jump straight up
                                      from this position.)

     4. Peak      |  body  |         (Standing on a single point.
                  +--------+          Here the contact is not a foot
                      /\              but somewhere along the body's
                     /  \             bottom edge. Since all edges
                                      are on triangles, the point is
                                      always on two edges. There may
                                      also be interior edges.)

     For situations where there are multiple coincident edges, we
     just pick one arbitrarily (but consistently). Level designers
     should probably avoid such situations.

     The fact that edges can move when the body moves presents some
     complications:
       - In principle, a fast-moving object can move completely through
         the body. We don't attempt to handle this case.
       - In Single/Valley-type contact, the foot can penetrate
         deep into the triangle(s) in a single step.
       - In Peak contact, the peak may penetrate into the body, in such
         a way that one or more feet are also inside the triangle(s).

     Contact is resolved as follows. We assume that the body is not
     overlapping any triangle at its start position.
       - Place the body at the new position (one pixel down), which may
         affect the locus.
       - First, test if any triangle endpoints are inside the body. If
         so, this will be a Peak-type contact. Note however a case
         like this one:
                       /
            ++-------+/
            |        |
            |       /|
            |      * |
            |   .-`  |
            +--`-----+
           .-`

         ... I think this intuitively should be a Single contact (but
         with which edge?), especially since if the two edges are
         colinear then it *would* be Single contact if not for the
         endpoint. XXX: Not sure what to do about this one. May
         have to inspect the angle, or maybe it's even another type
         of contact?
       - If no endpoints are inside the body, test each foot. If the
         feet are in ... hmm, maybe change this idea.
*)
  datatype contact =
    (* Not touching anything (downward). *)
      Air
    (* One foot is on a single line segment. *)
    | Single of { isleft: bool, vecXXX: unit }
    (* Each foot on a different line segment. *)
    | Valley of { vecleftXXX: unit, vecrightXXX: unit }
    (* Standing on a single point shared by two vectors. *)
    | Peak of { vecleftXXX: unit, vecrightXXX: unit }
  (* Also can stand on objects, which I think just acts like
     a peak, or single flat vector. *)

  (* Get the current touching state.
     XXX this is a bit complicated, because the vectors can move
     as a result of our movement. Might need to propagate this
     across frames by "sticking" to a vector.

     perhaps this should be generalized to seeing how an axis-aligned
     edge collides? We may also need this for L/R collisions, hitting
     head, etc. *)
  fun getground (screen, body) =
    (* XXX *)
    Air

  (* In 256ths of a pixel per frame per frame. *)
  val GRAVITY : fine = Fine.fromint 8
  val HORIZ_ACCEL : fine = Fine.fromint 12
  val MAX_SPEED : fine = Fine.fromint 256
  val DECEL_AIR : fine = Fine.fromint 8
  val JUMP_IMPULSE : fine = Fine.fromint (24 * 16)
  (* 16ths of pixel per frame *)
  val TERMINAL_VELOCITY : fine = Fine.fromint 300

  (* Get the triangle objects that collide with the shape at (x, y).
     (x, y) is in coarse integral coordinates. *)
  fun objectcollisions (screen, shape, (x, y)) =
    let
      val (lx, ly) = !locusf()
    in
      case Areas.gettriangle (Screen.areas screen) (lx, ly) of
        NONE => nil
      | SOME ((), area) =>
          let
            val (a, b, c) = Areas.T.nodes area

            (* XXX This duplicates some drawing code, and probably also
               something we need to write for 'getground', etc -- some
               core needs to be extracted I think *)
            val (la, lb, lc) =
              IntMaths.barycentric (Areas.N.coords a (),
                                    Areas.N.coords b (),
                                    Areas.N.coords c (),
                                    (x, y))

            (* Get the coordinates for the node by interpolating
               between the three keys. *)
            fun transform n =
              let
                (* These shouldn't fail because we checked that the
                   key is a key of the object above. *)
                val (ax, ay) = Obj.N.coords n a
                val (bx, by) = Obj.N.coords n b
                val (cx, cy) = Obj.N.coords n c

                val nx = Real.round (real ax * la +
                                     real bx * lb +
                                     real cx * lc)
                val ny = Real.round (real ay * la +
                                     real by * lb +
                                     real cy * lc)
              in
                (nx, ny)
              end

            fun trianglehit t =
              let
                val (d, e, f) = Obj.T.nodes t
                val dpt = transform d
                val ept = transform e
                val fpt = transform f
                fun hit (xx, yy) = IntMaths.pointinside (dpt, ept, fpt) (xx, yy)
              in
                (* n.b., this allows a very narrow triangle to pass
                   between the player's feet... *)
                case shape of
                  Rect (w, h) =>
                    let
                      val whalf = w div 2
                      val hhalf = h div 2
                    in
                      hit (x - whalf, y - hhalf) orelse
                      hit (x + whalf, y - hhalf) orelse
                      hit (x - whalf, y + hhalf) orelse
                      hit (x + whalf, y + hhalf)
                    end
              end

            (* Test a hit against one object. *)
            fun oneobject obj =
              Obj.iskey obj a andalso
              Obj.iskey obj b andalso
              Obj.iskey obj c andalso
              List.exists trianglehit (Obj.triangles obj)
          in
            List.filter oneobject (Screen.objs screen)
          end
    end


  fun movebody (screen, body as B { x, y, dx, dy, shape,
                                    lrwish, jumpwish, ... }) =
    let
      (* TODO: with time-varying locus, it may have moved since
         last frame, which might have put bodies inside walls.
         First push them out. Requires some history... *)
      val -- = Fine.-
      val ++ = Fine.+
      infix 6 -- ++

      val ground = getground (screen, body)
      (* TODO:
         jumping when on an angled surface should apply
         x vector as well, I think, since we're "kicking off"
         that surface.

         - also, need some jump timer so that we keep accelerating
         for "big jump" vs "small jump" *)

      (* Apply some acceleration to our velocity vector.
         Here, we are not trying to handle any collisions,
         just get the correct initial velocity. So, gravity
         accelerates us straight downward, and player
         control is likewise exactly horizontal. *)

      (* acceleration due to gravity *)
      val () =
        case ground of
          Air => dy := !dy ++ GRAVITY
        | _ => ()

      (* TODO: x-gravity "wind" also easy *)

      (* XXX only allow this when the player is on the
         ground. *)
      val () =
        if !jumpwish
        then
          let in
            dy := !dy -- JUMP_IMPULSE;
            jumpwish := false
          end
        else ()

      (* acceleration due to player control. *)
      val () =
        case !lrwish of
          SOME Left => dx := !dx -- HORIZ_ACCEL
        | SOME Right => dx := !dx ++ HORIZ_ACCEL
        | NONE =>
            case ground of
              Air => if Fine.> (!dx, DECEL_AIR)
                     then dx := !dx -- DECEL_AIR
                     else if Fine.< (!dx, Fine.~ DECEL_AIR)
                          then dx := !dx ++ DECEL_AIR
                          else dx := Fine.fromint 0
            | _ => raise Physics "same but more deceleration"

      val () =
        if Fine.> (!dx, MAX_SPEED)
        then dx := MAX_SPEED
        else if Fine.< (!dx, Fine.~ MAX_SPEED)
             then dx := Fine.~ MAX_SPEED
             else ()

      (* XXX max vertical upward speed *)
      val () =
        if Fine.> (!dy, TERMINAL_VELOCITY)
        then dy := TERMINAL_VELOCITY
        else ()

      (* OK, now we have the desired velocity vector, so we apply
         it. To prevent quantum tunneling, we iterate along the
         velocity vector, checking at each integral point whether we
         have any collision with a body or (possibly moving) triangle
         object. *)
      (* The first pixel in the line is returned as the second parameter.
         There's no point in checking it because we're already there. *)
      (* val () = print ("dx,dy: " ^ Int.toString (!dx) ^ "," ^
         Int.toString (!dy) ^ "\n") *)
      val ({ step, state }, _) =
        let
          val endx = !x ++ !dx
          val endy = !y ++ !dy
        in
          Bresenham.line (Fine.toint (!x), Fine.toint (!y))
                         (Fine.toint endx, Fine.toint endy)
        end

      (* Generate the next point along the vector. If it's clear,
         we update our position. *)
      fun move state =
        case step state of
          NONE => ()
        | SOME (state, (xx, yy)) =>
            let
              val xx = Fine.fromint xx
              val yy = Fine.fromint yy
              (* Have to provisionally update the body's location,
                 because it may affect the locus. *)
              val oldx = !x
              val oldy = !y
              val () = x := xx
              val () = y := yy

              val coarsex = Fine.tocoarse xx
              val coarsey = Fine.tocoarse yy
            in
              (* PERF: Since we perform the collision at coarse
                 integral coordinates, we're usually testing the
                 same value as last round, and could often skip
                 it. This would cease to be true if the locus was
                 based on fine 256ths, though. *)
              (* XXX also body collision. *)
              (* XXX we should "materialize" these objects/edges
                 (at the collision locus) visually, because when we
                 back up the player to the valid location, the object
                 may not look like it will collide, or might not
                 even be visible! *)
              case objectcollisions (screen, !shape, (coarsex, coarsey)) of
                nil => move state
              | _ =>
              (* XXX should find collision edge(s), and:
                 - stick to edge?
                 - apply edge's velocity from collision to body?
                 - like if I jump up, next to a wall moving left,
                 - it should push me to the left, not stop my
                   movement. *)
                let in
                  x := oldx;
                  y := oldy;

                  (* Don't just stop! *)
                  dx := Fine.fromint 0;
                  dy := Fine.fromint 0
                end
            end
    in
      move state
    end


  fun movebodies screen =
    (* Currently, processes the bodies in order.

       It would be good to do some kind of simultaneous solving,
       though. This probably behaves strangely with stacks of things
       (and differently if they are stacked in order or in reverse
       order!). *)
    app (fn b => movebody (screen, b)) (!bodies)


  val bodies = fn () => !bodies
end