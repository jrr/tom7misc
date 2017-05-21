structure Physics :> PHYSICS =
struct

  exception Physics of string

  structure Areas = Screen.Areas
  structure Obj = Screen.Obj

  datatype shape =
    (* width, height; centered at x,y. Odd dimensions
       are best (XXX required?), so that the center is a pixel. *)
    Rect of int * int

  val DEBUG = false
  fun dprint f =
    if DEBUG
    then print (f ())
    else ()

  type fine = Fine.fine

  datatype dir = U | D | L | R
  datatype lr = Left | Right

  fun dir_unit_vector U = (0, ~1)
    | dir_unit_vector D = (0, 1)
    | dir_unit_vector L = (~1, 0)
    | dir_unit_vector R = (1, 0)

  datatype body_data = BD of
    { x : fine, y : fine, dx : fine, dy : fine,
      facing : lr,
      lrwish : lr option, jumpwish : bool,
      shape : shape }

  (* XXX more... *)
  datatype body =
    B of { x : fine ref,
           y : fine ref,
           dx : fine ref,
           dy : fine ref,

           facing : lr ref,

           lrwish : lr option ref,
           jumpwish : bool ref,
           (* Used for body equality and error checking. Could instead be
              like a doubly-linked list cell, which would make deleting
              constant-time. *)
           inscene : bool ref,
           shape : shape ref,

           (* Debugging. *)
           history : body_data LastNBuffer.buffer option ref }

    fun newbody () =
      B { x = ref (Fine.fromcoarse 128),
          y = ref (Fine.fromcoarse 128),
          dx = ref (Fine.fromint 0),
          dy = ref (Fine.fromint 0),
          facing = ref Right,
          lrwish = ref NONE,
          jumpwish = ref false,
          inscene = ref false,
          shape = ref (Rect (9, 9)),
          history = ref NONE }

  fun getdata (B { x, y, dx, dy, facing, lrwish, jumpwish,
                   inscene = _, shape,
                   history = _ }) =
    BD { x = !x, y = !y, dx = !dx, dy = !dy, facing = !facing,
         lrwish = !lrwish, jumpwish = !jumpwish,
         shape = !shape }

  fun setdata (B { x, y, dx, dy, facing, lrwish, jumpwish,
                   inscene = _, shape,
                   history = _}) (BD r) =
    let in
      x := #x r;
      y := #y r;
      dx := #dx r;
      dy := #dy r;
      facing := #facing r;
      lrwish := #lrwish r;
      jumpwish := #jumpwish r;
      shape := #shape r
    end

  fun setxy (B { x, y, ... }) (xx, yy) =
    let in
      x := Fine.fromcoarse xx;
      y := Fine.fromcoarse yy
    end
  fun getxy (B { x, y, ... }) = (Fine.tocoarse (!x),
                                 Fine.tocoarse (!y))

  fun setdxy (B { dx, dy, ... }) (dxx, dyy) =
    let in
      dx := dxx;
      dy := dyy
    end
  fun getdxy (B { dx, dy, ... }) = (!dx, !dy)

  fun setfacing (B { facing, ... }) f = facing := f
  fun getfacing (B { facing, ... }) = !facing

  fun setlrwish (B { lrwish, ... }) lr = lrwish := lr
  fun getlrwish (B { lrwish, ... }) = !lrwish

  fun setjumpwish (B { jumpwish, ... }) jump = jumpwish := jump
  fun getjumpwish (B { jumpwish, ... }) = !jumpwish

  fun setshape (B { shape, ... }) ss = shape := ss
  fun getshape (B { shape, ... }) = !shape

  val bodies : body list ref = ref nil
  val locusf : (unit -> int * int) ref =
    ref (fn () => (Constants.WIDTH div 2, Constants.HEIGHT div 2))

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

  fun getlocus () = (!locusf)()
  fun setlocus f = locusf := f

  (* Coarse pixel-based collision for an edge of a rectangular
     body moving in its normal direction.

     Consider the bottom edge of this body:

        +-+-+-+-+-+
        | | | | | |
        +-+-+-+-+-+
        | | | | | |
        +-+-+-+-+-+
        | | | | | |
        +-+-+-+-+-+

     The body is assumed to be non-colliding (i.e., all its
     pixels are empty) before the call.* We move the body down
     one coarse pixel (because this may change the locus) and
     then ask whether any pixel in the bottom row is occupied.

     If no pixels are occupied, then the body can move freely
     along this axis and the result is Air.

     If more than one pixel is occupied, or there is a single
     occupied pixel but it's not in the corner, then the
     result is Blocked.

     To enable motion along angled surfaces, we also allow the
     body to be ejected up to one pixel. This happens when
     exactly one corner pixel is occupied (recall that we are
     only testing the bottom edge, so this is really to say
     that one of the edge's endpoints is occupied):

        +-+-+-+-+-+
       #| | | | | |
        +-+-+-+-+-+
       #| | | | | |
        +-+-+-+-+-+
       #|#| | | | |
        +-+-+-+-+-+
       # #   v

       # #

     In this case, ejection is possible if moving the body to
     the right one pixel leaves the (entire) right edge unblocked.
     We test this by recursively invoking getcontact on the right
     edge. In this recursive invocation we don't allow ejection,
     in order to prevent loops.
       - If the right edge is completely clear, then the result
         is Eject.
       - If the right edge has any pixels, then the result is
         Blocked. Both the corner pixel (in the y+1 configuration)
         and right pixel(s) (in the x+1,y+1 configuration) are
         implicated.

     (XXX to dynamics part:
      In the ejection case, it probably should affect velocity;
      consider the case when jumping up a very shallow upward
      slope, where you happen to land your right foot on a pixel
      and get ejected left.)

     * Note that in the recursive call for ejection, there IS
       GUARANTEED to be an occupied pixel, violating its own
       assumption?

     Note that using this edge contact method can allow a body
     (that moves the locus) to enter a previously-free space
     (say, with its feet) that causes some object to overlap
     its head. TODO: Not sure what to do about this (might not
     be a problem?)

     *)
  datatype contacttype =
    (* Not touching anything in the normal direction. *)
      Air
    (* Can not move in this direction. *)
    | Blocked
    (* Can move, but ejection needed in the returned direction,
       which will be perpendicular to the movement direction. *)
    | Eject of dir

  fun dirstring U = "U"
    | dirstring D = "D"
    | dirstring L = "L"
    | dirstring R = "R"
  fun contactstring Air = "A"
    | contactstring Blocked = "B"
    | contactstring (Eject d) = "E" ^ dirstring d

  (* Move the body one pixel in the given direction, and call the
     continuation. Restores the old position. Doesn't do anything
     about exceptions, so those should be considered fatal. *)
  fun save_excursion (B { x, y, ... }, dir) k =
    let
      val oldx = !x
      val oldy = !y

      val (dx, dy) =
        case dir of
          U => (Fine.fromint 0, Fine.~ Fine.PIXEL)
        | D => (Fine.fromint 0, Fine.PIXEL)
        | L => (Fine.~ Fine.PIXEL, Fine.fromint 0)
        | R => (Fine.PIXEL, Fine.fromint 0)

      val () = x := Fine.+ (oldx, dx);
      val () = y := Fine.+ (oldy, dy);

      val res = k ()
    in
      x := oldx;
      y := oldy;
      res
    end

  (* pixelhit (screen, (lx, ly)) (px, py)
     Get the first object that contains the coarse pixel (px, py)
     when the locus is at (lx, ly). Properly staged. *)
  fun pixelhit (screen, (lx, ly)) =
    case Areas.gettriangle (Screen.areas screen) (lx, ly) of
      NONE => (fn _ => NONE)
    | SOME ((), area) =>
      let
        val (a, b, c) = Areas.T.nodes area
        fun hit (px, py) =
          let
            (* XXX still need to figure out the right way to
               extract commonly pasted code here... *)
            val (la, lb, lc) =
              IntMaths.barycentric (Areas.N.coords a (),
                                    Areas.N.coords b (),
                                    Areas.N.coords c (),
                                    (px, py))

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
              in
                IntMaths.pointinside (dpt, ept, fpt) (px, py)
              end

            (* Test a hit against one object. *)
            fun oneobject obj =
              Obj.iskey obj a andalso
              Obj.iskey obj b andalso
              Obj.iskey obj c andalso
              List.exists trianglehit (Obj.triangles obj)
          in
            List.find oneobject (Screen.objs screen)
          end
      in
        hit
      end

  (* Get coordinates and so on for one of the body's edges. Terminology
     is in terms of the bottom edge, so e.g. left_dir is L in that case. *)
  fun get_body_edge (body, dir) =
    let
      val (bx, by) = getxy body
      val Rect (fullw, fullh) = getshape body
      val halfw = fullw div 2
      val halfh = fullh div 2
      val (bottomleft, bottomright, width, right, left_dir, right_dir) =
        case dir of
          D => ((* bottom left *)
                (bx - halfw, by + halfh),
                (* bottom right *)
                (bx + halfw, by + halfh),
                fullw,
                (1, 0),
                L, R)
        | U => ((* top right *)
                (bx + halfw, by - halfh),
                (* top left *)
                (bx - halfw, by - halfh),
                fullw,
                (~1, 0),
                R, L)
        | L => ((* top left *)
                (bx - halfw, by - halfh),
                (* bottom left *)
                (bx - halfw, by + halfh),
                fullh,
                (0, 1),
                U, D)
        | R => ((* bottom right *)
                (bx + halfw, by + halfh),
                (* top right *)
                (bx + halfw, by - halfh),
                fullh,
                (0, ~1),
                D, U)
    in
      { bottomleft = bottomleft,
        bottomright = bottomright,
        width = width,
        right = right,
        left_dir = left_dir,
        right_dir = right_dir }
    end


  (* Move the body in both directions and then check whether
     its corner pixel (in those two directions) is occupied.
     dir1 and dir2 must be perpendicular.
     TODO: should maybe return implicated body too? *)
  fun corner_pixel (screen, body, dir1, dir2) : bool =
    save_excursion (body, dir1)
    (fn () =>
     save_excursion (body, dir2)
     (fn () =>
      let
        val { bottomleft, bottomright, left_dir, right_dir, ... } =
          get_body_edge (body, dir1)

        (* The corner pixel to test. *)
        val (px, py) =
          (* The directions are perpendicular, we'll return one of bottomleft
             or bottomright. *)
          if left_dir = dir2
          then bottomleft
          else if right_dir = dir2
               then bottomright
               else raise Physics "args to corner_pixel are not perp"

        val (lx, ly) = getlocus ()
      in
        case pixelhit (screen, (lx, ly)) (px, py) of
          NONE => false
        | SOME _ => true
      end))

  (* In any contact type (except air), we return any objects (with a
     locus, which may be different in the case of ejection) that
     explain the contact. This may not be a complete set, in case of
     overlapping objects. TODO: Collisions with other bodies. *)
  fun getcontact (screen, body, original_dir : dir) :
    contacttype * ((int * int) * Screen.obj) list =
    let
      (* We need recursive invocations, which have a different
         contract. *)
      fun getcon (dir : dir, allow_ejection) =
        save_excursion (body, dir)
        (fn () =>
         (* Body is now moved one pixel in the given direction. *)
         let
           (* Integer vec2 ops *)
           infix 6 /- /+
           fun (x, y) /- (xx, yy) = (x - xx, y - yy)
           fun (x, y) /+ (xx, yy) = (x + xx, y + yy)

           (* The following logic is written in the terminology of
              the botom edge. *)
           val { bottomleft, bottomright, width, right, left_dir, right_dir } =
             get_body_edge (body, dir)

           val leftcorner = ref false
           val rightcorner = ref false
           val hitcount = ref 0

           val implicated = ref nil

           val (lx, ly) = getlocus ()
           val pixelhithere = pixelhit (screen, (lx, ly))
           fun testpixel px =
             case pixelhithere px of
               NONE => ()
             | SOME obj =>
                 let in
                   if px = bottomleft
                   then leftcorner := true
                   else ();
                   if px = bottomright
                   then rightcorner := true
                   else ();
                   hitcount := !hitcount + 1;
                   implicated := ((lx, ly), obj) :: !implicated
                 end

           (* Run testpixel for every pixel on the edge.
              PERF: Can unroll a little to get the corner tests out
              of the general case. *)
           fun testedge px =
             let in
               testpixel px;
               if px = bottomright
               then ()
               else testedge (px /+ right)
             end

         in
           testedge bottomleft;
           case !hitcount of
             0 => (Air, nil)
           | 1 =>
               (case (!leftcorner, !rightcorner) of
                  (* For bodies of width 1 *)
                  (true, true) => (Blocked, !implicated)
                | (true, _) =>
                    (case getcon (right_dir, false) of
                       (Air, _) => (Eject right_dir, !implicated)
                     | (_, more_implicated) =>
                         (Blocked, more_implicated @ !implicated))
                | (_, true) =>
                    (case getcon (left_dir, false) of
                       (Air, _) => (Eject left_dir, !implicated)
                     | (_, more_implicated) =>
                         (Blocked, more_implicated @ !implicated))
                | (false, false) =>
                    (* single-pixel mid-edge collision *)
                    (Blocked, !implicated))
           | _ => (Blocked, !implicated)
         end)
    in
      getcon (original_dir, true)
    end

  (* In 256ths of a pixel per frame per frame. *)
  val GRAVITY : fine = Fine.fromint 6
  val HORIZ_ACCEL_GROUND : fine = Fine.fromint 7
  val HORIZ_ACCEL_AIR : fine = Fine.fromint 3
  val MAX_SPEED : fine = Fine.fromint 200
  val DECEL_AIR : fine = Fine.fromint 1
  val DECEL_GROUND : fine = Fine.fromint 4
  val JUMP_IMPULSE : fine = Fine.fromint (24 * 16)
  (* 16ths of pixel per frame *)
  val TERMINAL_VELOCITY : fine = Fine.fromint 260

  val MIN_CLIMB_VELOCITY : fine = HORIZ_ACCEL_GROUND

  (* XXX need some way to return the implicated objects
     (from the call to getcontact via 'go', perhaps including
     the ontheground call too), because the objects she's
     colliding with aren't even necessarily visible (could
     be in a different area) at the currently displayed locus! *)
  fun movebody (screen, body as B { x, y, dx, dy, facing, shape,
                                    lrwish, jumpwish, history, ... }) =
    let
      val () =
        case !history of
          NONE => ()
        | SOME buf => LastNBuffer.push_front (buf, getdata body)

      (* TODO: with time-varying locus, it may have moved since
         last frame, which might have put bodies inside walls.
         First push them out. Requires some history... *)
      val -- = Fine.-
      val ++ = Fine.+
      infix 6 -- ++

      (* XXX should implicate what we're standing on? *)
      val start_downcontact : contacttype = #1 (getcontact (screen, body, D))
      val ontheground =
        case start_downcontact of
          Blocked => true
        | Eject R =>
            Fine.<= (!dx, Fine.~ MIN_CLIMB_VELOCITY)
        | Eject L =>
            Fine.>= (!dx, MIN_CLIMB_VELOCITY)
        | _ => false

      val () = dprint (fn () =>
                       "Start: " ^ contactstring start_downcontact ^
                       (if ontheground then " (otg)\n" else "\n"))

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

      (* Acceleration due to gravity. If on the ground, then
         dy is set to be non-positive. *)
      val () =
        if ontheground
        then dy := Fine.min (!dy, Fine.fromint 0)
        else dy := !dy ++ GRAVITY

      (* TODO: x-gravity "wind" also easy *)

      (* XXX: When running down shallow slopes, there are
         moments where we are not "on the ground". It should
         always be possible to jump! *)
      (* XXX: might allow accelerating up slopes if you
         manage to jump while already moving up. Not clear
         if this is desirable. Fix might be in "ontheground"
         or here... *)
      val () =
        if !jumpwish andalso ontheground
        then
          let in
            dy := !dy -- JUMP_IMPULSE;
            jumpwish := false
          end
        else ()

      val () =
        if ontheground
        then (case !lrwish of
                NONE => ()
              | SOME lr => facing := lr)
        else ()

      (* acceleration due to player control. *)
      fun accelerate () =
        let
          val accel =
            if ontheground
            then HORIZ_ACCEL_GROUND
            else HORIZ_ACCEL_AIR
        in
          case !lrwish of
            SOME Left =>
              (case getcontact (screen, body, L) of
                 (Blocked, _) => false
               | _ =>
                   let in
                     dx := !dx -- accel;
                     true
                   end)
          | SOME Right =>
               (case getcontact (screen, body, R) of
                  (Blocked, _) => false
                | _ =>
                    let in
                      dx := !dx ++ accel;
                      true
                    end)
          | NONE => false
        end

      (* Allow the player to accelerate if she can. But
         if not, then apply air or ground deceleration. *)
      val () =
        if accelerate ()
        then ()
        else let
               val decel =
                 if ontheground
                 then DECEL_GROUND
                 else DECEL_AIR
             in
               if Fine.> (!dx, decel)
               then dx := !dx -- decel
               else if Fine.< (!dx, Fine.~ decel)
                    then dx := !dx ++ decel
                    else dx := Fine.fromint 0
             end

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

      (* OK, now we have the desired velocity vector, so we apply it.
         This is where it gets complicated. We're moving in this fine
         integer space, but collisions happen between coarse pixels.
         We move along a fine integral line (Bresenham's algorithm),
         but if we have collisions at the coarse level, then we may
         need to make adjustments (ejections) to allow smooth motion
         up a stair-step slope. Since we may deviate from the strict
         Bresenham line while executing the velocity vector, we can't
         necessarily keep the same Bresenham state for the whole
         motion. And although we are usually moving one coarse pixel
         or left, simply stopping does not work well at all. We get
         in loops like this:

                                           +-----+ (not on ground)
               +-----+  dxdy (~200, 0)     |     | dxdy (~200, 6)
               |     |  <- eject up        |     | | eject right
               |     |                     +-----+ v
          #####+-----+                 #####
          #####                        #####
          #####           (A)          #####          (B)

         When in state (A), the very first fine pixel motion moves us
         into the corner pixel, so we eject up. Since we stop
         immediately, we are only 1 fine pixel overlapping the
         lip--despite having a large dx magnitude. Now in state (B)
         we start tracing at fine coordinates, but the first coarse
         step we make is downward. This requires us to eject to
         the right, which loops us back to (A).

         It may be wrong to eject right when the major direction is
         L, so we should fix that. But it's also problematic that we
         made only 1 fine pixel of progress despite having ~200 magnitude
         in the state (A).

         So, we start by computing the major axis and the number of
         ticks (fine steps) along this axis. If we need to do any
         ejections, we can create a new bresenham line and execute
         it.
          - does the new line go from the current state (after ejection)
            to the old destination? Or to the current state + dx/dy?
            The current reapplies dx/dy to get a new end position.

         But we only execute this new line for the number of ticks
         remaining. (With the possibility of ejection, it's not clear that
         we would always terminate otherwise.) *)

      (* For a fine vector <dx,dy>, get the major axis and its length. *)
      fun getmaj (dx, dy) =
        let
          val dxmag = Fine.abs dx
          val dymag = Fine.abs dy
        in
          if dxmag = dymag
          (* XXX: We could use something else to break ties here,
             like possibly having some histeresis? I preferred
             L/R for smoother motion, but perhaps D results in
             fewer explotable glitches? *)
          then (case Fine.sign dx of
                  ~1 => (L, dxmag)
                | 1 => (R, dxmag)
                | _ (* zero *) => (D, dymag))
          else if Fine.< (dxmag, dymag)
               then (case Fine.sign dy of
                       ~1 => (U, dymag)
                     | _ => (D, dymag))
               else (case Fine.sign dx of
                       ~1 => (L, dxmag)
                     | _ => (R, dxmag))
        end

      (* Run the motion, but only for up to ticks_left more steps.
         (see above). *)
      fun apply_motion ticks_left =
        let
          val () = dprint (fn () =>
                           "applymotion " ^ Int.toString ticks_left ^ "\n")
          (* PERF can just exit if dx = dy = 0 *)

          val (step, state, major_dir) =
            let
              val dx = !dx
              val dy = !dy
              val endx = !x ++ dx
              val endy = !y ++ dy

              (* The first pixel in the line is returned as the second
                 parameter. There's no point in checking it because
                 we're already there. *)
              val ({ step, state }, _) =
                Bresenham.line (Fine.toint (!x), Fine.toint (!y))
                               (Fine.toint endx, Fine.toint endy)

              val (major, _) = getmaj (dx, dy)

              val () = dprint
                (fn () =>
                 (if ontheground then "[otg] "
                  else "[   ] ") ^
                     Fine.tostring (!x) ^ "," ^ Fine.tostring (!y) ^
                     " + " ^
                     Fine.tostring dx ^ "," ^ Fine.tostring dy ^
                     " maj " ^ dirstring major ^ "\n")
            in
              (step, state, major)
            end

          (* Generate the next point along the vector. If it's clear,
             we update our position and continue. *)
          fun move (0, state) = ()
            | move (ticks_left, state) =
            case step state of
              NONE => ()
            | SOME (state, (xx, yy)) =>
                let
                  val xx = Fine.fromint xx
                  val yy = Fine.fromint yy

                  val (bx, by) = getxy body

                  (* Check to see if this fine location would move the
                     body to a new coarse location. *)
                  val nbx = Fine.tocoarse xx
                  val nby = Fine.tocoarse yy

                  fun flatten dir =
                    (* Flatten the velocity vector along the blocked axis. *)
                    case dir of
                      U => dy := Fine.fromint 0
                    | D => dy := Fine.fromint 0
                    | L => dx := Fine.fromint 0
                    | R => dx := Fine.fromint 0

                  (* Move as the line would have us move. *)
                  fun accept () =
                    let in
                      x := xx;
                      y := yy
                    end
                  fun accept_and_continue () =
                    let in
                      x := xx;
                      y := yy;
                      move (ticks_left - 1, state)
                    end

                  fun replan () = apply_motion (ticks_left - 1)

                  (* Geometry is forcing us to move (one coarse pixel) in given
                     direction, even though it is not exactly how the line
                     would have us move. Move the body to the closest fine
                     location that has this coarse coordinate. This
                     minimizes error so should give slightly smoother
                     motion.

                     When ejecting in a given direction, we also cancel any
                     velocity exactly opposite to that direction. *)
                  fun eject dir =
                    (case dir of
                       U =>
                         let in
                           dy := Fine.max (Fine.fromint 0, !dy);
                           y := Fine.barely_prev_pixel (!y);
                           dprint (fn () =>
                                   "Eject up y " ^ Fine.tostring (!y) ^
                                   " dy " ^ Fine.tostring (!dy) ^ "\n")
                         end
                     | D =>
                         let in
                           dy := Fine.min (Fine.fromint 0, !dy);
                           y := Fine.barely_next_pixel (!y);
                           dprint (fn () =>
                                   "Eject down y " ^ Fine.tostring (!y) ^
                                   " dy " ^ Fine.tostring (!dy) ^ "\n")
                         end
                     | L =>
                         let in
                           dx := Fine.min (Fine.fromint 0, !dx);
                           x := Fine.barely_prev_pixel (!x);
                           dprint (fn () =>
                                   "Eject left x " ^ Fine.tostring (!x) ^
                                   " dx " ^ Fine.tostring (!dx) ^ "\n")
                         end
                     | R =>
                         let in
                           dx := Fine.max (Fine.fromint 0, !dx);
                           x := Fine.barely_next_pixel (!x);
                           dprint (fn () =>
                                   "Eject right x " ^ Fine.tostring (!x) ^
                                   " dx " ^ Fine.tostring (!dx) ^ "\n")
                         end)

                  (* XXX I wonder if this can just generalize 'go'? *)
                  fun go2 (dir1, dir2) =
                    let
                      val (c1, c2) = (getcontact (screen, body, dir1),
                                      getcontact (screen, body, dir2))
                      val () = dprint (fn () =>
                                       "go2 " ^
                                       dirstring dir1 ^ " " ^
                                       dirstring dir2 ^ " -> " ^
                                       contactstring (#1 c1) ^ " " ^
                                       contactstring (#1 c2) ^ "\n")
                    in
                      case (c1, c2) of
                        ((Air, _), (Air, _)) =>
                          (* Though the two directions (wlog: down and
                             right) are clear on their own, the
                             combination might still enter a pixel.

                              +--+
                              |  |     \,
                              +--+     ``
                                  #

                             If this is the case we try moving along
                             the major axis (only). *)
                          if corner_pixel (screen, body, dir1, dir2)
                          then
                            let in
                              dprint (fn () =>
                                      "air/air corner px, maj: " ^
                                      dirstring major_dir ^ "\n");

                              (* In this case we only move in the major
                                 dir (via eject, to try to stay as close
                                 as possible to the corner), but we don't
                                 accept the new position (it's illegal).

                                 XXX Alternate would be to accept and then
                                 eject in that order -- might be better? *)
                              if dir1 = major_dir
                              then
                                let in
                                  eject dir1;
                                  (* XXX flatten? *)
                                  replan ()
                                end
                              else if dir2 = major_dir
                                   then
                                     let in
                                       eject dir2;
                                       (* XXX flatten? *)
                                       replan ()
                                     end
                                   else raise Physics "impossible go2/aa"
                            end
                          else accept_and_continue ()
                      | ((Blocked, _), (Air, _)) =>
                          let in
                            flatten dir1;
                            replan ()
                          end
                      | ((Air, _), (Blocked, _)) =>
                          let in
                            flatten dir2;
                            replan ()
                          end
                      | ((Blocked, _), (Blocked, _)) =>
                          let in
                            flatten dir1;
                            flatten dir2
                            (* no point in replanning -- velocity is 0 now *)
                          end
                      | ((Eject ejection_dir, _), (Air, _)) =>
                          if ejection_dir = dir2
                          then
                            (* The new diagonal position accounts for the
                               ejection we need. XXX I think this actually
                               needs to do the corner pixel test as in
                               the air, air case? *)
                            accept_and_continue ()
                          else
                            let in
                              (* This is a situation like this, when
                                 moving diagonally down-right. Right is
                                 clear, but down would require rejecting
                                 left.

                                       +--+   \
                                       |  |    \,
                                       +--+    ``
                                          ###
                                         ####

                                 Guess it makes sense to break the tie
                                 based on the fully-clear pixel. *)
                              flatten dir1;
                              replan ()
                            end
                      | ((Air, _), (Eject ejection_dir, _)) =>
                          if ejection_dir = dir1
                          then accept_and_continue ()
                          else
                            let in
                              flatten dir2;
                              replan ()
                            end
                      | _ =>
                          let in
                            flatten dir1;
                            flatten dir2
                            (* Velocity is zero, so just stop *)
                          end
                    end

                  (* Move in the given direction, coarsely, if possible. *)
                  fun go dir =
                    let
                      val c = getcontact (screen, body, dir)
                      val () = dprint (fn () =>
                                       "go " ^ dirstring dir ^ " -> " ^
                                       contactstring (#1 c) ^ "\n")
                    in
                      case c of
                        (Air, _) => accept_and_continue ()
                      | (Blocked, implicated) =>
                          let in
                            flatten dir;
                            replan ()
                          end
                      | (Eject ejection_dir, implicated) =>
                          let in
                            (* Go to the new position but then eject. *)
                            accept ();
                            eject ejection_dir;
                            replan ()
                          end
                    end
                in
                  case (nbx - bx, nby - by) of
                    (0, 0) =>
                      (* Since subpixels are 1/256th of coarse pixels, this
                         will be the most common case by far. *)
                      accept_and_continue ()
                  | (1, 0) => go R
                  | (~1, 0) => go L
                  | (0, 1) => go D
                  | (0, ~1) => go U
                    (* It's definitely possible (but rare) to have
                       UR, UL, DR, DL when moving exactly diagonally. *)
                  | (1, 1) => go2 (D, R)
                  | (~1, 1) => go2 (D, L)
                  | (1, ~1) => go2 (U, R)
                  | (~1, ~1) => go2 (U, L)
                  (* Impossible because Bresenham guarantees single fine
                     pixel steps. *)
                  | (deltax, deltay) =>
                      raise Physics ("impossible delta: " ^
                                     Int.toString deltax ^ "/" ^
                                     Int.toString deltay)
                end
        in
          move (ticks_left, state)
        end

      val (_, original_length) = getmaj (!dx, !dy)
    in
      apply_motion (Fine.toint original_length)
    end

  fun sethistorysize (body as B { history, ... }) n =
    history := SOME (LastNBuffer.buffer (n, getdata body))
  fun undo (body as B { history, ... }) =
    case !history of
      NONE => ()
    | SOME buf =>
        let
          val bd = LastNBuffer.sub (buf, 0)
        in
          setdata body bd;
          LastNBuffer.rotate_left buf
        end

  datatype debug_contact =
    DB_BLOCKED | DB_EJECT | DB_AIR

  fun getdebug screen body =
    let
      val ontheground =
        case getcontact (screen, body, D) of
          (Blocked, _) => true
        | _ => false

      fun mc dir =
        case getcontact (screen, body, dir) of
          (Blocked, _) => DB_BLOCKED
        | (Air, _) => DB_AIR
        | (Eject _, _) => DB_EJECT

      fun contacts () =
        { l = mc L,
          r = mc R,
          d = mc D }

      fun hit (lx, ly) (x, y) =
        case pixelhit (screen, (lx, ly)) (x, y) of
          NONE => false
        | SOME _ => true
    in
      { ontheground = ontheground,
        contacts = contacts,
        hit = hit }
    end

  fun movebodies screen =
    let in
      dprint (fn () => "-- movebodies --\n");
      (* Currently, processes the bodies in order.

         It would be good to do some kind of simultaneous solving,
         though. This probably behaves strangely with stacks of things
         (and differently if they are stacked in order or in reverse
         order!). *)
      app (fn b => movebody (screen, b)) (!bodies)
    end


  val bodies = fn () => !bodies
end
