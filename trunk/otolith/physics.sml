structure Physics :> PHYSICS =
struct

  exception Physics of string

  structure Areas = Screen.Areas
  structure Obj = Screen.Obj

  datatype shape =
    (* width, height; centered at x,y. Odd dimensions
       are best (XXX required?), so that the center is a pixel. *)
    Rect of int * int

  type fine = Fine.fine

  datatype dir = U | D | L | R
  datatype lr = Left | Right

  fun dir_unit_vector U = (0, ~1)
    | dir_unit_vector D = (0, 1)
    | dir_unit_vector L = (~1, 0)
    | dir_unit_vector R = (1, 0)

  datatype body_data = BD of
    { x : fine, y : fine, dx : fine, dy : fine,
      lrwish : lr option, jumpwish : bool,
      shape : shape }

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
           shape : shape ref,

           (* Debugging. *)
           ontheground : bool ref,
           history : body_data LastNBuffer.buffer option ref }

    fun newbody () =
      B { x = ref (Fine.fromcoarse 128),
          y = ref (Fine.fromcoarse 128),
          dx = ref (Fine.fromint 0),
          dy = ref (Fine.fromint 0),
          lrwish = ref NONE,
          jumpwish = ref false,
          inscene = ref false,
          shape = ref (Rect (9, 9)),
          (* XXX *)
          ontheground = ref false,
          history = ref NONE }

  fun getdata (B { x, y, dx, dy, lrwish, jumpwish,
                   inscene = _, shape, ontheground = _,
                   history = _ }) =
    BD { x = !x, y = !y, dx = !dx, dy = !dy,
         lrwish = !lrwish, jumpwish = !jumpwish,
         shape = !shape }

  fun setdata (B { x, y, dx, dy, lrwish, jumpwish,
                   inscene = _, shape, ontheground = _,
                   history = _}) (BD r) =
    let in
      x := #x r;
      y := #y r;
      dx := #dx r;
      dy := #dy r;
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

  fun setlrwish (B { lrwish, ... }) lr = lrwish := lr
  fun getlrwish (B { lrwish, ... }) = !lrwish

  fun setjumpwish (B { jumpwish, ... }) jump = jumpwish := jump
  fun getjumpwish (B { jumpwish, ... }) = !jumpwish

  fun setshape (B { shape, ... }) ss = shape := ss
  fun getshape (B { shape, ... }) = !shape

  fun getontheground (B { ontheground, ... }) = !ontheground

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
           val (bottomleft, bottomright, width, right, left_dir, right_dir) =
             let
               val (bx, by) = getxy body
               val Rect (fullw, fullh) = getshape body
               val halfw = fullw div 2
               val halfh = fullh div 2
             in
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
             end

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
  val HORIZ_ACCEL_GROUND : fine = Fine.fromint 4
  val HORIZ_ACCEL_AIR : fine = Fine.fromint 2
  val MAX_SPEED : fine = Fine.fromint 200
  val DECEL_AIR : fine = Fine.fromint 1
  val DECEL_GROUND : fine = Fine.fromint 4
  val JUMP_IMPULSE : fine = Fine.fromint (24 * 16)
  (* 16ths of pixel per frame *)
  val TERMINAL_VELOCITY : fine = Fine.fromint 260

  (* XXX need some way to return the implicated objects
     (from the call to getcontact via 'go', perhaps including
     the ontheground call too), because the objects she's
     colliding with aren't even necessarily visible (could
     be in a different area) at the currently displayed locus! *)
  fun movebody (screen, body as B { x, y, dx, dy, shape,
                                    ontheground = debug_ontheground,
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

      val ontheground =
        case getcontact (screen, body, D) of
          (Blocked, _) => true
        | _ => false

      val () = debug_ontheground := ontheground

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
        if ontheground
        then ()
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

      (* OK, now we have the desired velocity vector, so we apply
         it. To prevent quantum tunneling, we iterate along the
         velocity vector, checking at each integral point whether we
         have any collision with a body or (possibly moving) triangle
         object. *)
      (* The first pixel in the line is returned as the second parameter.
         There's no point in checking it because we're already there. *)
      val ({ step, state }, _) =
        let
          val endx = !x ++ !dx
          val endy = !y ++ !dy
        in
          Bresenham.line (Fine.toint (!x), Fine.toint (!y))
                         (Fine.toint endx, Fine.toint endy)
        end

      (* Generate the next point along the vector. If it's clear,
         we update our position and continue. *)
      fun move state =
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

              fun accept () =
                let in
                  x := xx;
                  y := yy;
                  move state
                end

              (* XXX I wonder if this can just generalize 'go'? *)
              fun go2 (dir1, dir2) =
                let
                  val (c1, c2) = (getcontact (screen, body, dir1),
                                  getcontact (screen, body, dir2))
                  val () = print ("go2 " ^
                                  dirstring dir1 ^ " " ^
                                  dirstring dir2 ^ " -> " ^
                                  contactstring (#1 c1) ^ " " ^
                                  contactstring (#1 c2) ^ "\n")
                in
                  case (c1, c2) of
                    ((Air, _), (Air, _)) =>
                      (* XXX buggy in this case:

                         +--+
                         |  |     \,
                         +--+     ``
                             #

                         because it actually allows entering
                         the pixel. *)

                      accept ()
                  | ((Blocked, _), (Air, _)) =>
                      let in
                        flatten dir1
                      end
                  | ((Air, _), (Blocked, _)) =>
                      let in
                        flatten dir2
                      end
                  | ((Blocked, _), (Blocked, _)) =>
                      let in
                        flatten dir1;
                        flatten dir2
                      end
                  | ((Eject ejection_dir, _), (Air, _)) =>
                      if ejection_dir = dir2
                      then
                        (* The new position ejects us as needed. *)
                        accept ()
                      else
                        let in
                          (* XXX? This is a situation like this, when moving
                             diagonally down-right. Right is clear, but down
                             would require rejecting left.

                                   +--+   \
                                   |  |    \,
                                   +--+    ``
                                      ###
                                     ####

                             Guess it makes sense to break the tie based
                             on the fully-clear pixel. *)
                          flatten dir1
                        end
                  | ((Air, _), (Eject ejection_dir, _)) =>
                      if ejection_dir = dir1
                      then accept ()
                      else
                        (* XXX? as above. *)
                        flatten dir2
                    (* Anything else should be treated as a collision.
                       XXX Two ejections could be compatible, maybe? *)
                  | _ =>
                      let in
                        flatten dir1;
                        flatten dir2
                      end
                end

              (* Move in the given direction, coarsely, if possible. *)
              fun go dir =
                let
                  val c = getcontact (screen, body, dir)
                  val () = print ("go " ^ dirstring dir ^ " -> " ^
                                  contactstring (#1 c) ^ "\n")
                in
                  case c of
                    (Air, _) => accept ()
                  | (Blocked, implicated) =>
                      let in
                        flatten dir;
                        (* XXX would be more physical to restart
                           the line from the current position,
                           if the other component is nonzero. But
                           we'll still keep moving, on the next
                           frame. We'd need to know what fraction of
                           the vector we've already traveled. *)
                        ()
                      end
                  | (Eject ejection_dir, implicated) =>
                      let in
                        (* Move the body to the closest fine location that
                           has this coarse coordinate. This minimizes error
                           so should give slightly smoother motion. *)
                        (case ejection_dir of
                           U => y := Fine.barely_prev_pixel yy
                         | D => y := Fine.barely_next_pixel yy
                         | L => x := Fine.barely_prev_pixel xx
                         | R => x := Fine.barely_next_pixel xx);
                           (* XXX better to continue the line. But we can't
                              just call move recursively, because the
                              bresenham state just ignores the update we
                              made. *)
                           ()
                      end
                end
            in
              case (nbx - bx, nby - by) of
                (0, 0) =>
                  (* Since subpixels are 1/256th of coarse pixels, this
                     will be the most common case by far. *)
                  accept ()
              | (1, 0) => go R
              | (~1, 0) => go L
              | (0, 1) => go D
              | (0, ~1) => go U
                (* It's definitely possible (but rare) to have UR, UL, DR, DL
                   when moving exactly diagonally. *)
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
      move state
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
    (* Currently, processes the bodies in order.

       It would be good to do some kind of simultaneous solving,
       though. This probably behaves strangely with stacks of things
       (and differently if they are stacked in order or in reverse
       order!). *)
    app (fn b => movebody (screen, b)) (!bodies)


  val bodies = fn () => !bodies
end
