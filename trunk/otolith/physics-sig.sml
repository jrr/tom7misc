signature PHYSICS =
sig

  exception Physics of string

  (* A rectangular thing on the screen that gets physical
     simulation. *)
  type body
  val newbody : unit -> body

  (* Screen coordinates. Note that internally, physics keeps
     sub-pixel positions. (TODO: Make this available too?) *)
  val setxy : body -> int * int -> unit
  val getxy : body -> int * int

  (* Quantities are in 256ths of a pixel per frame *)
  val setdxy : body -> Fine.fine * Fine.fine -> unit
  val getdxy : body -> Fine.fine * Fine.fine

  (* Indicates whether the body wants to move left/right;
     these are mutually exclusive. *)
  datatype lr = Left | Right

  (* also up/down? *)
  val setlrwish : body -> lr option -> unit
  val getlrwish : body -> lr option
  val setjumpwish : body -> bool -> unit
  val getjumpwish : body -> bool

  (* TODO shapes: individual pixels and other dust. circles. *)
  datatype shape =
    (* width, height; centered at x,y. Odd dimensions
       are best (XXX require?), so that the center is a pixel. *)
    Rect of int * int
  val setshape : body -> shape -> unit
  val getshape : body -> shape

  (* TODO:
     - whether the body interacts with other objects, solids
     - for circles, angle, and angular momentum?
     - attachment to walls and floors (for when they move)
       (maybe generalizes "stick velocity", where the object
        stays touching the wall unless the normal velocity
        exceeds a certain amount)
     *)

  (* A body is only simulated if it's currently in the scene.
     Exception upon adding a body that's already in the scene,
     or removing one that's not. *)
  val bodies : unit -> body list
  val addbody : body -> unit
  val delbody : body -> unit

  (* Get the locus, which is the x,y position that determines
     the current geometry of the world. This can depend on the
     state of bodies, particularly since the locus is normally
     tied to the player.

     This function is called *during* physics loops (e.g.,
     while moving the player along her velocity vector and
     looking for a collision with screen objects) so it should
     consult the current versions of bodies on each call. *)
  val getlocus : unit -> int * int
  val setlocus : (unit -> int * int) -> unit

  (* Move a set of physics bodies.

     For each object:
      - Update its x,y coordinates according to its
        current velocity.
      - Prevent it from entering any solids, including other
        objects.
      - Apply forces to the object, accelerating it.

        *)
  val movebodies : Screen.screen -> unit

  (* Debugging.

     XXX remove this; just use the getdebug call below.

     These are properties from the last call to movebodies. Note that
     in the case of ontheground, the body is moved after this is calculated,
     so it may not reflect the "current" state of the body. *)
  val getontheground : body -> bool

  datatype debug_contact =
    DB_BLOCKED | DB_EJECT | DB_AIR
  val getdebug : Screen.screen -> body ->
    { ontheground : bool,
      contacts : unit -> { l : debug_contact,
                           r : debug_contact,
                           d : debug_contact },
      (* hit (lx, ly) (x, y)
         If the locus is a lx,ly, is the pixel x,y filled? *)
      hit : (int * int) -> (int * int) -> bool
      }

end