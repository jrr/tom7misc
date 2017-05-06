
structure Otolith =
struct

  exception Quit
  open Constants

  fun eprint s = TextIO.output(TextIO.stdErr, s ^ "\n")

  val fillscreen = _import "FillScreenFrom" private :
      Word32.word Array.array -> unit ;

  val ctr = ref 0
  val pixels = Array.array(WIDTH * HEIGHT, 0wxFFAAAAAA : Word32.word)
  val rc = ARCFOUR.initstring "anything"

  structure Areas = Screen.Areas
  structure Obj = Screen.Obj


  val worldx = ref 4
  val worldy = ref 4

  (* If this fails, we start with an empty everything. *)
  val () = World.load ()

  (* Current screen at worldx,worldy. *)
  (* This ref is just for the benefit of the editor; modify the screen
     itself to make changes to the world. *)
  val screen : Screen.screen ref =
    ref (World.getorcreate (!worldx, !worldy))

  datatype mode = Editing | Playing
  val mode = ref Editing : mode ref

  (* Always in game pixels. The event loop scales down x,y before
     calling any of these functions. *)
  val mousex = ref 0
  val mousey = ref 0

  val player = Physics.newbody ()
  (* Physical size of player (not player graphic) *)
  val PLAYERW = 5
  val PLAYERH = 11

  val () = Physics.setshape player (Physics.Rect (PLAYERW, PLAYERH))
  val () = Physics.setxy player (80, 80)
  val () = Physics.addbody player
  val () = Physics.setlocus (fn () => Physics.getxy player)

  (* XXX: Should probably be a mask on all keys. *)
  val holdingshift = ref false
  val holdingcontrol = ref false
  val holdingspace = ref false
  val holdingbackslash = ref false
  val holdingtab = ref false
  val holdingm = ref false
  val holdingv = ref false

  val holdingleft = ref false
  val holdingright = ref false

  val mousedown = ref false

  datatype anynode =
      AreasNode of Areas.node
    | ObjNode of Screen.obj * Obj.node

  val draggingnode = ref NONE : anynode option ref
  val frozennode = ref NONE : Areas.node option ref

  fun warptoscreen (x, y) =
    let in
      draggingnode := NONE;
      frozennode := NONE;
      worldx := x;
      worldy := y;
      screen := World.getorcreate (x, y)
    end

  (* XXX should probably make it more configurable than this,
     like allow dragging a box or entering some kind of triangle
     strip clicking thing... *)
  fun addobject node =
    let
      val (x0, y0) = (Int.min(!mousex, WIDTH - 25),
                      Int.min(!mousey, WIDTH - 25))
      val (x1, y1) = (x0 + 20, y0 + 20)

      val newscreen = Screen.addrectangle (!screen) node (x0, y0, x1, y1)
    in
      eprint "Add object..";
      screen := newscreen;
      World.setscreen (!worldx, !worldy, newscreen)
    end

  val DRAG_DISTANCE = 5
  val SPLIT_DISTANCE = 5

  (* Link an object to the frozen node. Adds this node as a key,
     setting the positions "smartly" (right now they are just copied
     from the first key). *)
  fun linkobject (node : Areas.node) =
    case Screen.objectscontaining (Screen.objs (!screen))
                                  (!mousex, !mousey) of
      nil => eprint "press when mouse is near an object to link."
    | (obj : Screen.obj, key) :: _ =>
        if Obj.iskey obj node
        then eprint "already linked to this object"
        else
          let
            fun newcoords (onode : Obj.node) =
              let val (x, y) = Obj.N.coords onode key
              in (x, y)
              end
          in
            Obj.addkey obj newcoords (node : Obj.key)
          end

  val MOUSECIRCLE = Draw.mixcolor (0wxFF, 0wxAA, 0wx33, 0wxFF)
  val CLOSESTCIRCLE = Draw.mixcolor (0wx44, 0wx44, 0wx44, 0wxFF)
  val DELCIRCLE = Draw.mixcolor (0wxFF, 0wx00, 0wx00, 0wxFF)
  val SNAPCOLOR = Draw.mixcolor (0wx77, 0wx00, 0wx90, 0wxFF)

  val DRAGGING = Draw.mixcolor (0wxFF, 0wxFF, 0wx00, 0wxFF)
  val FROZEN = Draw.mixcolor (0wxFF, 0wxFF, 0wxFF, 0wxFF)

  val ACTIONTEXT = Draw.mixcolor (0wx44, 0wx66, 0wx22, 0wxFF)
  val ACTIONTEXTHI = Draw.mixcolor (0wx55, 0wxAA, 0wx22, 0wxFF)
  val ACTIONTEXTDEL = Draw.mixcolor (0wxAA, 0wx00, 0wx00, 0wxFF)
  val FLIPOLDLINE = Draw.mixcolor (0wx11, 0wx22, 0wx77, 0wxFF)
  val FLIPNEWLINE = Draw.mixcolor (0wx22, 0wx55, 0wxAA, 0wxFF)

  fun closestedgewithin n =
    let
      val nsq = n * n
      val (n1, n2, x, y) =
        Areas.closestedge (Screen.areas (!screen)) () (!mousex, !mousey)
    in
      if IntMaths.distance_squared ((!mousex, !mousey), (x, y)) <= nsq
      then SOME (n1, n2, x, y)
      else NONE
    end

  fun closestobjectedgewithin (x, y) key dist =
    case Screen.closestobjectedge (Screen.objs (!screen)) key (x, y) of
      NONE => NONE
    | SOME (obj, n1, n2, nx, ny) =>
      if IntMaths.distance_squared ((x, y), (nx, ny)) <= (dist * dist)
      then SOME (obj, n1, n2, nx, ny)
      else NONE

  datatype decoration =
    (* x, y, radius, color *)
      Circle of int * int * int * Word32.word
    (* color, x, y, text *)
    | Text of Word32.word * int * int * string
    | Line of Word32.word * (int * int) * (int * int)

  fun drawdecoration (Circle (x, y, r, c)) =
    Draw.drawcircle (pixels, x, y, r, c)
    | drawdecoration (Text (c, x, y, s)) =
    Draw.drawtextcolor (pixels, Font.pxfont, c, x, y, s)
    | drawdecoration (Line (c, (x0, y0), (x1, y1))) =
    Draw.drawline (pixels, x0, y0, x1, y1, c)

  (* Get left-mouse actions for when the button isn't currently held. *)
  fun get_lmb_actions (x, y) =
    (* If we're holding control, then the only thing we can do is
       freeze/unfreeze areas nodes. *)
    if !holdingcontrol
    then
        let
          fun disequal (old, new) =
            let
              val ot = case old of
                SOME n => let val (x, y) = Areas.N.coords n ()
                          in [Text (ACTIONTEXTHI, x - 18, y - 14, "Unfreeze")]
                          end
              | NONE => []
              val nt = case new of
                SOME n => let val (x, y) = Areas.N.coords n ()
                          in [Text (ACTIONTEXTHI, x - 18, y - 11, "Freeze")]
                          end
              | NONE => []
            in
              (ot @ nt, (fn () => frozennode := new))
            end
        in
          case (!frozennode,
                Areas.getnodewithin
                (Screen.areas (!screen)) () (x, y) 5) of
              (SOME n, SOME nn) => if Areas.N.eq (n, nn)
                                   then ([], ignore)
                                   else disequal (SOME n, SOME nn)
            | (NONE, NONE) => ([], ignore)
            | (old, new) => disequal (old, new)
        end

      else if !holdingshift
      then
        case !frozennode of
          SOME key =>
            (case (Screen.objectclosestnodewithin
                   (Screen.objs (!screen)) key (x, y) 5,
                   closestobjectedgewithin (x, y) key SPLIT_DISTANCE) of
               (NONE, SOME (obj, _, _, x, y)) =>
                 let
                   fun split () =
                     case Obj.splitedge obj key (x, y) of
                       NONE => ()
                     | SOME n =>
                         let in
                           draggingnode := SOME (ObjNode (obj, n));
                           ignore (Obj.trymovenode obj n key (x, y))
                         end
                 in
                   ([Circle (x, y, 3, CLOSESTCIRCLE),
                     Text (ACTIONTEXTHI, x - 6, y - 11, "add")],
                    split)
                 end
          | (SOME (obj, delnode), _) =>
              let
                val (x, y) = Obj.N.coords delnode key
                fun del () =
                  ignore (Obj.trydeletenode obj delnode)
              in
                (* XXX would be good to highlight this same node in
                   each of its other occurrences? *)
                ([Circle (x, y, 3, DELCIRCLE),
                  Text (ACTIONTEXTDEL, x - 6, y - 11, "del")],
                 del)
              end

          | _ => ([], ignore))

        | NONE =>
          case (Areas.getnodewithin (Screen.areas (!screen)) ()
                (!mousex, !mousey) DRAG_DISTANCE,
                closestedgewithin SPLIT_DISTANCE) of
            (NONE, SOME (_, _, x, y)) =>
              let
                fun split () =
                  case Areas.splitedge (Screen.areas (!screen)) () (x, y) of
                    NONE => ()
                  | SOME n =>
                      let in
                        draggingnode := SOME (AreasNode n);
                        ignore (Areas.trymovenode
                                (Screen.areas (!screen)) n () (x, y))
                      end
              in
                ([Circle (x, y, 3, CLOSESTCIRCLE),
                  Text (ACTIONTEXTHI, x - 6, y - 11, "add")],
                 split)
              end
          | (SOME delnode, _) =>
              let
                val (x, y) = Areas.N.coords delnode ()
                fun del () =
                  let
                    val deleted =
                      Areas.trydeletenode (Screen.areas (!screen)) delnode
                    fun cleanup node =
                      screen := Screen.removelinks (!screen) node
                  in
                    app cleanup deleted
                  end
              in
                (* XXX test with candeletenode *)
                ([Circle (x, y, 3, DELCIRCLE),
                  Text (ACTIONTEXTDEL, x - 6, y - 11, "del")],
                 del)
              end
          | _ => ([], ignore)

      else if !holdingbackslash
      then
        (* Allows flipping a tesselation edge. *)
        case !frozennode of
          SOME key =>
            (case Screen.closestobjectedge (Screen.objs (!screen)) key (x, y) of
               NONE => ([], ignore)
             | SOME (obj, _, _, _, _) =>
               (* Here we just used closestobjectedge to get an object
                  to query for a flip edge. Would be a little better
                  if we found e.g. the closest edge that allows flipping.
                  But this still allows flipping any edge by just making
                  sure it's the one closest to the mouse. *)
               (case Obj.closestflipedge obj key (x, y) of
                  NONE => ([], ignore)
                | SOME ((n1, n2), (cx, cy), (n3, n4)) =>
                  let
                    fun flip () =
                      if Obj.flipedge obj key (x, y)
                      then ()
                      else eprint "Failed to flip"

                    (* Draw the flip lines for every key. *)
                    fun getedge k =
                      let
                        val (x1, y1) = Obj.N.coords n1 k
                        val (x2, y2) = Obj.N.coords n2 k
                        val (x3, y3) = Obj.N.coords n3 k
                        val (x4, y4) = Obj.N.coords n4 k
                      in
                        [Line (FLIPOLDLINE, (x1, y1), (x2, y2)),
                         Line (FLIPNEWLINE, (x3, y3), (x4, y4))]
                      end

                    val keys = Obj.keys obj
                    val lines = List.concat (map getedge keys)
                  in
                    (Text (ACTIONTEXTHI, x - 13, y - 11, "flip") :: lines,
                     flip)
                  end))
        | NONE =>
            (case Areas.closestflipedge (Screen.areas (!screen)) () (x, y) of
               NONE => ([], ignore)
             | SOME ((n1, n2), (cx, cy), (n3, n4)) =>
               let
                 val (x1, y1) = Areas.N.coords n1 ()
                 val (x2, y2) = Areas.N.coords n2 ()
                 val (x3, y3) = Areas.N.coords n3 ()
                 val (x4, y4) = Areas.N.coords n4 ()

                 fun flip () =
                   if Areas.flipedge (Screen.areas (!screen)) () (x, y)
                   then ()
                   else eprint "Failed to flip"
               in
                 ([Text (ACTIONTEXTHI, x - 13, y - 11, "flip"),
                   Line (FLIPOLDLINE, (x1, y1), (x2, y2)),
                   Line (FLIPNEWLINE, (x3, y3), (x4, y4))],
                  flip)
               end)

      else
        (* No modifiers. *)

        (* We drag object nodes if an areas node is frozen. Otherwise we
           drag areas nodes. *)
        case !frozennode of
          SOME key =>
            (case Screen.objectclosestnodewithin
                  (Screen.objs (!screen)) key (x, y) 5 of
               NONE => ([], ignore)
             | SOME (obj, node) =>
                 ([Text (ACTIONTEXTHI, x - 13, y - 11, "drag")],
                  fn () => draggingnode := SOME (ObjNode (obj, node))))

        | NONE =>
            (case Areas.getnodewithin (Screen.areas (!screen)) () (x, y) 5 of
               NONE => ([], ignore)
             | SOME node => ([Text (ACTIONTEXTHI, x - 13, y - 11, "drag")],
                              fn () => draggingnode := SOME (AreasNode node)))

  fun get_unlmb_actions (x, y) =
    (* First, test if we are currently dragging... *)
    case !draggingnode of
      NONE => ([], ignore)
    | SOME anynode =>
      (* Shift allows snapping to nearby node. *)
        if !holdingshift
        then
          (case (!frozennode, anynode) of
             (SOME key, ObjNode (obj, node)) =>
               (case Obj.cansnapwithin obj key node 5 of
                  NONE => ([], ignore)
                | SOME snapnode =>
                    let
                      val (sx, sy) = Obj.N.coords snapnode key
                      fun snap () =
                        ignore (Obj.snap obj node snapnode)
                    in
                      ([Text (SNAPCOLOR, x - 13, y - 11, "snap"),
                        Circle (sx, sy, 3, SNAPCOLOR)],
                       snap)
                    end)

           | (NODE, AreasNode node) =>
               (case Areas.cansnapwithin (Screen.areas (!screen)) () node 5 of
                  NONE => ([], ignore)
                | SOME snapnode =>
                    let
                      val (sx, sy) = Areas.N.coords snapnode ()
                      fun snap () =
                        let
                          (* XXX maybe we just want to return one?
                             instead of removing links, we could keep
                             the links if only one of the two nodes
                             has them. *)
                          val deleted =
                            Areas.snap (Screen.areas (!screen)) node snapnode
                          fun cleanup node =
                            screen := Screen.removelinks (!screen) node
                        in
                          app cleanup deleted
                        end
                    in
                      ([Text (SNAPCOLOR, x - 13, y - 11, "snap"),
                        Circle (sx, sy, 3, SNAPCOLOR)],
                       snap)
                    end)
           (* otherwise: bad state? *)
           | _ => ([], ignore))
        else ([], ignore)

  val INSIDE = Draw.hexcolor 0wxFFEEFF

  (* Draw the objects as though the player is at (x, y).

     Find what area we're in, from Areas.
     If the object has keys for all its three triangles, then
     compute the interpolated position.

     TODO: Figure out what to do if the object doesn't have keys.
     Currently we just don't draw it, but there are multiple options
     that would make sense... *)
  fun drawinterpolatedobjects (x, y) =
    case Areas.gettriangle (Screen.areas (!screen)) (x, y) of
      NONE => ()
    | SOME ((), triangle) =>
      let
        val (a, b, c) = Areas.T.nodes triangle

        fun marknode n =
          let val (nx, ny) = Areas.N.coords n ()
          in Draw.drawcircle (pixels, nx, ny, 1, INSIDE)
          end

        (* Convert to barycentric coordinates. This basically gives a weight
           for each vertex of the triangle. Since we're inside the triangle,
           these will all be in [0.0, 1.0] and sum to 1.0. *)
        val (la, lb, lc) =
          IntMaths.barycentric (Areas.N.coords a (),
                                Areas.N.coords b (),
                                Areas.N.coords c (),
                                (x, y))

        fun oneobj obj =
          if Obj.iskey obj a andalso
             Obj.iskey obj b andalso
             Obj.iskey obj c
          then
            let

              (* Edges can appear in two triangles. Don't draw them twice. *)
              val drawn : unit Obj.EM.map ref = ref Obj.EM.empty

              (* Get the coordinates for the node by interpolating
                 between the three keys. *)
              fun transform n =
                let
                  (* These shouldn't fail because we checked that the
                     key is a key of the object above. *)
                  val (ax, ay) = Obj.N.coords n a
                  val (bx, by) = Obj.N.coords n b
                  val (cx, cy) = Obj.N.coords n c

                  val nx = Real.round (real ax * la + real bx * lb +
                                       real cx * lc)
                  val ny = Real.round (real ay * la + real by * lb +
                                       real cy * lc)
                in
                  (nx, ny)
                end

              fun drawline (d, e) =
                case Obj.EM.find (!drawn, (d, e)) of
                  SOME () => ()
                | NONE =>
                    let
                      val (x0, y0) = transform d
                      val (x1, y1) = transform e
                    in
                      drawn := Obj.EM.insert (!drawn, (d, e), ());
                      Draw.drawline (pixels, x0, y0, x1, y1, INSIDE)
                    end

              fun drawtriangle t =
                let val (d, e, f) = Obj.T.nodes t
                in
                  drawline (d, e);
                  drawline (e, f);
                  drawline (f, d)
                end

            in
              app drawtriangle (Obj.triangles obj)
            end
          else ()

      in
        marknode a;
        marknode b;
        marknode c;
        app oneobj (Screen.objs (!screen))
      end

  fun draweditorstuff () =
    let
      val (decorations, _) =
        if !mousedown
        then get_unlmb_actions (!mousex, !mousey)
        else get_lmb_actions (!mousex, !mousey)
    in
      if Option.isSome (!draggingnode)
      then Draw.drawcircle (pixels, !mousex, !mousey, 5, MOUSECIRCLE)
      else ();

      app drawdecoration decorations;

      if !holdingv
      then drawinterpolatedobjects (!mousex, !mousey)
      else ();

      (case !frozennode of
         NONE => ()
       | SOME n =>
           let val (nx, ny) = Areas.N.coords n ()
           in
             (* XXX don't always use circle *)
             Draw.drawcircle (pixels, nx, ny, 7, FROZEN)
           end);
      ()
    end

  fun savetodisk () = World.save ()

  fun loadfromdisk () =
    let in
      World.load ();
      screen := World.getorcreate (!worldx, !worldy)
    end

  fun mousemotion (x, y) =
    (* XXX should case on what kinda node it is..? *)
    case !draggingnode of
      NONE => ()
    | SOME (AreasNode n) =>
        ignore (Areas.trymovenode (Screen.areas (!screen)) n () (x, y))
    | SOME (ObjNode (obj, n)) =>
        (case !frozennode of
           NONE => eprint "Can't move object node without frozen node"
         | SOME key => ignore (Obj.trymovenode obj n key (x, y)))

  (* The work is done by get_lmb_actions so that the indicators
     and actions are in sync. Here we just apply the action function. *)
  fun leftmouse (x, y) =
    let val (_, action) = get_lmb_actions (x, y)
    in action ()
    end

  fun leftmouseup (x, y) =
    let val (_, action) = get_unlmb_actions (x, y)
    in
      action ();
      (* Always deselect any node. *)
      draggingnode := NONE
    end

  val start = Time.now()

  fun dirkey (dx, dy) =
    if !holdingtab
    then
      let
        val nx = !worldx + dx
        val ny = !worldy + dy
      in
        if nx >= 0 andalso ny >= 0 andalso
           nx < WORLD_WIDTH andalso ny < WORLD_HEIGHT
        then warptoscreen (nx, ny)
        else eprint "can't go outside the map"
      end
    else ()

  fun keydown SDL.SDLK_ESCAPE =
    let in
      (* For now, always save. XXX reconsider this
         once the world becomes valuable... *)
      savetodisk();
      raise Quit
    end

    | keydown SDL.SDLK_RETURN =
    let in
      (* XXX better if some shared code for this *)
      frozennode := NONE;
      draggingnode := NONE;
      Physics.setxy player (!mousex, !mousey);
      mode := (case !mode of Editing => Playing | Playing => Editing)
    end

    | keydown SDL.SDLK_SPACE =
    let in
      if !mode = Playing
      then Physics.setjumpwish player true
      else ();
      holdingspace := true
    end
    | keydown SDL.SDLK_m = holdingm := true
    | keydown SDL.SDLK_v = holdingv := true
    | keydown SDL.SDLK_LSHIFT = holdingshift := true
    | keydown SDL.SDLK_RSHIFT = holdingshift := true
    | keydown SDL.SDLK_LCTRL = holdingcontrol := true
    | keydown SDL.SDLK_RCTRL = holdingcontrol := true
    | keydown SDL.SDLK_BACKSLASH = holdingbackslash := true
    | keydown SDL.SDLK_TAB = holdingtab := true

    | keydown SDL.SDLK_UP = dirkey (0, ~1)
    | keydown SDL.SDLK_DOWN = dirkey (0, 1)
    | keydown SDL.SDLK_LEFT =
    let in
      holdingleft := true;
      dirkey (~1, 0)
    end
    | keydown SDL.SDLK_RIGHT =
    let in
      holdingright := true;
      dirkey (1, 0)
    end

    (* Need much better keys... *)
    | keydown SDL.SDLK_o =
      (case !frozennode of
         NONE => eprint "Freeze a node with ctrl-click first."
       | SOME node => addobject node)

    | keydown SDL.SDLK_l =
      (case !frozennode of
         NONE => eprint "Freeze a node with ctrl-click first."
       | SOME node => linkobject node)

    | keydown _ = ()

  fun keyup SDL.SDLK_LSHIFT = holdingshift := false
    | keyup SDL.SDLK_RSHIFT = holdingshift := false
    | keyup SDL.SDLK_LCTRL = holdingcontrol := false
    | keyup SDL.SDLK_RCTRL = holdingcontrol := false
    | keyup SDL.SDLK_SPACE =
    let in
      if !mode = Playing
      then Physics.setjumpwish player false
      else ();
      holdingspace := false
    end
    | keyup SDL.SDLK_m = holdingm := false
    | keyup SDL.SDLK_v = holdingv := false
    | keyup SDL.SDLK_BACKSLASH = holdingbackslash := false
    | keyup SDL.SDLK_TAB = holdingtab := false
    | keyup SDL.SDLK_LEFT = holdingleft := false
    | keyup SDL.SDLK_RIGHT = holdingright := false

    | keyup _ = ()

  (* TODO: Joystick *)
  fun events () =
    case SDL.pollevent () of
        NONE => ()
      | SOME evt =>
         case evt of
             SDL.E_Quit => raise Quit
           | SDL.E_KeyDown { sym } => keydown sym
           | SDL.E_KeyUp { sym } => keyup sym
           | SDL.E_MouseMotion { state : SDL.mousestate,
                                 x : int, y : int, ... } =>
               let
                 val x = x div PIXELSCALE
                 val y = y div PIXELSCALE
               in
                 mousex := x;
                 mousey := y;
                 mousemotion (x, y)
               end
           | SDL.E_MouseDown { button = 1, x, y, ... } =>
               let
                 val x = x div PIXELSCALE
                 val y = y div PIXELSCALE
               in
                 mousedown := true;
                 leftmouse (x, y)
               end
           | SDL.E_MouseUp { button = 1, x, y, ... } =>
               let
                 val x = x div PIXELSCALE
                 val y = y div PIXELSCALE
               in
                 mousedown := false;
                 leftmouseup (x, y)
               end
           | SDL.E_MouseDown { button = 4, ... } =>
               let in
                 eprint "scroll up"
               end

           | SDL.E_MouseDown { button = 5, ... } =>
               let in
                 eprint "scroll down"
               end
           | _ => ()

  val TESSELATIONLINES = Draw.mixcolor (0wx44, 0wx44, 0wx55, 0wxFF)
  val TESSELATIONSEGMENT = Vector.fromList [TESSELATIONLINES, 0w0]

  val TESSELATIONLINESLOW = Draw.mixcolor (0wx2A, 0wx2A, 0wx3B, 0wxFF)
  val TESSELATIONSEGMENTLOW = Vector.fromList [TESSELATIONLINESLOW, 0w0, 0w0]

  fun loop () =
    let
      val () = events ()

      val () = Draw.randomize_loud pixels
      val () = Render.drawareacolors (pixels, Screen.areas (!screen))

      (* val () = eprint "areas" *)
      val () = Render.drawareas (pixels, Screen.areas (!screen),
                                 case !frozennode of
                                   NONE => TESSELATIONSEGMENT
                                 | SOME _ => TESSELATIONSEGMENTLOW)

      val drawobjects =
        case !mode of
          Playing =>
            let in
              Draw.darken pixels;
              false
            end
        | Editing => true

      val drawobjects = true
      (* Don't draw objects if we're drawing the interpolated ones. *)
      val drawobjects = if !holdingv
                        then false
                        else drawobjects

      (* Don't draw objects if we're drawing the mask. *)
      val drawobjects = if !holdingm
                        then false
                        else drawobjects

      (* Don't draw objects if we're flipping edges in the areas tesselation. *)
      val drawobjects = if !holdingbackslash andalso
                           not (Option.isSome (!frozennode))
                        then false
                        else drawobjects

      val () =
        if drawobjects
        then Render.drawobjects (pixels, !screen, !frozennode)
        else ()

      val () =
        case !mode of
          Playing => drawinterpolatedobjects (Physics.getlocus ())
        | _ => ()

      val () =
        case !mode of
          Editing => draweditorstuff ()
        | _ => ()

      val () =
        if !holdingm
        then Render.drawmask (pixels, !screen)
        else ()

      (* Show map? *)
      val () =
        if !holdingtab
        then
          let in
            Draw.darken pixels;
            Render.drawmap (pixels, !worldx, !worldy)
          end
        else ()

      val () =
        if !mode = Playing
        then
          case (!holdingleft, !holdingright) of
            (* If holding both, don't change wish.
               Actually, it probably feels more responsive
               if we switch to whatever the most recent
               keypress was. *)
            (true, true) => ()
          | (true, false) => Physics.setlrwish player (SOME Physics.Left)
          | (false, true) => Physics.setlrwish player (SOME Physics.Right)
          | (false, false) => Physics.setlrwish player NONE
        else ()

      val () =
        case !mode of
          Playing =>
            let
              val (playerx, playery) = Physics.getxy player
              val Physics.Rect (playerw, playerh) = Physics.getshape player
            in
              (* XXX needs to take into account facing direction (which
                 may need to be external), velocity, lrwish, jumpwish,
                 etc. *)
              (*
              Draw.blit { dest = (WIDTH, HEIGHT, pixels),
                          src = Images.person,
                          srcrect = NONE,
                          (* Offsets from center pixel. *)
                          dstx = playerx - 5,
                          dsty = playery - 7 }
              *)
              Draw.drawrect (pixels,
                             playerx - playerw div 2,
                             playery - playerh div 2,
                             playerx + playerw div 2,
                             playery + playerh div 2,
                             SNAPCOLOR)
            end
        | Editing =>
            (* draw mouse. Should probably take mode into account *)
            Draw.blit { dest = (WIDTH, HEIGHT, pixels),
                        src = Images.tiniestmouse,
                        srcrect = NONE,
                        dstx = !mousex,
                        dsty = !mousey }

      (* Update physics. *)
      val drawobjects =
        case !mode of
          Playing => Physics.movebodies (!screen)
        | Editing => ()

      val () =
        case !mode of
          Playing =>
            let val (dx, dy) = Physics.getdxy player
            in Draw.drawtextcolor (pixels, Font.pxfont,
                                   SNAPCOLOR, 8, 8,
                                   Int.toString dx ^ "," ^
                                   Int.toString dy)
            end
        | _ => ()
      (* val () = Draw.noise_postfilter pixels *)
      (* val () = Draw.scanline_postfilter pixels *)
      (* val () = Draw.mixpixel_postfilter 0.25 0.8 pixels *)
      val () = fillscreen pixels
      val () = ctr := !ctr + 1
    in
      loop ()
    end

  val () = SDL.show_cursor false
  val () = loop ()
      handle Quit => ()
           | e =>
          let in
            eprint "\n\nUnhandled exception:\n";
            (case e of
               Screen.Screen s => eprint ("Screen: " ^ s)
             | World.World s => eprint ("World: " ^ s)
             | Constants.Impossible s => eprint ("Impossible: " ^ s)
             | Physics.Physics s => eprint ("Physics: " ^ s)
             | _ => ());

            app eprint (MLton.Exn.history e)
          end
end