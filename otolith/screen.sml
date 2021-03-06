structure Screen :> SCREEN =
struct

  open Constants

  exception Screen of string

  (* TODO: Something that checks that for every possible pixel,
     none of the objects have "bad" triangles (overlapping,
     degenerate, etc.)? This should be true at the vertices of
     the areas because the editor only lets you create good
     tesselations, but when interpolating this might be
     violated. *)

  (* State of a single screen in the game.
     Not worrying about how screens are connected together
     for this experiment.

     We have a single tesselation which should cover the whole screen
     area. Call this the "areas". Then we have a list of objects,
     themselves tesselations, that are keyed by the nodes in the
     areas.

     *)

  structure AreaArg =
  struct
    type key = unit
    fun compare _ = EQUAL
    fun tostring () = ""
    fun exn s = Screen ("areas: " ^ s)
  end

  structure Areas = KeyedTesselation(AreaArg)
  type areas = Areas.keyedtesselation

  structure ObjArg =
  struct
    type key = Areas.node
    val compare = Areas.N.compare
    (* Perhaps could allow debugging output of nodes? *)
    fun tostring n = "(NODE)"
    fun exn s = Screen ("obj: " ^ s)
  end

  structure Obj = KeyedTesselation(ObjArg)
  type obj = Obj.keyedtesselation

  (* XXX: Does there only need to be one screen tesselation? Why not
     just give every object its own tesselation? Not clear that one
     is easier to think about than another... *)
  datatype screen = S of { areas : areas,
                           objs : obj list }

  fun areas (S { areas, objs }) = areas
  fun objs (S { areas, objs }) = objs

  fun starter () = S { areas =
                         Areas.rectangle ()
                         { x0 = 0, y0 = 0,
                           x1 = WIDTH - 1, y1 = HEIGHT - 1 },
                       objs = nil }

  fun objectscontaining (objs : obj list) (x, y) =
    List.mapPartial
    (fn obj =>
     case Obj.gettriangle obj (x, y) of
       NONE => NONE
     | SOME (k, _) => SOME (obj, k))
    objs

  fun objectclosestnodewithin (objs : obj list) (key : Obj.key) (x, y) dist =
    let
      val closest = ref NONE
      fun closer d =
        case !closest of
          NONE => true
        | SOME (dd, _, _) => d < dd

      fun oneobject obj =
        if Obj.iskey obj key
        then
          case Obj.getnodewithin obj key (x, y) dist of
            NONE => ()
          | SOME n =>
              let
                val (xx, yy) = Obj.N.coords n key
                val d = IntMaths.distance_squared ((x, y), (xx, yy))
              in
                if closer d
                then closest := SOME (d, obj, n)
                else ()
              end
        else ()
    in
      app oneobject objs;
      Option.map (fn (_, b, c) => (b, c)) (!closest)
    end

  fun closestobjectedge (objs : obj list) key (x, y) =
    let
      (* I need to do this kind of thing a lot -- can probably be a
         nice little utility *)
      val closest = ref NONE
      fun closer d =
        case !closest of
          NONE => true
        | SOME (dd, _, _, _, _, _) => d < dd

      fun oneobject obj =
        if Obj.iskey obj key
        then
          let
            val (n1, n2, xx, yy) = Obj.closestedge obj key (x, y)
            val dsq = IntMaths.distance_squared ((x, y), (xx, yy))
          in
            if closer dsq
            then closest := SOME (dsq, obj, n1, n2, xx, yy)
            else ()
          end
        else ()
    in
      app oneobject objs;
      Option.map (fn (_, obj, n1, n2, xx, yy) =>
                  (obj, n1, n2, xx, yy)) (!closest)
    end

  (* XXX it is weird that this has to return a new screen...
     maybe screen should just be mutable at toplevel? *)
  fun addrectangle (S { areas, objs }) node (x0, y0, x1, y1) : screen =
    let
      val obj = Obj.rectangle node { x0 = x0, y0 = y0, x1 = x1, y1 = y1 }
    in
      (* Attach to other nodes? *)
      S { areas = areas, objs = obj :: objs }
    end

  fun removelinks (S { areas, objs }) node =
    let
      fun oneobj (obj : obj) =
        if Obj.iskey obj node
        then
          let in
            Obj.deletekey obj node;
            (* If we deleted the last key, also delete the object. *)
            case Obj.keys obj of
              nil => NONE
            | _ => SOME obj
          end
        else SOME obj
    in
      S { areas = areas, objs = List.mapPartial oneobj objs }
    end

  (* Most of the work is done by KeyedTesselation itself.
     But we need to set up a mapping between area nodes and
     stringified integers, since those are used as keys for
     coordinates in the objs. *)
  fun totf (S { areas, objs }) : WorldTF.screen =
    let
      val (areas, getid) = Areas.totf (fn () => "") areas
      fun serializekey n = Int.toString (getid n)

      fun oneobj obj =
          let val (kt, _) = Obj.totf serializekey obj
          in kt
          end
    in
      WorldTF.S { areas = areas,
                  objs = map oneobj objs }
    end

  fun fromtf (WorldTF.S { areas, objs }) : screen =
    let
      fun checkkey "" = SOME ()
        | checkkey _ = NONE
      val (areas, getnode) = Areas.fromtf checkkey areas

      fun deserializekey s =
          Option.map getnode (Int.fromString s)

      fun oneobj obj =
        let val (kt, _) = Obj.fromtf deserializekey obj
        in kt
        end
    in
      S { areas = areas,
          objs = map oneobj objs }
    end


end
