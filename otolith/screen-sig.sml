(* A screen consists of both the areas and the objects that are linked
   to the areas' vertices. *)
signature SCREEN =
sig

  exception Screen of string

  type screen

  structure Areas : KEYEDTESSELATION where type key = unit
  type areas = Areas.keyedtesselation

  structure Obj : KEYEDTESSELATION where type key = Areas.node
  type obj = Obj.keyedtesselation

  (* Basic starting screen for editing. *)
  val starter : unit -> screen

  val fromtf : WorldTF.screen -> screen
  val totf : screen -> WorldTF.screen

  val objs : screen -> obj list
  val areas : screen -> areas

  (* addrectangle screen node (x0, y0, x1, y1) *)
  val addrectangle : screen -> Areas.node -> int * int * int * int -> screen

  (* objectscontaining screen (x, y)

     Find all the objects that contain the point (x, y) at at least one
     of their keys.
     Gives a key for each that causes the point to be inside it. *)
  val objectscontaining : obj list -> int * int -> (obj * Obj.key) list
  (* XXX docs *)
  val closestobjectedge : obj list -> Areas.node -> int * int ->
    (obj * Obj.node * Obj.node * int * int) option

  (* Return the closest node (and object) within the given distance from
     (x, y), using the configuration key. In the case of ties, the first
     object in the list is returned; the order among nodes that are the
     same distance is arbitrary, however. *)
  val objectclosestnodewithin : obj list -> Obj.key -> int * int -> int ->
    (obj * Obj.node) option

end