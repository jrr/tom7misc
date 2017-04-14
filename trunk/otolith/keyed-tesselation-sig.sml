
(* A tesselation is a collection of non-overlapping triangles. Each
   vertex can have multiple coordinates, one for each "key" (functor
   argument).

   We use this data structure to implement both the "areas" and
   "objects".

   Each screen has a single "areas" tesselation that covers the play
   area. It defines the gradients along which the objects move.
   Gradients are given with barycentric interpolation between each
   triangle's vertices. The areas are static, so there is just one
   set of coordinates. The key is just (), and some of the generality
   of this functor becomes degenerate and a little confusing.

   A screen can have many objects; these are the solids that the
   player sees and interacts with. An object's vertices have multiple
   coordinates; they are keyed to the vertices of the areas
   tesselation. This allows the object's shape/location to be
   parameterized.

   This code doesn't compute the interpolations; it simply stores
   the tesselations as a data structure and provides some functionality
   for editing it. Screen instantiates the functor twice to create
   the areas and objects, and understands how they interact. *)
signature KEYARG =
sig
  type key
  val compare : key * key -> order
  (* For debugging. Serialization takes these as arguments,
     since they may depend on some context. *)
  val tostring : key -> string
  (* Exception raised by internal functions. *)
  val exn : string -> exn
end

(* functor KeyedTesselation(KEYARG) :> *)
signature KEYEDTESSELATION =
sig

  (* From argument *)
  type key

  (* Mutable, nonempty.

     Uses integer coordinates. *)
  type keyedtesselation

  (* A vertex, usually in multiple triangles. *)
  type node

  (* Three nodes. Never degenerate. *)
  type triangle

  (* Edge maps. (a, b) is considered equal to (b, a).
     There is no requirement that the nodes actually be connected
     in the tesselation, though all nodes in an EM.map must be from
     the same tesselation. Order is arbitrary but consistent. *)
  structure EM : ORD_MAP where type Key.ord_key = node * node

  (* Creates an object of the given rectangle, which looks like this:

     .---.
     |\  |
     | \ |
     |__\|

     It starts with just a single position, for the key argument. *)
  val rectangle : key -> { x0 : int, y0 : int, x1 : int, y1 : int } ->
                  keyedtesselation

  (* Get the triangle this point is within *)
  val gettriangle : keyedtesselation -> int * int -> (key * triangle) option

  (* When using the given key, return the closest edge to the given
     point, and the point on that edge that's closest to the input
     point. Note that this may not be unique, so chooses arbitrarily
     in that case. This is always the same edge and point used by
     splitedge, when it can succeed. *)
  val closestedge : keyedtesselation -> key -> int * int ->
                    node * node * int * int

  (* Split the edge closest to the given point (when using the given
     key), at its closest point. The edge will be on 1 or 2 triangles;
     each of those triangles is split by making a new edge from the
     created split point to the one node that is not on the edge.

     Only succeeds if this point is not one of the edge's vertices,
     and if no degenerate triangles will result. Additionally, the
     same split must work (and produce no degenerate triangles) in all
     configurations for the keyedtesselation. Your best bet is to do
     all the splitting for the tesselation with a single configuration
     before creating others. *)
  val splitedge : keyedtesselation -> key -> int * int -> node option

  (* When using the given key, find the closest internal edge to the
     given point which can be flipped. Flipping is possible only for
     internal edges, which are shared by two triangles. Flipping
     changes the edge to be between the unconnected nodes as shown,
     resulting in two different triangles. This is allowed:

          n1                           n1
           .---. n3                     .---. n3
           |\  |                        |  /|
           | \ |          --->          | / |
        n4 |__\|                     n4 |/__|
               n2                           n2

     But for example not this one:

                    n3
                   .
                 ,`/
               ,` /
             ,`  /
           ,`   /
      n1  `----+ n2
          \    |
           \   |
            \  |
             \ |
              \|
              n4

     since a new edge from n3 to n4 would leave us with two overlapping
     triangles (or if n3 and n4 are colinear, a degenerate triangle).

     When it can succeed, returned are the original edge, the point on
     it that's closest to the input point, and the potential new edge.
     No changes are made. This is the same edge used by flipedge
     below, when it can succeed. *)
  val closestflipedge : keyedtesselation -> key -> int * int ->
                        ((node * node) * (int * int) * (node * node)) option

  (* Actually flip the edge. Returns true if successful.
     This only fails in cases where there is no closest interior edge (like
     for a single triangle) or flipping the edge would result in a disallowed
     triangle (because it is degenerate or too small). *)
  val flipedge : keyedtesselation -> key -> int * int -> bool

  val triangles : keyedtesselation -> triangle list
  val nodes : keyedtesselation -> node list

  (* getnodewithin kt key (x, y) radius
     Get the closest node when the tesselation is configured to this key,
     as long as it is within the radius. *)
  val getnodewithin : keyedtesselation -> key -> int * int -> int ->
                      node option

  (* closestnodesatisfying kt key (x, y) radius pred
     As getnodewithin, but for the closest node satisfying the predicate. *)
  val closestnodesatisfying : keyedtesselation -> key -> int * int -> int ->
                              (node -> bool) -> node option

  (* Get all of the keys. *)
  val keys : keyedtesselation -> key list

  val iskey : keyedtesselation -> key -> bool

  (* addkey kt newcoords k
     Add a key to the configuration of every node.
     The function newcoords is used to generate the (x, y) coordinates
     for each node.
     Raises an exception if the key is already present. *)
  val addkey : keyedtesselation -> (node -> int * int) -> key -> unit

  (* Raises exception if the key is not a current key. *)
  val deletekey : keyedtesselation -> key -> unit

  (* Try moving the node in the configuration given by the key.
     This moves the node in each attached triangle. The point may
     not move to the desired spot, since the triangles are not
     allowed to be degenerate. *)
  val trymovenode : keyedtesselation -> node -> key -> int * int -> int * int
  val canmovenode : keyedtesselation -> node -> key -> int * int -> bool

  (* Try to delete the node. Doesn't allow deletion of nodes that
     cause degenerate tesselations (XXX checks here are incomplete).

     If successful, this node, and any other nodes that were part of
     triangles that got deleted, become invalid. You may need to clean
     up references to them (in the case of deleting an "areas" node
     with linked objects, specifically). The list of now-invalid nodes
     is returned; this is nil if deletion of the node was unsuccessful. *)
  val trydeletenode : keyedtesselation -> node -> node list
  (* Same, but doesn't actually delete. *)
  val candeletenode : keyedtesselation -> node -> bool

  (* cansnapwithin kt k node px
     Find a node within px pixels of 'node' (at the key k) that
     can be snapped to the node (for all keys). *)
  val cansnapwithin : keyedtesselation -> key -> node -> int -> node option

  (* snap kt node1 node2
     Snap node to node2, making them the same node. Returns the deleted
     node so that references to it can be cleaned up. The snapping
     must be legal (e.g. not creating degenerate triangles), so this
     should be the result of cansnapwithin. *)
  val snap : keyedtesselation -> node -> node -> node list

  (* Convert to the serialization format. These are somewhat fancy
     because the key may be nontrivial. For example, when deserializing
     an object keyed by some other tesselation's nodes, the string to
     key function looks up the node ID in a map of already-deserialized
     nodes to find it.

     Since toworld is allowed to renumber nodes, it returns a mapping
     from node to the key used in this serialization. *)
  val totf : (key -> string) -> keyedtesselation ->
             WorldTF.keyedtesselation * (node -> int)
  val fromtf : (string -> key option) -> WorldTF.keyedtesselation ->
               keyedtesselation * (int -> node)

  (* Checks the structure of the keyedtesselation. This should never be
     necessary, but is useful if you suspect a bug. *)
 (*   val check : keyedtesselation -> unit *)

  structure T :
  sig
    val nodes : triangle -> node * node * node
    (* Always length three *)
    val nodelist : triangle -> node list
  end

  structure N :
  sig
    (* Raises KeyedTesselation if the key is not present *)
    val coords : node -> key -> int * int
    val coordsmaybe : node -> key -> (int * int) option

    (* A node is usually part of multiple triangles. *)
    val triangles : node -> triangle list

    (* If a node has been removed (e.g. deleted, or snapped to
       another node), then handles to it become invalid. *)
    val is_valid : node -> bool

    (* Get the unique ID of the node. -- XXX trying not to
       leak this abstraction. *)
    (* val id : node -> IntInf.int *)

    val compare : node * node -> order
    val eq : node * node -> bool
  end

end
