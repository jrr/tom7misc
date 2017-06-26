
(* $Id: octree-sig.sml,v 1.1 2003/05/23 13:16:52 tom7 Exp $ *)

signature OCTREE =
sig

    type octree

    structure Primitive : PRIMITIVE

    type prim = Primitive.prim

    type p3 = {x : real,
	       y : real,
	       z : real}
	
    type bounds = {x : real * real,
		   y : real * real,
		   z : real * real}
	
    (*     origin * dir *)
    (* invt : dir is normalized *)
    type ray = p3 * p3
	

    type isect = prim * real

    (*            maxobjspernode,  depth *)
    val new : prim list * int * int -> octree

    val intersect : octree -> ray -> isect option

    val tostring : octree -> string


end