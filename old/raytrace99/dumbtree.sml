
(* $Id: dumbtree.sml,v 1.1 2003/05/23 13:16:52 tom7 Exp $ *)

functor Dumbfun (Prim : PRIMITIVE) :> OCTREE (* where type prim = Prim.prim *) =
struct

    val rayeps = 0.00001 (* ? *)


    type prim = Prim.prim

    structure Primitive = Prim

    exception Unimplemented and Impossible


    open Point
        
    type isect = prim * real
        
    type octree = prim list

    val new = #1 : (prim list * int * int) -> octree

	fun tostring _ = ""

    fun intersect pl r = 
	foldr (fn (pr, cc as (SOME c)) => (case Prim.intersect r pr of
					       NONE => cc 
					     | (SOME(p,t)) =>  
						   if (t < (#2 c)) then 
						       SOME(p,t)
						   else cc)  
      | (pr, NONE) => Prim.intersect r pr) (NONE : isect option) pl 

end