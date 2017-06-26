
signature POINT =
sig

    (* point or vector *)

    type p3 = {x : real,
	       y : real,
	       z : real}

    type bounds = {x : real * real,
		   y : real * real,
		   z : real * real}

    (* plane is pointinplane * unit-normal-vector *)
    type plane = p3 * p3

    (* ray is originpoint * unit-direction-vector *)
    type ray = p3 * p3
	
    val within : (p3 * bounds) -> bool

    val p3add   : (p3 * p3) -> p3
    val p3sub   : (p3 * p3) -> p3
    val p3scale : (p3 * real) -> p3

    val p3cross : (p3 * p3) -> p3
    val p3dot   : (p3 * p3) -> real

    val norm : p3 -> p3

    val boundstostring : bounds -> string
    val pointtostring  : p3     -> string
    val raytostring    : ray    -> string
	
    (* returns a scalar for the ray st it meets the plane,
       or NONE if it misses *)
    val rayplaneisect  : ray -> plane -> real option

end
