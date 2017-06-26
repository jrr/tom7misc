(* $Id: prim-sig.sml,v 1.1 2003/05/23 13:16:52 tom7 Exp $ *)

signature PRIMITIVE =
sig

    type prim

    type isect = prim * real

    type p3 = {x : real,
	       y : real,
	       z : real}

    (*             min    max   *)
    type bounds = {x : real * real,
		   y : real * real,
		   z : real * real}

    type color = real * real * real

    type texture = color Array2.array

    (* color *)
    type matl = {diffuse     : color,
		 specular    : color,
		 reflectance : real,
		 exponent    : real,
		 text        : texture option}

    type ray = p3 * p3
	
    val sphere : (p3 * real)    * matl -> prim
    val tri    : ((p3 * p3 * p3) *
		  (p3 * p3 * p3) * 
		  ((real * real) * (real * real) * (real * real))) * matl 
	-> prim

    val extent : prim -> bounds

    val includebox : bounds -> prim -> bool

    val intersect : ray -> prim -> isect option

    val tostring  : prim -> string

    val getmatl : prim -> matl

    val trisect2d : bool ->
	             (((real * real)  *
                     (real * real)  *
		     (real * real))) ->
             	     (real * real) -> bool

    val getnormal : (prim * p3) -> p3
    val gettexuv  : (prim * p3) -> (real*real)

end