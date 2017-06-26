(* $Id: trace-sig.sml,v 1.1 2003/05/23 13:16:52 tom7 Exp $ *)
signature TRACE =
sig

    type octree

    type p3 = {x : real,
	       y : real,
	       z : real}

    type bounds = {x : real * real,
		   y : real * real,
		   z : real * real}

    type ray = p3 * p3
	
    type color = (real * real * real)

    type light = p3 * color

    type scene = octree * light list

    val trace : scene -> ray -> color option
	
    (* where the array is width*3 bytes wide (RGB) *)
    (* (p3 * p3 * p3) = (origin * lookdir * updir) *)
    (* lookdir and updir are normalized *)
    (* int1 = pixels wide *)
    (* int2 = pixels high *)
    (* real = focal length *)

    val render : (scene * (p3 * p3 * p3) * (int * int) * real)
	-> Word8.word Array2.array

end
