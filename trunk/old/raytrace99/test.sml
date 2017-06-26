(* $Id: test.sml,v 1.1 2003/05/23 13:16:52 tom7 Exp $ *)

val _ = SMLofNJ.Internals.GC.messages false;

structure Octree = Dumbfun(Primitive);
structure Trace  = Tracefun(Octree);

(* val plasma = Main.loadtex "tex1.raw"; *)

(* val propaganda = Main.loadtex "tex2.raw"; *)

open Octree;
open Point;

fun pt xx yy zz = {x=xx,
		   y=yy,
		   z=zz} : p3
(*
local
val (ta, tb, tc) = ((0.5, 0.8), (0.7, 0.4), (0.2, 0.2))
in
val _ = 
    let

	fun rfi (a, b) = (Real.floor (20.0 * a),
			  Real.floor (20.0 * b))

	val (tta, ttb, ttc) = (rfi ta, rfi tb, rfi tc)
	fun ss 20 = ()
	  | ss v  = 
	    let
		fun tt 20 = ()
		  | tt u =
		    ( print
		     
		     (if (u,20-v) = tta then "A"
		      else if (u,20-v) = ttb then "B"
			   else if (u,20-v) = ttc then "C"
				else if Primitive.trisect2d false (ta, tb, tc)
				    ((Real.fromInt u) / 20.0,
				     (Real.fromInt (20-v)) / 20.0)
					 then "@"
				     else ":");
			  tt (u+1))
	    in
		(tt 0; print "\n"; ss (v+1))
	    end
    in
	ss 0
    end
end
*)
(* val res = Primitive.trisect2d true (ta, tb, tc) (0.5, 0.5); *)
(*
local exception stop in
val _ = raise stop
end
*)

fun plastic clr = {diffuse  = clr : real * real * real,
		   specular = (1.0, 1.0, 1.0),
		   reflectance = 0.0,
		   exponent = 70.0,
		   text = NONE} : Primitive.matl
(*
val plasmat = {diffuse = (0.0,0.0,0.0),
	       specular = (1.0, 1.0, 1.0),
	       reflectance = 0.0,
	       exponent = 15.0,
	       text = SOME plasma} : Primitive.matl

val propagandat = {diffuse = (0.0,0.0,0.0),
		   specular = (1.0, 1.0, 1.0),
		   reflectance = 0.0,
		   exponent = 15.0,
		   text = SOME propaganda} : Primitive.matl
*)

fun makethingy 0 _       _ = nil
  | makethingy n (x,y,z) r =
    let val d = r + (r / 2.0)
    in [(Primitive.sphere (((pt x y z), r),
			   plastic
			       (0.2 + (x + 3.5) / 7.5,
				0.2 + (y + 3.5) / 7.5,
				0.2 + (z + 3.5) / 7.5)
			       ))]
	@ (makethingy (n - 1) (x + d, y, z) (r / 2.0))
	@ (makethingy (n - 1) (x - d, y, z) (r / 2.0))
	@ (makethingy (n - 1) (x, y + d, z) (r / 2.0))
	@ (makethingy (n - 1) (x, y - d, z) (r / 2.0))
	@ (makethingy (n - 1) (x, y, z + d) (r / 2.0))
	@ (makethingy (n - 1) (x, y, z - d) (r / 2.0))
    end

val thingy  = makethingy 3 (0.0, 0.0, 0.0) (2.0)

(*
fun sign ((loc,
	   dir,
	   up)) = 
    let val center = (p3add(loc, p3scale(dir, 1.0)))
	val right = norm (p3cross (lookdir, updir))
    in (Primitive.tri (p3scale
*)

val lights = [((pt 4.5 1.0 1.2), (0.7, 1.0, 0.7)),
	      ((pt ~4.5 ~1.0 ~1.2), (1.0, 0.3, 0.8))]
val treetest = 
    new (
(*
	 (Primitive.tri (  ((pt 4.0  4.0  1.0,
			     pt 4.0  4.0 ~1.0,
			     pt 4.0 ~4.0 ~1.0),
			    (pt 1.0 0.0 0.0,
			     pt 1.0 0.0 0.0,
			     pt 1.0 0.0 0.0),
			    ((0.0, 1.0),
			     (0.0, 0.0),
			     (1.0, 0.0))),
			 propagandat)) ::

	 (Primitive.tri (  ((pt 4.0 ~4.0  1.0,
			     pt 4.0  4.0  1.0,
			     pt 4.0 ~4.0 ~1.0),
			    (pt 1.0 0.0 0.0,
			     pt 1.0 0.0 0.0,
			     pt 1.0 0.0 0.0),
			    ((1.0, 1.0),
			     (0.0, 1.0),
			     (1.0, 0.0))),
	 propagandat)) :: *)
	 thingy, 2, 2);

(*
    new ([ (* Primitive.sphere (pt 0.0 0.0 0.0, 0.5), *)
	   (* Primitive.sphere (pt 1.0 0.0 0.0, 0.25) *)

(*
	  Primitive.sphere ( ( (pt 3.8 0.0 4.5),
			      0.5),

	  plastic (0.4, 0.0, 1.0)),
*)
	  Primitive.sphere ( ( (pt 2.0 0.5 1.0), 1.0),
			    plastic (0.0, 1.0, 0.7)),
	  
	  Primitive.sphere ( ( (pt 0.0 0.0 0.0),
			       2.5),

			    plastic (0.4, 0.0, 1.0))


(*              (* textured *)
	  Primitive.tri (( (pt 0.0  0.0 0.0,
			    pt 0.0  0.0 5.0,
			    pt 0.0  5.0 0.0),                
			  (norm (pt 1.0 ~0.5 ~0.2),
			   norm (pt 1.0 0.2 0.5),
			   norm (pt 1.0 0.5 ~0.2)),
			  ((0.5, 0.0),
			   (0.0, 1.0),
			   (1.0, 0.0))
			  ),
			 
			 plastic (1.0, 0.0, 0.0) )
*)

(*
	  Primitive.tri ((pt ~1.0  0.0 0.0,
			  pt 0.0  0.0 0.0,
			  pt 0.0  1.0 0.0
			  ),
			 plastic (0.0, 1.0, 0.0)),

	  Primitive.tri ((pt 0.0  0.0 1.0,
			   pt 0.0  1.0 1.0,
			   pt 0.0  1.0 0.0
			   ),
			 plastic (0.0, 0.0, 1.0))
*)
	  ],
	 1, 2);
*)

(*
val poo = Trace.trace (treetest, lights) (pt 8.0 0.0 0.0, 
					  norm (pt ~8.0 0.0 0.0))
*)
(*
local exception stop
in val _ = raise stop end;
*)
(*
local open TextIO 
    val ouf = openOut "octreedump.txt"
in
    
    val _ = output (ouf, Octree.tostring treetest)
    val _ = closeOut ouf
	
    val _ = print " -- ok \n"
end;
*)
local
    fun cam loc lookat up =
	(loc,
	 norm (p3sub(lookat, loc)),
	 norm up)

    fun tos t = if t < 10 then ("00" ^ Int.toString t)
		else if t < 100 then ("0" ^ Int.toString t)
		     else Int.toString t
in
    fun doit t = 
	let val tt = (Real.fromInt t) / 80.0
	in Main.saveraw 
	    (Trace.render ((treetest,lights), 
			   (cam 
			    (pt 
			     (8.0 * (Math.sin tt)) 
			     (8.0 * (Math.cos tt)) 
			     0.0)
			    (pt 0.0 0.0 0.0)
			    (pt 0.0 (Math.sin tt) 1.0)),
			    (320, 240),
			    (0.3 + (tt / 2.0))))
			    ("anim/" ^ (tos t) ^ ".raw")
	end

    fun domany 0 = ()
      | domany m = 
	(print ("Doing #" ^ Int.toString m ^ "...\n");
	 doit m;
	 domany (m-1))
end;

