
(* $Id: trace.sml,v 1.1 2003/05/23 13:16:52 tom7 Exp $ *)

(* In ML-land, we count the levels of abstraction, not the number
   of primitive operations per iteration! *)

functor Tracefun (oct : OCTREE) :> TRACE where type octree = oct.octree =
struct

    type octree = oct.octree

    type prim = oct.prim

    structure Primitive = oct.Primitive

    exception Unimplemented

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


    local 
	open Point
	val ambient = (0.05, 0.05, 0.05)
	fun sat (r) = if r >= 1.0 then 1.0 else r
	fun sat3 (r,g,b) = (sat r,
			    sat g,
			    sat b)
	fun cadd ((r1,g1,b1) : real*real*real,(r2,g2,b2)) =
	    ((r1 + r2), (g1 + g2), (b1 + b2))
	fun sum f = foldr (fn (a,b) => cadd(f a,b)) (0.0,0.0,0.0)
	fun cscale ((r, g, b), s : real) =
	    ((r * s),
	     (g * s),
	     (b * s))

	fun cmult ((r1,g1,b1) : real*real*real,(r2,g2,b2)) =
	    ((r1 * r2), (g1 * g2), (b1 * b2))

	fun dotpeg (a, b) =
	    let val d = p3dot (a, b)
	    in if d <= 0.0 then 0.0 else d
	    end

    in
	fun trace (oct, ll) (r : ray) =
	    let fun shade (p, t) =
		let val point = p3add(#1 r, p3scale(#2 r, t))
		    val nor   = Primitive.getnormal (p, point)

		    fun shadowed (li : light) = let
			val ldist = p3sub (#1 li, point)
			val dir = norm ldist
				      in
			(case oct.intersect oct (p3add(point,
						      p3scale(dir,0.0001)),
						dir) of
			    NONE => false
			  | SOME(p : Primitive.prim,t) =>
				(t < (p3dot(ldist,ldist))))
				      end
		    val mat = Primitive.getmatl p
		    val lll = map (fn x => (x, shadowed x)) ll

		    val diffuse = case #text mat of
			NONE => #diffuse mat
		      | SOME arr => (
				     let val (tu, tv) = Primitive.gettexuv 
					 (p, point)
					 val (r, c) = Array2.dimensions arr
				     in
					 Array2.sub
					 (arr,
					  Real.floor(tv * (Real.fromInt r)),
					  Real.floor(tu * (Real.fromInt c)))
				     end
				     )

		    val eye = norm(p3sub(#1 r, point))
(*
		    val _ = print ("intersect: " ^ pointtostring point ^ "\n")
		    val _ = print ("normal:    " ^ pointtostring nor ^ "\n")


		    val _ = print ("eye: " ^ pointtostring eye ^ "\n")
		    val _ = print ("light: " ^ pointtostring dir ^ "\n")
(*
		    val half = p3scale(p3add(eye,dir),0.5)
		    val _ = print ("half: " ^ pointtostring half ^ "\n")
		    val dop  = dotpeg(nor, dir)*)

		    val _ = print ("dop: " ^ Real.toString dop ^ "\n") *)

		    val res = 
		sat3(
		cadd(cmult(diffuse,
			   cadd(ambient, 
				(sum (fn (li,ss) =>
				      if ss then
					  (0.0,0.0,0.0)
				      else cscale(#2 (li : light),
						  dotpeg(nor,
							 norm(p3sub(#1 li,
								    point)))))
				 lll))),
		     cmult(#specular mat,
			   sum (fn (li,ss) => 
				if ss then
				    (0.0,0.0,0.0)
				else cscale(#2 li,
					    Math.pow(dotpeg(nor,
						 (* "half" *)
						 p3scale(p3add(eye,
						           norm(p3sub(#1 li,
								      point
								      ))),
							 0.5)),
						#exponent mat)
					    )) lll)))
	    in
		res
	    end					     
	    (*
	     half: avg of light v and eye v

	    (#diffuse (Primitive.getmatl p)) *)
	in
	case oct.intersect oct r of
	    NONE => NONE
	  (* fixme: add fog effect *)
	  | SOME(p : Primitive.prim,t) => SOME (shade (p,t))
	end
		 
    end

    local open Array2
	  open Point

	fun expandthree (a : (real*real*real) array) : Word8.word array
	    = let val (rows, cols) = dimensions a
		  fun tabf (r, c) = 
		      (((case c mod 3 of
			     0 => #1
			   | 1 => #2
			   | _ => #3) 
			     : (real * real * real) -> real)
			     (sub (a, r, c div 3)))
			     * 255.0
	      in
		  tabulate RowMajor (rows, cols * 3, 
				     Word8.fromInt o Real.floor o tabf)
	      end
						  
    in
    fun render (scn, 
		cam as (origin : p3, lookdir : p3, updir : p3), 
		(wpxl, hpxl), 
		focallength) = 
	let 
	    val rightdir = norm (p3cross (lookdir, updir))
	    val rightslice = p3scale(rightdir, 1.0 / 
				     ((Real.fromInt wpxl)))
	    val upslice    = p3scale(updir, 1.0 / 
				     ((Real.fromInt hpxl)))

	    val focalpoint = p3add(p3scale(lookdir, focallength),
				   origin)

	    (* the start is the bottom left; so subtract half of
	       the up and right dirs *)
	    val start = p3sub(p3sub(origin,
				    p3scale(rightdir, 0.5)),
			      p3scale(updir, 0.5));
		
	    val _ = print ("rightdir: " ^
			   (pointtostring rightdir) ^ "\n");

	    val ourtrace = trace scn
		
	    fun doray (r, c) = 
		let 
		    (* report a little *)
		    val _ = case c of 
			0 => (case r mod 32 of 
			    0 => print ("row " ^ Int.toString r ^ "/" 
					^ Int.toString hpxl ^ " ")
			  | 31 => print "\n"
			  | _ => print ".")
		      | _ => ()

		    val from = 
			p3add(p3add(start,
				    p3scale(rightslice, Real.fromInt c)),
			      p3scale(upslice, Real.fromInt r))

		    val dir = norm(p3sub (focalpoint, from))

		in
		    case ourtrace (from, dir) of
			(* FIXME bgcolor *)
			NONE =>  (0.0, 0.0, 0.0)
(*
			    (Real.fromInt r, 
				 Real.abs(Math.sin((Real.fromInt c) / 50.0)),
				 Real.abs(Math.sin((Real.fromInt r) / 20.0) *
					  Math.sin((Real.fromInt c) / 12.0)))
			    *)
		      | SOME c => c
		end
	    val sss = tabulate RowMajor (hpxl, wpxl, doray)
	    val _ = print "\nConverting to RGB 24-bit...\n"
	in
	    expandthree sss
	end
    end
end