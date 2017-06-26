
(* $Id: prim.sml,v 1.1 2003/05/23 13:16:52 tom7 Exp $ *)

structure Primitive :> PRIMITIVE = 
struct

    exception Unimplemented

    open Point

    datatype Shape = Sphere of p3 * real
	                      (* normalized normals *)
      | Tri of (p3 * p3 * p3) * (p3 * p3 * p3)
	         (* texture coords *)
	     * ((real * real) * (real * real) * (real * real))

    type color = real * real * real


    type texture = color Array2.array

    (* color *)
    type matl = {diffuse     : color,
		 specular    : color,
		 reflectance : real,
		 exponent    : real,
		 text        : texture option}

    type prim = Shape * matl

    fun sphere (a,b : matl) = (Sphere a, b) : prim
    fun tri    (a,b) = (Tri a, b)


    (* identity combinator *)
    fun I x = x

    local
	infix mn mx
	fun a mn b = if (a < b) then a : real else b
	fun a mx b = if (a > b) then a : real else b
    in

    (* bounding box of a shape. Some octree code could save unnecessary
       tests by being more accurate about when a shape lies in some
       bounds ... *)
    fun extent (Sphere ({x=cx,y=cy,z=cz},r), _) =
	{x=(cx-r,cx+r),
	 y=(cy-r,cy+r),
	 z=(cz-r,cz+r)} : bounds
      | extent (Tri (({x=x1,y=y1,z=z1},
		      {x=x2,y=y2,z=z2},
		      {x=x3,y=y3,z=z3}), _, _), _) = 
	{x=(x1 mn x2 mn x3, x1 mx x2 mx x3),
	 y=(y1 mn y2 mn y3, y1 mx y2 mx y3),
	 z=(z1 mn z2 mn z3, z1 mx z2 mx z3)} : bounds
    end

    type isect = prim * real

    fun inrange ((umin,umax), v : real) = (umin <= v) andalso (v <= umax)

    fun tostring (Sphere(ctr, radius), _) = ("Sph" ^ (pointtostring ctr) ^
					     "x" ^ Real.toString radius)
      | tostring (Tri((p1,p2,p3),_, _), _) =
	("Tri{" ^
	 pointtostring p1 ^ ", " ^
	 pointtostring p2 ^ ", " ^
	 pointtostring p3 ^ "}")

    fun within' ({x=(x1,x2),
		  y=(y1,y2),
		  z=(z1,z2)} : bounds) ({x=x,y=y,z=z} : p3)= 
	(x1 <= x andalso x <= x2) andalso
	(y1 <= y andalso y <= y2) andalso
	(z1 <= z andalso z <= z2)

    fun pt' (xx, yy, zz) = {x=xx,
			    y=yy,
			    z=zz} : p3

    (* from C++ code *)
    fun themisect ((b1 : bounds), (b2 : bounds)) : bool =
	let 
	    val (xmin1,xmax1) = #x b1
	    val (ymin1,ymax1) = #y b1
	    val (zmin1,zmax1) = #z b1

	    val (xmin2,xmax2) = #x b2
	    val (ymin2,ymax2) = #y b2
	    val (zmin2,zmax2) = #z b2
	in

	    (xmax1 >= xmin2) andalso (xmin1 <= xmax2) andalso
	    (ymax1 >= ymin2) andalso (ymin1 <= ymax2) andalso
	    (zmax1 >= zmin2) andalso (zmin1 <= zmax2)

	end

    fun includebox (b : bounds) (s : prim) =
	(* an intersection with the box if any of the extremities are in b *)
	themisect (b, extent s)

    fun trisect2d X
	(a : real * real, 
	 b : real * real,
	 c : real * real)
	(u : real, v : real) : bool =
	let 
	    val (cu, cv) = (((#1 a) + (#1 b) + (#1 c)) / 3.0,
			    ((#2 a) + (#2 b) + (#2 c)) / 3.0)
		
	    fun inhalfplane ((u1, v1), (u2, v2)) =
		let 
		    val dv = (v2-v1)
		    val du = (u2-u1)

		    val _ = if X then 
			print ("du: " ^ Real.toString du ^ 
			       ", dv: " ^ Real.toString dv ^"\n") 
			    else ()

		    val cf = I

		    val (bb, aa) = (if ((Real.abs dv) >
					(Real.abs du)) 
					then (du/dv,~1.0)
				    else (~1.0, dv/du))

		    val cc = ~ (aa * u1 + bb * v1)
			
		in
		    if X then 
		    print ("test: " ^ 
			   Real.toString aa ^ " * " ^
			   Real.toString u  ^ " + " ^
			   Real.toString bb ^ " * " ^
 			   Real.toString v  ^ " + " ^
			   Real.toString cc ^ " < 0.0 \n")
		    else ();
		    ((aa * u  + bb * v  + cc) < 0.0) =
		    ((aa * cu + bb * cv + cc) < 0.0)
		end
	in
	    (inhalfplane (a,b)) andalso
	    (inhalfplane (b,c)) andalso
	    (inhalfplane (c,a)) andalso
	    true
	end

    local
	datatype blah = A | B | C
	fun argmax (a : real, b, c) =
	    if c > a then
		(if b > c then B else C)
	    else
		(if b > a then B else A)
		     

    in
	fun make2d (trinorm : p3) (a : p3, b: p3, c : p3) = 
	    let
		val uvf : p3 -> (real * real) =
		    
		    case argmax (abs (#x trinorm),
				 abs (#y trinorm),
				 abs (#z trinorm)) of
			A => (fn q => (#y q, #z q))
		      | B => (fn q => (#z q, #x q))
		      | C => (fn q => (#x q, #y q))
	    in
		((uvf a,
		  uvf b,
		  uvf c), uvf : p3 -> (real * real))
	    end
    end

    fun solvedegenquad (B, C) = 
	let 
	    val inside = B*B - 4.0*C
	in
	    if (inside < 0.0) then NONE
	    else SOME let val top = Math.sqrt inside
		      in
			  ((~B + top)/2.0,
			   (~B - top)/2.0)
		      end
	end

	fun intersect ({x=x0,
			y=y0,
			z=z0}:p3,
		       {x=dx, (* invt: normalized! *)
			y=dy,
			z=dz}: p3) (s as (Sphere(ctr as {x=a,y=b,z=c},
					       radius), _)) =
	    let 
		val B = 2.0 * (dx * (x0 - a) +
			       dy * (y0 - b) +
			       dz * (z0 - c))

		val C = (x0 - a) * (x0 - a) +
		        (y0 - b) * (y0 - b) +
			(z0 - c) * (z0 - c) - (radius * radius)

	    in
		case solvedegenquad(B,C) of
		    NONE => NONE
		  | SOME (r1, r2) => if r1 < 0.0 then (if r2 < 0.0 then NONE
						       else SOME (s,r2))
				     else if r1 < r2 then SOME (s,r1)
					  else (if r2 < 0.0 then SOME(s,r1)
						else SOME (s,r2))
	    end
			
	  | intersect r (tr as (Tri((a, b, c),_, _), mtl)) =
	    ((* print ("Intersect? " ^ tostring tr ^ " with " ^ 
		    raytostring r ^ "\n") *)0;
	     let
		 val trinorm = norm (p3cross(p3sub(b, a), p3sub(c, a)))
		 val plane = (a, trinorm)
		 val pointinplane = rayplaneisect r plane
	     in
		 case pointinplane of 
		     NONE => NONE
		   | SOME t => 
			 ((* did this point end up inside the triangle? *)
				let

				    val ((a2,b2,c2),uvf) = 
					make2d trinorm (a,b,c)

				    val (u, v) = uvf (p3add(#1 r,
							    p3scale(#2 r, t)))

				in
				    if   trisect2d false (a2,b2,c2) (u,v)
				    then SOME (tr, t)
				    else NONE
				end
			    )
	     end)
	    
    val getmatl = #2 : prim -> matl

    (* Microsoft Barycentric(R) Coordinates 98 *)
    fun barycentric (((x1,y1),
		      (x2,y2),
		      (x3,y3)), (x0,y0)) =
	let val b0 = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1) : real
	in (((x2 - x0) * (y3 - y0) - (x3 - x0) * (y2 - y0)) / b0,
	    ((x3 - x0) * (y1 - y0) - (x1 - x0) * (y3 - y0)) / b0,
	    ((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)) / b0)
	end

    (* invt: point must be on primitive *)
    fun getnormal (pr as (Tri  ((a,b,c),(na,nb,nc), _), _),
		   pt as {x=x,y=y,z=z}) =
	let

	    val (nt,uvf) = (make2d 
			    (norm (p3cross(p3sub(b, a), p3sub(c, a))))
			    (a,b,c))

	    val (x0, y0) = uvf pt
	    val (b1, b2, b3) = barycentric (nt, (x0,y0))

	in
	    norm(p3add(p3scale(na,b1),
		       p3add(p3scale(nb,b2),
			     p3scale(nc,b3))))
	end
      | getnormal (pr as (Sphere ({x=x,y=y,z=z},r), _),
		   pt as {x=xx,y=yy,z=zz}) =
	(* ignore r. The normal is just the vector from the center to
	   the intersection point *)
	norm {x=xx-x,
	      y=yy-y,
	      z=zz-z}

    fun gettexuv (pr as (Tri  ((a,b,c),(na,nb,nc), 
			       ((au,av), (bu,bv), (cu,cv))), _),
		  pt as {x=x,y=y,z=z}) =
	let
	    val (nt,uvf) = (make2d 
			    (norm (p3cross(p3sub(b, a), p3sub(c, a))))
			    (a,b,c))

	    val (x0, y0) = uvf pt
	    val (b1, b2, b3) = barycentric (nt, (x0,y0))
	in 
	    ((b1 * au + b2 * bu + b3 * cu) / 3.0,
	     (b1 * av + b2 * bv + b3 * bv) / 3.0)
	end
      | gettexuv ((Sphere (ctr,r), _),
		  pt as {x=px,y=py,z=pz}) =
	let val {x=a,y=b,z=c} = norm(p3sub(pt, ctr))
	in
	    ((1.0 + a) / 2.0, 
	     (1.0 + b) / 2.0)
	end
end