
(* $Id: octree.sml,v 1.1 2003/05/23 13:16:52 tom7 Exp $ *)

functor Octfun (Prim : PRIMITIVE) :> OCTREE (* where type prim = Prim.prim *) =
struct

    val rayeps = 0.00001 (* ? *)

    structure Primitive = Prim

    type prim = Primitive.prim


    exception Unimplemented and Impossible


    open Point
        
    type isect = prim * real
        
    datatype Node = Leaf of prim list
      | Internal of iaxis
      | Empty
    withtype onode = bounds * Node
        
    (* quad 0 1 2 3 4 5 6 7 *)
    (* invt: length = 8 *)
    and iaxis = onode Vector.vector

    type octree = onode

(*
          z
          .
          |

   +------+------+
   |\  0     1  /|
   | +----+----+ |
   | |\   |   /| |  
   | | +--|--+ | |
   | | | 4|5 | | |
   + +----x----+ +  --> y
   | | | 6|7 | | |
   | | +--|--+ | |
   | |/   |   \| |
   | +---------+ |
   |/  2     3  \|
   +------+------+

*)

    fun I x = x

    (* invt: a < b *)
    fun mid (a : real, b) : real =
        a + (( b - a ) / 2.0)
        
    (* FIXME *)
    (* invt: b power of 2 *)
    infix &
    fun a & 1 = 1 = (a mod 2)
      | a & b = (a div 2) & (b div 2)


    (* if there are more than objs objects in a node,
     we'll split (unless we are at a depth > depth) *)
    local 

        fun nh (nil, _, _, b : bounds) = (b, Empty)
          | nh (pl, _, 0, b) = (b, Leaf pl)
          | nh (pl, os, dp, b) =
            if os >= (length pl) then (b, Leaf pl)
            else (* split *)
                let
		    val midx = mid (#x b)
                    val midy = mid (#y b)
                    val midz = mid (#z b)

                    fun box n = 
                        let 
                            val newb = {z = if (n & 1) then (midz, #2 (#z b))
                                            else (#1 (#z b), midz),
                                        y = if (n & 2) then (midy, #2 (#y b))
                                            else (#1 (#y b), midy),
                                        x = if (n & 4) then (midx, #2 (#x b))
                                            else (#1 (#x b), midx)}
                        in
                            nh (List.filter (Prim.includebox newb) pl,
                                os,
                                dp - 1,
                                newb)
                        end
                in
                    (b, Internal(Vector.tabulate (8, box)))
                end

         
        fun grow ((min1 : real,max1 : real),
                  (min2,max2)) = (if min1 < min2 then min1 else min2,
                                      if max1 > max2 then max1 else max2)
            
        fun swell ({x=x1,y=y1,z=z1} : bounds,
                   {x=x2,y=y2,z=z2}) = {x = grow(x1,x2),
                                        y = grow(y1,y2),
                                        z = grow(z1,z2)}
                    
        in          
            fun new (pl, objs, depth) = nh (pl, objs, depth,
                                            (* calc start bounds *)
                                            foldr (fn (p,c) => swell 
                                                   (Prim.extent p, c))
                                            (Prim.extent (hd pl)) pl)
    end

(* "pretty"-print an octree *)
    fun tostring (ot : octree) = 
	let
	    fun pad 0 = ""
	      | pad d = " " ^ (pad (d-1))

	    fun tostring' (d,(b,Empty)) = (pad d) ^ "-\n"
	      | tostring' (d,(b,Internal v)) = (pad d) ^ "<" ^
		(boundstostring b) ^ " {\n" ^
		(foldr (fn (a,b) => (pad (d+1)) ^ "-> " ^ Int.toString a ^ ":\n"
			^ (tostring' (d+3,(Vector.sub (v, a)))) ^ b) 
		 "" (List.tabulate (8, I))) ^ (pad d) ^ "}>\n"
	      | tostring' (d,(b,Leaf pl)) = (pad d) ^ "leaf:\n" ^
		(foldr (fn (a, b) => (pad (d+1)) ^ "* " ^ 
			Prim.tostring a ^ "\n" ^ b) "" pl)
	in
	    tostring' (0, ot)
	end
    

    (* find the leaf voxel containing pt *)
    fun getvox (bds, root) (pt as {x=x,
				   y=y,
				   z=z}) =
	(if within (pt, bds) then 
	    (let 
		(* invt: point is inside tree *)
		 fun gvh (bb, Internal v) = 
		     gvh (Vector.sub (v,
				      (if x > (mid (#x bb)) then 4 else 0) +
				      (if y > (mid (#y bb)) then 2 else 0) +
				      (if z > (mid (#z bb)) then 1 else 0)))
		   | gvh x = x
	    in
		SOME (gvh (bds, root))
	    end)
	else NONE)

    local
	val xx = (#x : p3 -> real)
	val yy = (#y : p3 -> real)
	val zz = (#z : p3 -> real)
	    
    (* don't want to figure out how this works, so here is a
       messy but straightforward port of the C++ code *)

    in
    fun entrypt (bb:bounds) (orig, dir) = 
	let 
	    fun test raxis limit tf1 tf2 t =
		let val curt = (limit - (raxis orig)) / (raxis dir)
		in
		    if curt < t then
			let val pp = (p3add(orig, p3scale(dir, curt - rayeps)))
			in
			     if tf1 pp andalso tf2 pp then curt else t
			end
		    else t
		end

	    val (xmin,xmax) = (#x bb)
	    val (ymin,ymax) = (#y bb)
	    val (zmin,zmax) = (#z bb)

	    fun wix ({x=x,...}:p3) = (xmin <= x) andalso (x <= xmax)
	    fun wiy ({y=y,...}:p3) = (ymin <= y) andalso (y <= ymax)
	    fun wiz ({z=z,...}:p3) = (zmin <= z) andalso (z <= zmax)

	    val tstart  = 1.0e16
	    val compute = 
		(if Real.!= (xx dir,0.0) then
		     (test xx xmax wiy wiz) o
		     (test xx xmin wiy wiz)
		else I) o
		(if Real.!= (yy dir,0.0) then
		     (test yy ymax wix wiz) o
		     (test yy ymin wix wiz)
		else I) o
		(if Real.!= (zz dir,0.0) then
		     (test zz zmax wix wiy) o
		     (test zz zmin wix wiy)
		else I)
	    val tend = compute tstart
	in
	    if tend >= 1.0e16 then NONE
	    else SOME (p3add(orig, p3scale (dir, tend + rayeps)))
	end

    fun exitpt (bb:bounds) (orig, dir) : p3 = 
	let 
	    fun test raxis limit tf1 tf2 t =
		let val curt = (limit - (raxis orig)) / (raxis dir)
		in
		    if curt > t then
			let val pp = (p3add(orig, p3scale(dir, curt - rayeps)))
			in
			     if tf1 pp andalso tf2 pp then curt else t
			end
		    else t
		end

	    val (xmin,xmax) = (#x bb)
	    val (ymin,ymax) = (#y bb)
	    val (zmin,zmax) = (#z bb)

	    fun wix ({x=x,...}:p3) = (xmin <= x) andalso (x <= xmax)
	    fun wiy ({y=y,...}:p3) = (ymin <= y) andalso (y <= ymax)
	    fun wiz ({z=z,...}:p3) = (zmin <= z) andalso (z <= zmax)

	    val tstart  = ~1.0e16
	    val compute = 
		(if Real.!= (xx dir,0.0) then
		     (test xx xmax wiy wiz) o
		     (test xx xmin wiy wiz)
		else I) o
		(if Real.!= (yy dir,0.0) then
		     (test yy ymax wix wiz) o
		     (test yy ymin wix wiz)
		else I) o
		(if Real.!= (zz dir,0.0) then
		     (test zz zmax wix wiy) o
		     (test zz zmin wix wiy)
		else I)
	    val tend = compute tstart
	in
	    p3add (orig, p3scale (dir, tend + rayeps))
	end
    end
	    
    fun entryvox (ot : octree) ( pt as (orig, dir) ) = 
	(case getvox ot orig of
	    SOME x => SOME x
	  | NONE => ( case (entrypt (#1 ot) pt) of
		     NONE => NONE
		   | SOME y => getvox ot y))
	
	    
    fun nodesect _ Empty = NONE
      | nodesect r (Leaf pl) = 
	foldr (fn (pr, cc as (SOME c)) => (case Prim.intersect r pr of
				       NONE => cc 
				     | (SOME(p,t)) =>  
					   if (t < (#2 c)) then 
					       SOME(p,t)
					   else cc)  
               | (pr, NONE) => Prim.intersect r pr) (NONE : isect option) pl 
      | nodesect _ _ = raise Impossible


    fun intersect ot r =
        let 

	    (* voxel -> ray -> (voxel, ray) *)
	    fun nextvox (v : octree) (rr : ray) =
		let val ep = exitpt (#1 v) rr
		in (getvox ot ep, (ep, #2 rr))
		end

	    (* fixme: does r change? *)
            fun ih (NONE : octree option, r) = NONE
              | ih ((SOME v), r) = (case nodesect r (#2 v) of
					NONE => ih (nextvox v r)
				      | a => a)
        in
            ih (entryvox ot r, r)
        end


end