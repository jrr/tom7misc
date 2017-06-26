
structure Point : POINT =
struct

    exception Unimplemented

    type p3 = {x : real,
	       y : real,
	       z : real}

    type bounds = {x : real * real,
		   y : real * real,
		   z : real * real}

    type ray = p3 * p3
	
    type plane = p3 * p3

    fun within ({x=x,y=y,z=z} : p3,
		{x=(x1,x2),
		 y=(y1,y2),
		 z=(z1,z2)} : bounds) = 
	(x1 <= x andalso x <= x2) andalso
	(y1 <= y andalso y <= y2) andalso
	(z1 <= z andalso z <= z2)

    fun p3add ({x=x1,y=y1,z=z1},
	       {x=x2,y=y2,z=z2}) =
	{x=x1+x2,
	 y=y1+y2,
	 z=z1+z2} : p3

    fun p3scale ({x=x,y=y,z=z},
		 s) =
	{x=x*s,
	 y=y*s,
	 z=z*s} : p3


    fun norm (pt as {x=x,y=y,z=z}) : p3 =
	p3scale (pt,1.0 / Math.sqrt (x*x + y*y + z*z))

    fun p3sub ({x=x1,y=y1,z=z1},
	       {x=x2,y=y2,z=z2}) =
	{x=x1-x2,
	 y=y1-y2,
	 z=z1-z2} : p3


    fun p3cross ({x=a1,y=a2,z=a3},
		 {x=b1,y=b2,z=b3}) =
	{x=a2*b3 - a3*b2,
	 y=a3*b1 - a1*b3,
	 z=a1*b2 - a2*b1} : p3

    fun p3dot ({x=x1,y=y1,z=z1},
	       {x=x2,y=y2,z=z2}) = (x1*x2 + y1*y2 + z1*z2) : real

    fun boundstostring {x=(xmin,xmax),
			y=(ymin,ymax),
			z=(zmin,zmax)} =
	"(x=(" ^ Real.toString xmin ^ ", " ^ Real.toString xmax ^"), " ^
  	 "y=(" ^ Real.toString ymin ^ ", " ^ Real.toString ymax ^"), " ^
	 "z=(" ^ Real.toString zmin ^ ", " ^ Real.toString zmax ^"))" 

    fun pointtostring {x=x,y=y,z=z} = "(" ^
	Real.toString x ^ ", " ^
	Real.toString y ^ ", " ^
	Real.toString z ^ ")"
	
    fun raytostring (a, b) = "{@" ^ 
	pointtostring a ^ " -> " ^
	pointtostring b ^ "}"
	

    fun rayplaneisect  
	({x=px,
	  y=py,
	  z=pz} : p3,
	 {x=dx,
	  y=dy,
	  z=dz} : p3)
	({x=x0,
	  y=y0,
	  z=z0} : p3, 
	 {x=A,
	  y=B,
	  z=C} : p3) =
	let val D = ~ (A * x0 +
		       B * y0 + 
		       C * z0)
	    val den = (A * dx +
		       B * dy +
		       C * dz)
	in
	    if Real.== (den, 0.0) then
		NONE
	    else let
		 val t = ~ (A * px +
			    B * py +
			    C * pz + D) / den
		 in
		     if t <= 0.0 then NONE
		     else SOME t
		 end
	end

end