structure Blob :> BLOB =
struct

    type blob = string list
	
    fun size nil = 0
      | size (h::t) = String.size h + size t

    fun blob "" = nil
      | blob s  = [s]

    val tostring = String.concat

    fun tostringlist x = x

    fun sub (nil, _) = raise SubScript
      | sub (h::t, i) = 
	if i < size h then
	    String.sub (h, i) 
	else sub (t, i - size h)

    (* XXX not efficient *)
    fun subblob (b,lo,hi) = blob (String.substring (tostring b, lo, hi))

    val concat = op @

    val op ^ = op @

    val implode = blob o String.implode

    (* this is never going to be efficient for large strings,
       so it is not particularly optimized here. *)
    fun explode nil = nil
      | explode (h::t) = String.explode h @ explode t

    fun map f = List.map (String.map f)

    fun translate f = List.map (String.translate f)
	
    fun split f (x,y) = (f x, f y)

    (* XXX not efficient *)
    val compare = String.compare o (split tostring)
    val op <= = String.<= o (split tostring)
    val op < = String.< o (split tostring)
    val op > = String.> o (split tostring)
    val op >= = String.>= o (split tostring)

    fun eq (nil, nil) = true
      | eq (_, nil) = false
      | eq (nil,_) = false
      | eq (a::b,c::d) = String.= (a, b) andalso eq (b, d)

end