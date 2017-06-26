
signature AUTH =
sig
    
    (* checks a username/password pair for validity.
       If valid, return some information,
       If not valid, return NONE *)

    val check : string * string ->
	        ({ uid : int,
		   home : string,
		   name : string
		   }, string) Util.sum

end
