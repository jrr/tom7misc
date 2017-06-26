
signature OCTHASH =
sig

    type table
    type nodeid

    type item

    val new = int -> table
    val insert = (table * item) -> unit
	
	

end