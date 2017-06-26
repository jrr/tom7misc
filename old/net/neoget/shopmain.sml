
structure Main =
struct

    val _ =
	case Params.docommandline () of
	    [cookie, userid, name, id, price, pricemine, limit] =>
		(case map Int.fromString [id, price, pricemine, limit] of
		    [SOME id, SOME price, SOME pricemine, SOME limit] => Shop.buythresh cookie userid name id price pricemine limit
		  | _ => print "usage: shopping cookie userid itemname itemid price limit\n")
	  | _ => print "usage: shopping cookie userid itemname itemid price limit\n"

end