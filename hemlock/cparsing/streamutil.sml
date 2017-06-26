
structure StreamUtil :> STREAMUTIL =
struct

    (* converts a string to a char stream *)
    (* XXX doesn't need to make list; inefficient *)
    fun stostream s =
	foldr Stream.cons Stream.empty (String.explode s)

    (* convert a file to a char stream *)
    fun ftostream f =
	let
	    val ff = TextIO.openIn f

	    fun rd () =
		case TextIO.input1 ff of
		    NONE => (TextIO.closeIn ff; 
			     Stream.empty)
		  | SOME c => Stream.lcons(c, rd)
	in
	    Stream.delay rd
	end

end