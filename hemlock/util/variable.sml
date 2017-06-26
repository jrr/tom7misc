
structure Variable :> VARIABLE =
struct

    type var = int * string * (unit -> string)

    val arena = StringOnce.arenaex "_"

    fun namedvar s =
        let val (i, f) = StringOnce.symbol arena s
        in (i, s, f)
        end

    fun newvar () = namedvar "vv"

    fun basename (_, s, _) = s

    fun alphavary v = namedvar (basename v)

    fun eq ((n1, _, _), (n2, _, _)) = n1 = n2
        
    fun compare ((n1, _, _), (n2, _, _)) = Int.compare (n1, n2)
        
    fun tostring (_, _, f) = f ()
        
    structure Map = SplayMapFn (struct
                                    type ord_key = var
                                    val compare = compare
                                end)
end
