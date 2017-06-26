
structure Loc :>
    sig
	type loc
	val compare : loc * loc -> order
	val newloc : unit -> loc
	val tostring : loc -> string
    end =
struct

    type loc = int
    val ctr = ref 0
    fun newloc () = (ctr := !ctr + 1; !ctr)
    val compare = Int.compare
    fun tostring l = "l" ^ Int.toString l
end