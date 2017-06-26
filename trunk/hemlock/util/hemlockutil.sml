structure HemlockUtil =
struct
    infixr 9 `
    fun a ` b = a b

    val itos = Int.toString

    local val ctr = ref 0
    in
        fun newstring sep s = 
            let in
                ctr := (!ctr + 1);
                s ^ sep ^ itos ` !ctr
            end

        val newstr = newstring "$"
    end

end

structure StringMap = 
     SplayMapFn(type ord_key = string val compare = String.compare)
structure StringMapUtil = MapUtil(structure M = StringMap)
structure IntMap = SplayMapFn(type ord_key = int val compare = Int.compare)
structure IntMapUtil = MapUtil(structure M = IntMap)
