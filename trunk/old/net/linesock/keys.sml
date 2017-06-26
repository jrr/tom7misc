
structure IntKey :> KEY where type key = int =
struct

    type key = int
    val compare = Int.compare

end

structure StringKey :> KEY where type key = string =
struct

    type key = string
    val compare = String.compare

end