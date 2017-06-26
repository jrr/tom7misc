
datatype t = An of t -> unit

fun eater (An f) = f (An f)

val looper = eater (An eater)
