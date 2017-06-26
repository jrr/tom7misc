signature STREAM =
sig

    type 'a stream

    exception Empty

    val empty : 'a stream

    val cons : 'a * 'a stream -> 'a stream
    val lcons : 'a * (unit -> 'a stream) -> 'a stream
    val delay : (unit -> 'a stream) -> 'a stream

    val is_empty : 'a stream -> bool

    val uncons : 'a stream -> 'a * 'a stream
    val hd : 'a stream -> 'a
    val tl : 'a stream -> 'a stream
    val ltl : 'a stream -> 'a stream

    val map : ('a -> 'b) -> ('a stream -> 'b stream)
    val app : ('a -> unit) -> ('a stream -> unit)

    val append : 'a stream -> 'a stream -> 'a stream

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b

    val toList : 'a stream -> 'a list

end
