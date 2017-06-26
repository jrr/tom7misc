
(* fake implementation of SockUtil for SML/NJ 
   Actually, many of these really could be written
   in real Standard ML; maybe that would be a good idea.
*)

structure SockUtil :> SOCKUTIL =
struct

  exception Unimplemented
  exception SockUtil of string

  fun ipstring _ = raise Unimplemented

  fun getipbyhost _ = raise Unimplemented

  fun sendfile _ = raise Unimplemented

  fun sendfilefilt _ = raise Unimplemented

  fun getpeername _ = raise Unimplemented

  datatype iostream = 
        IN of TextIO.instream
      | OUT of TextIO.outstream
    
  fun select _ = raise Unimplemented

end
