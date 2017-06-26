
structure Hsock :> HSOCK =
struct

  type address = unit
    
  type connection = unit

  exception Error

  fun address x = raise Error

  fun connect a p = raise Error

  fun send (c : connection) s = raise Error

  fun ipall s = raise Error

  fun recvall _  = raise Error
    
  fun close _ = raise Error

end
