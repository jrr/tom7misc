
structure GenericSock :> GENERIC_SOCK =
struct

  exception Unimplemented

  (* I think these have buggy specifications. They are
     unimplemented in SML/NJ, too, so I'm gonna punt. *)
  fun addressFamilies () = raise Unimplemented
  fun socketTypes () = raise Unimplemented
    
  val socket = UnixSocket.Ext.socket
  val socketPair = UnixSocket.Ext.socketPair

  val socket' = UnixSocket.Ext.socket'
  val socketPair' = UnixSocket.Ext.socketPair'

end
