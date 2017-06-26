(* inet-sock.sml
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(* The implementation of this structure is mostly in
   unix-socket.sml *)

structure INetSock : INET_SOCK =
struct

  exception Unimplemented

  structure SOCK = UnixSocket.SOCK
    
  type inet = UnixSocket.Ext.inet

  type 'a sock = (inet, 'a) UnixSocket.sock
  type 'a stream_sock = 'a UnixSocket.stream sock
  type dgram_sock = UnixSocket.dgram sock
    
  type sock_addr = inet UnixSocket.sock_addr
    
  val inetAF = Option.valOf(UnixSocket.AF.fromString "INET")
    
  val toAddr = UnixSocket.Ext.toAddr
  val fromAddr = UnixSocket.Ext.fromAddr
  val any = UnixSocket.Ext.any

  structure UDP =
    struct
      fun socket () = GenericSock.socket (inetAF, SOCK.dgram)
      fun socket' proto = GenericSock.socket' (inetAF, SOCK.dgram, proto)
    end
  
  structure TCP =
    struct
      fun socket () = GenericSock.socket (inetAF, SOCK.stream)
      fun socket' proto = GenericSock.socket' (inetAF, SOCK.stream, proto)

      fun getNODELAY (PreSock.SOCK fd) = raise Unimplemented
      fun setNODELAY (PreSock.SOCK fd, flg) = raise Unimplemented
    end
end

