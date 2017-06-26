(* generic-sock.sig
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(* MLton implementation by Tom 7 *)

signature GENERIC_SOCK =
sig

  (* returns supported address families,
     including at least UnixSocket.AF.inet *)
  val addressFamilies : unit -> UnixSocket.AF.addr_family list
    
  (* returns supported socket types,
     including at least UnixSocket.SOCK.stream and UnixSocket.SOCK.dgram *)
  val socketTypes : unit -> UnixSocket.SOCK.sock_type

  (* create sockets using default protocol *)
  val socket : (UnixSocket.AF.addr_family * UnixSocket.SOCK.sock_type)
    -> ('a, 'b) UnixSocket.sock
  val socketPair : (UnixSocket.AF.addr_family * UnixSocket.SOCK.sock_type)
    -> (('a, 'b) UnixSocket.sock * ('a, 'b) UnixSocket.sock)
    
  (* create sockets using the specified protocol *)
  val socket' : (UnixSocket.AF.addr_family * UnixSocket.SOCK.sock_type * int)
    -> ('a, 'b) UnixSocket.sock
  val socketPair' : (UnixSocket.AF.addr_family * UnixSocket.SOCK.sock_type * int)
    -> (('a, 'b) UnixSocket.sock * ('a, 'b) UnixSocket.sock)

end

