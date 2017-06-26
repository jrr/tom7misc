(* inet-sock.sig
 * portions COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature INET_SOCK =
sig

    type inet

    type 'a sock = (inet, 'a) UnixSocket.sock
    type 'a stream_sock = 'a UnixSocket.stream sock
    type dgram_sock = UnixSocket.dgram sock

    type sock_addr = inet UnixSocket.sock_addr

    (* DARPA internet protocols *)
    val inetAF : UnixSocket.AF.addr_family

    val toAddr   : (NetHostDB.in_addr * int) -> sock_addr
    val fromAddr : sock_addr -> (NetHostDB.in_addr * int)
    val any      : int -> sock_addr

    structure UDP : 
      sig
	val socket  : unit -> dgram_sock
	val socket' : int -> dgram_sock
      end

    structure TCP : 
      sig
	val socket  : unit -> 'a stream_sock
	val socket' : int -> 'a stream_sock

        (* set sockets blocking/nonblocking *)
	val getNODELAY : 'a stream_sock -> bool
	val setNODELAY : ('a stream_sock * bool) -> unit
      end
end

