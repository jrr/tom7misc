
signature LINESOCK =
sig
    
    type key
    type 'a lsock
	
    val lsock : string * ('a, UnixSocket.active UnixSocket.stream) UnixSocket.sock * key -> 'a lsock

    val getkey : 'a lsock -> key


(* need to think over sharing semantics before allowing this *)
(*    val sock : 'a lsock ->  string * ('a, active stream) sock *)

    datatype 'a event =
	Closed of key
      | Line of key * string
      | Accept of ('a, UnixSocket.active UnixSocket.stream) UnixSocket.sock * 'a UnixSocket.sock_addr
      | Timeout

    (* wait server lsocks timeout *)
    val wait :
	(('a, UnixSocket.passive UnixSocket.stream) UnixSocket.sock option *
	 'a lsock list * 
	 (int * int) option) -> 'a event

end