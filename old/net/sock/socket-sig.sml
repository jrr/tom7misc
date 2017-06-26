(* socket.sig
 * portions COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature SOCKET =
sig

  (* sockets are polymorphic; the instantiation of the type variables
   * provides a way to distinguish between different kinds of sockets.
   *)
  type ('af, 'sock) sock = ('af, 'sock) Socket.sock
  type 'af sock_addr = 'af Socket.sock_addr
    
  (* witness types for the socket parameter *)
  type dgram
  type 'a stream
  type passive          (* for passive streams *)
  type active           (* for active (connected) streams *)
    
  (* address families *)
  structure AF : 
    sig
      type addr_family = NetHostDB.addr_family
      val list : unit -> (string * addr_family) list
      (* list known address families *)
      val toString : addr_family -> string
      val fromString : string -> addr_family option
    end
  
  (* socket types *)
  structure SOCK : 
    sig
      eqtype sock_type
      val stream : sock_type            (* stream sockets *)
      val dgram : sock_type             (* datagram sockets *)
      val list : unit -> (string * sock_type) list
      (* list known socket types *)
      val toString : sock_type -> string
      val fromString : string -> sock_type option
    end

  (* socket control operations *)
  structure Ctl : 
    sig
      
      (* get/set socket options *)
      val getDEBUG              : ('a, 'b) sock -> bool
      val setDEBUG              : (('a, 'b) sock * bool) -> unit
      val getREUSEADDR          : ('a, 'b) sock -> bool
      val setREUSEADDR          : (('a, 'b) sock * bool) -> unit
      val getKEEPALIVE          : ('a, 'b) sock -> bool
      val setKEEPALIVE          : (('a, 'b) sock * bool) -> unit
      val getDONTROUTE          : ('a, 'b) sock -> bool
      val setDONTROUTE          : (('a, 'b) sock * bool) -> unit
      val getLINGER             : ('a, 'b) sock -> Time.time option
      val setLINGER             : (('a, 'b) sock * Time.time option) -> unit
      val getBROADCAST          : ('a, 'b) sock -> bool
      val setBROADCAST          : (('a, 'b) sock * bool) -> unit
      val getOOBINLINE          : ('a, 'b) sock -> bool
      val setOOBINLINE          : (('a, 'b) sock * bool) -> unit
      val getSNDBUF             : ('a, 'b) sock -> int
      val setSNDBUF             : (('a, 'b) sock * int) -> unit
      val getRCVBUF             : ('a, 'b) sock -> int
      val setRCVBUF             : (('a, 'b) sock * int) -> unit
      val getTYPE               : ('a, 'b) sock -> SOCK.sock_type
      val getERROR              : ('a, 'b) sock -> bool
        
      val getPeerName           : ('a, 'b) sock -> 'a sock_addr
      val getSockName           : ('a, 'b) sock -> 'a sock_addr
      val setNBIO               : (('a, 'b) sock * bool) -> unit
      val getNREAD              : ('a, 'b) sock -> int
      val getATMARK             : ('a, active stream) sock -> bool
    end (* Ctl *)

  (* socket address operations *)
  val sameAddr     : ('a sock_addr * 'a sock_addr) -> bool
  val familyOfAddr : 'a sock_addr -> AF.addr_family
    
  (* socket management *)
  val accept  : ('a, passive stream) sock
                 -> (('a, active stream) sock * 'a sock_addr)
  val bind    : (('a, 'b) sock * 'a sock_addr) -> unit
  val connect : (('a, 'b) sock * 'a sock_addr) -> unit
  val listen  : (('a, passive stream) sock * int) -> unit
  val close   : ('a, 'b) sock -> unit

  datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
  val shutdown : (('a, 'b stream) sock * shutdown_mode) -> unit
    
  val pollDesc : ('a, 'b) sock -> OS.IO.poll_desc
    
  (* Sock I/O option types *)
  type out_flags = {don't_route : bool, oob : bool}
  type in_flags = {peek : bool, oob : bool}

  type 'a buf = {buf : 'a, i : int, sz : int option}
    
  (* Sock output operations *)

  (* sendvec and friends might not send all of the data; they return
     an integer revealing how many bytes were actually sent -- see
     SockUtil for versions that guarantee to send the whole thing. *)
  val sendVec      : (('a, active stream) sock * Word8Vector.vector buf)
                        -> int
  val sendArr      : (('a, active stream) sock * Word8Array.array buf)
                        -> int
  val sendVec'   : (('a, active stream) sock * 
                    Word8Vector.vector buf * out_flags) -> int
  val sendArr'   : (('a, active stream) sock * 
                    Word8Array.array buf * out_flags) -> int
  val sendVecTo  : (('a, dgram) sock * 
                    'a sock_addr * Word8Vector.vector buf) -> int
  val sendArrTo  : (('a, dgram) sock * 'a sock_addr * 
                    Word8Array.array buf) -> int
  val sendVecTo' : (('a, dgram) sock * 'a sock_addr * 
                    Word8Vector.vector buf * out_flags) -> int
  val sendArrTo' : (('a, dgram) sock * 'a sock_addr * 
                    Word8Array.array buf * out_flags) -> int
    
  (* Sock input operations *)
  val recvVec      : (('a, active stream) sock * int) -> Word8Vector.vector
  val recvArr        : (('a, active stream) sock * 
                        Word8Array.array buf) -> int
  val recvVec'     : (('a, active stream) sock * 
                      int * in_flags) -> Word8Vector.vector
  val recvArr'     : (('a, active stream) sock * 
                      Word8Array.array buf * in_flags) -> int
  val recvVecFrom  : (('a, dgram) sock * int) -> (Word8Vector.vector * 
                                                  'b sock_addr)
  val recvArrFrom  : (('a, dgram) sock * 
                      {buf : Word8Array.array, i : int}) -> (int * 
                                                             'a sock_addr)
  val recvVecFrom' : (('a, dgram) sock * 
                      int * in_flags) -> (Word8Vector.vector * 'b sock_addr)

  val recvArrFrom' : (('a, dgram) sock * 
                      {buf : Word8Array.array, i : int} * in_flags)
                           -> (int * 'a sock_addr)

  (* select and friends *)
  type sock_desc
      
  val sockDesc : ('a, 'b) sock -> sock_desc
  val sameDesc : sock_desc * sock_desc -> bool
  val compare  : sock_desc * sock_desc -> order
  val select :
      { rds : sock_desc list, wrs : sock_desc list, exs : sock_desc list, 
	timeout : Time.time option } 
      -> { rds : sock_desc list, wrs : sock_desc list, exs : sock_desc list }

  (* extensions to the basis *)

  structure Ext :
  sig
    (* GenericSock.socket etc. will call these. *)

    (* create sockets using the default protocol *)
    val socket : (AF.addr_family * SOCK.sock_type)
          -> ('a, 'b) sock
    val socketPair : (AF.addr_family * SOCK.sock_type)
          -> (('a, 'b) sock * ('a, 'b) sock)

    (* create sockets using the specified protocol *)
    val socket' : (AF.addr_family * SOCK.sock_type * int)
          -> ('a, 'b) sock
    val socketPair' : (AF.addr_family * SOCK.sock_type * int)
          -> (('a, 'b) sock * ('a, 'b) sock)

    type inet

    (* the inet address family *)
    val inetAF   : AF.addr_family

    (* really to inet sock_addr *)
    val any      : int -> inet sock_addr

    val toAddr   : (NetHostDB.in_addr * int) -> inet sock_addr
    val fromAddr : inet sock_addr -> (NetHostDB.in_addr * int)

  end

  exception SockError of string

end

