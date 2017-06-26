(* unix-socket.sml
 * portions COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(* Mostly rewritten by Tom 7 for MLton *)

structure UnixSocket :> SOCKET =
struct

  exception Unimplemented
  and       Impossible
  and       SockError of string

  structure W8A = Word8Array
  structure W8V = Word8Vector
    
  type w8vector = W8V.vector
  type w8array = W8A.array
    
  (* the system's representation of a socket *)
  type sockFD = PreSock.socket
    
  (* to inherit the various socket related types *)
  open PreSock
    
  (* witness types for the socket parameter *)
  datatype dgram = DGRAM
  datatype 'a stream = STREAM
  datatype passive = PASSIVE
  datatype active = ACTIVE
    
  (* address families *)
  structure AF =
  struct
    type addr_family = NetHostDB.addr_family
    fun toString AF_UNIX = "UNIX"
      | toString AF_INET = "INET"
    fun list () = map (fn a => (toString a, a)) [AF_UNIX, AF_INET]
    fun fromString "UNIX" = SOME AF_UNIX
      | fromString "INET" = SOME AF_INET
      | fromString _ = NONE
  end

  (* socket types. Perhaps some day
     this will be expanded to include other
     socket types, but what else is there? *)
  structure SOCK =
  struct
    type sock_type = PreSock.sock_type
    val stream = ST_STREAM
    val dgram  = ST_DGRAM
    fun list () = [ ("STREAM", ST_STREAM),
                    ("DGRAM",  ST_DGRAM) ]
    fun toString ST_STREAM = "STREAM"
      | toString ST_DGRAM  = "DGRAM"

    fun fromString "STREAM" = SOME ST_STREAM
      | fromString "DGRAM"  = SOME ST_DGRAM
      | fromString _        = NONE
  end

  (* socket control operations *)
  structure Ctl =
  struct

    fun getDEBUG _ = raise Unimplemented
    fun setDEBUG _ = raise Unimplemented
    fun getREUSEADDR _ = raise Unimplemented
    fun setREUSEADDR _ = raise Unimplemented
    fun getKEEPALIVE _ = raise Unimplemented
    fun setKEEPALIVE _ = raise Unimplemented
    fun getDONTROUTE _ = raise Unimplemented
    fun setDONTROUTE _ = raise Unimplemented
    fun getLINGER _ = raise Unimplemented
    fun setLINGER _ = raise Unimplemented
    fun getBROADCAST _ = raise Unimplemented
    fun setBROADCAST _ = raise Unimplemented
    fun getOOBINLINE _ = raise Unimplemented
    fun setOOBINLINE _ = raise Unimplemented
    fun getSNDBUF _ = raise Unimplemented
    fun setSNDBUF _ = raise Unimplemented
    fun getRCVBUF _ = raise Unimplemented
    fun setRCVBUF _ = raise Unimplemented
    fun getTYPE _ = raise Unimplemented
    fun getERROR _ = raise Unimplemented
        
    fun getPeerName _ = raise Unimplemented
    fun getSockName _ = raise Unimplemented
    fun setNBIO _ = raise Unimplemented
    fun getNREAD _ = raise Unimplemented
    fun getATMARK _ = raise Unimplemented

  end
    
  (* note that the type is not ''a sock_addr... *)
  fun sameAddr (INADDR (a1,p1), INADDR (a2,p2)) = a1 = a2 andalso p1 = p2
    | sameAddr (UNADDR s1, UNADDR s2) = s1 = s2
    | sameAddr _ = false
  fun familyOfAddr (INADDR _) = AF_INET
    | familyOfAddr (UNADDR _) = AF_UNIX

  fun accept (SOCK sock) =
    case SockPrim.accept sock of
      SockPrim.INL err => raise SockError err
    | SockPrim.INR (newsock, addr) => (SOCK newsock, INADDR addr)

  fun bind (SOCK sock, INADDR (addr, port)) =
    (case SockPrim.bind_inet (sock, addr, port) of
       NONE => ()
     | SOME err => raise SockError err)
    | bind (SOCK sock, _) = raise Unimplemented

  fun connect (sock, addr) = raise Unimplemented

  fun listen (SOCK sock, backlog) =
    (case SockPrim.listen (sock, backlog) of
       NONE => ()
     | SOME err => raise SockError err)

  fun close sock = raise Unimplemented

  datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS

  fun shutdown (sock, smode) = raise Unimplemented

  fun pollDesc sock = raise Unimplemented

  type out_flags = {don't_route : bool, oob : bool}
  type in_flags  = {peek : bool, oob : bool}

  type 'a buf = {buf : 'a, i : int, sz : int option}

  fun chk (len, buf, i, NONE) =
    if ((i < 0) orelse (len < i))
      then raise Subscript
    else (buf, i, len - i)
    | chk (len, buf, i, SOME sz) =
      if ((i < 0) orelse (sz < 0) orelse (len-i < sz))
        then raise Subscript
      else (buf, i, sz)

  fun vbuf {buf, i, sz} = chk (W8V.length buf, buf, i, sz)
  fun abuf {buf, i, sz} = chk (W8A.length buf, buf, i, sz)

  fun sendVec' (SOCK s, bf, {don't_route, oob}) =
    let
      val (buf, i, sz) = vbuf bf
    in
      case SockPrim.sendv (s, buf, i, sz, don't_route, oob) of
        SockPrim.INL s => raise SockError s
      | SockPrim.INR n => n
    end
  
  fun sendArr' (SOCK s, bf, {don't_route, oob}) =
    let
      val (buf, i, sz) = abuf bf
    in
      case SockPrim.senda (s, buf, i, sz, don't_route, oob) of
        SockPrim.INL s => raise SockError s
      | SockPrim.INR n => n
    end
  
  val off_fl = {don't_route=false, oob=false}
    
  fun sendVec (s, buf) =
    sendVec' (s, buf, off_fl)
    
  fun sendArr (s, buf) =
    sendArr' (s, buf, off_fl)
    
  fun sendVecTo' (SOCK s, addr, buf, {don't_route, oob}) =
    let
	val (buf, i, sz) = vbuf buf
    in
	case addr of
	    INADDR (ad, port) =>
		(case SockPrim.sendtov (s, buf, i, sz, ad, port, don't_route, oob) of
		     SockPrim.INL err => raise SockError err
		   | SockPrim.INR n => n)
	  | _ => raise Unimplemented
    end
  
  fun sendArrTo' (SOCK s, addr, buf, {don't_route, oob}) =
    let
	val (buf, i, sz) = abuf buf
    in
	case addr of
	    INADDR (ad, port) =>
		(case SockPrim.sendtoa (s, buf, i, sz, ad, port, don't_route, oob) of
		     SockPrim.INL err => raise SockError err
		   | SockPrim.INR n => n)
	  | _ => raise Unimplemented
    end
  
  fun sendVecTo (s, addr, buf) =
    sendVecTo' (s, addr, buf, off_fl)
    
  fun sendArrTo (s, addr, buf) =
    sendArrTo' (s, addr, buf, off_fl)
    
    
  val iff_fl = {peek=false, oob=false}
    
  fun recvVec' (SOCK s, i, {peek, oob}) =
    let
      val arr = Word8Array.array (i, 0w0)
    in
      case SockPrim.recv (s, arr, 0, i, peek, oob) of
        SockPrim.INL err => raise SockError err
      | SockPrim.INR n => Word8Array.extract (arr, 0, SOME n)
    end
  
  fun recvArr' (SOCK s, b, {peek, oob}) =
    let 
	val (buf, i, sz) = abuf b
    in
	case SockPrim.recv (s, buf, i, sz, peek, oob) of
	    SockPrim.INL err => raise SockError err
	  | SockPrim.INR n => n
    end
  
  fun recvVec (s, i) =
    recvVec' (s, i, iff_fl)
    
  fun recvArr (s, b) =
    recvArr' (s, b, iff_fl)
    
  fun recvVecFrom' (SOCK s, i, {peek, oob}) =
    let
      val arr = Word8Array.array (i, 0w0)
    in
      case SockPrim.recvfrom (s, arr, 0, i, peek, oob) of
        SockPrim.INL err => raise SockError err
      | SockPrim.INR (n, addr, port) => (Word8Array.extract (arr, 0, SOME n), INADDR (addr, port))
    end
  
  fun recvArrFrom' (s, b, fl) =
    let in
      raise Unimplemented
    end
  
  fun recvVecFrom (s, len) =
    recvVecFrom' (s, len, iff_fl)
    
  fun recvArrFrom (s, b) =
    recvArrFrom' (s, b, iff_fl)


  (* select and friends *)
  type sock_desc = Word32.word
      
  fun sockDesc (SOCK s) = Word32.fromInt s

  val sameDesc = op=

  val compare = Word32.compare

  (*
    val select : 
    { rds : sock_desc list, wrs : sock_desc list, exs : sock_desc list, 
    timeout : Time.time option } 
       -> { rds : sock_desc list, wrs : sock_desc list, exs : sock_desc list }
       *)


  fun select {rds, wrs, exs, timeout} =
      let in
	  case SockPrim.selectprim (rds, wrs, exs, timeout) of
	      SockPrim.INL s => raise SockError s
	    | SockPrim.INR (rds, wrs, exs) => {rds=rds, wrs=wrs, exs=exs}
      end

  structure Ext =
  struct

    fun gettypenum ST_STREAM = SockPrim.st_stream
      | gettypenum ST_DGRAM  = SockPrim.st_dgram

    fun getafnum AF_INET = SockPrim.af_inet
      | getafnum AF_UNIX = SockPrim.af_unix

    fun socket' (af, st, prot) =
      let in
(*
        print ("socket (" ^ Int.toString (getafnum af) ^ ", " ^
               Int.toString (gettypenum st) ^ ", " ^
               Int.toString prot ^ ")\n"); *)
        case SockPrim.sys_socket (getafnum af, gettypenum st, prot) of
          ~1 => raise SockError "call to socket failed"
        | n  => ((* print ("ret: " ^ Int.toString n ^ "\n")  ; *) SOCK n)
      end

    fun socket(a, b) = socket'(a, b, 0)

    fun socketPair' (af, st, prot) =
      let
        val ss = Array.array (2, 0)
      in
        case SockPrim.sys_socketpair (getafnum af, gettypenum st, prot, ss) of
          0 => (SOCK (Array.sub(ss, 0)), 
                SOCK (Array.sub(ss, 1)))
        | _ => raise SockError "call to socketpair failed"
      end

    fun socketPair (a, b) = socketPair' (a, b, 0)

    datatype inet = INET__

    val inetAF = AF_INET

    fun any port = INADDR(SockPrim.inaddr_any, port)

    fun fromAddr (INADDR(a, p)) = (a, p)
      | fromAddr _ = raise Impossible

    fun toAddr (a, p) = INADDR(a, p)

  end

end

