
(* Rewrite this file in your favorite SML implementation.
   I've tried to simplify it as much as possible; if your
   SML compiler has an interface to C, following the same
   basic code as I have here should make porting this easy.
*)


(* XXX: needs cleanup, bad.
   XXX: needs signature. *)

structure SockPrim =
struct

    (* XXX *)
  exception SockPrim of string
  val Unimplemented = SockPrim "unimplemented"

  datatype ('a, 'b) sum = INL of 'a | INR of 'b

  val sys_errno = Unsafe.cast "ml_errno" : unit -> int;
  val sys_strerror = Unsafe.cast "strerror" : int -> string;

  fun errstr s =
    let val err = sys_errno()
    in (s ^ "(" ^ Int.toString err ^ "): " ^ sys_strerror err)
    end
  
  fun getpeername w =
    let
      val rt = ref 0w0
      val sys_getpeername = Unsafe.cast "ml_getpeer" : word * word ref -> bool ;
    in
      if sys_getpeername (w, rt) then
        !rt
      else raise SockPrim "ml_getpeer failed"
    end
  
  fun selectprim (ins, outs, exs, to) =
    let
      (* automatically zeroed *)
      val sys_alloc_fdset = Unsafe.cast "ml_alloc_fdset" : unit -> SysWord.word ;
      val sys_add_fdset = Unsafe.cast "ml_add_fdset" : SysWord.word * SysWord.word -> unit ;
      val sys_select = Unsafe.cast "ml_select" : SysWord.word * SysWord.word * SysWord.word * 
	  SysWord.word * int * int -> int ;
      val sys_select_null = Unsafe.cast "select" : SysWord.word * SysWord.word * SysWord.word * 
	  SysWord.word * SysWord.word -> int ;
      val sys_check_fdset = Unsafe.cast "ml_check_fdset" : SysWord.word * SysWord.word -> bool ;
      val sys_dealloc_fdset = Unsafe.cast "free" : SysWord.word -> unit ;
        
      val inset  = sys_alloc_fdset ()
      val outset = sys_alloc_fdset ()
      val exnset = sys_alloc_fdset ()
        
      val _ = app (fn r => sys_add_fdset (r, inset)) ins
      val _ = app (fn w => sys_add_fdset (w, outset)) outs
      val _ = app (fn x => sys_add_fdset (x, exnset)) exs

      fun highest ll =
        let
          fun shst x nil = x
            | shst x (h::t) = shst (if x > h then x else h) t
          fun hst x nil = x
            | hst x (h::t) = hst (shst x h) t
        in
          hst (0w0 : SysWord.word) ll
        end
      
      val n = 0w1 + highest [ins, outs, exs]
        
      fun settolist s =
        let
          fun f m = 
            if m > n then nil
            else if sys_check_fdset (m, s) then m :: f (m + 0w1)
                 else f (m + 0w1)
        in
          f (0w0 : SysWord.word)
        end

      val rv = 
	  case to of
	      NONE => sys_select_null (n, inset, outset, exnset, 0w0)
	    | SOME t => 
		  (case Time.toSeconds t of
		       0 => sys_select (n, inset, outset, exnset, 0,  LargeInt.toInt (Time.toMicroseconds t mod 1000000))
		     | m => sys_select (n, inset, outset, exnset, LargeInt.toInt m, 0))

      val ret = (settolist inset,
                 settolist outset,
                 settolist exnset)
    in
      sys_dealloc_fdset inset;
      sys_dealloc_fdset outset;
      sys_dealloc_fdset exnset;
      
      case rv of
	  ~1 => INL (errstr "select")
	| _ => INR ret
    end

  val sys_socket = Unsafe.cast "socket" : int * int * int -> int ;
  val sys_socketpair = Unsafe.cast "socketpair" 
              : int * int * int * int Array.array -> int ;
  
  val st_stream = Unsafe.cast "ML_SOCK_STREAM" : int;
  val st_dgram  = Unsafe.cast "ML_SOCK_DGRAM" : int;

  val af_unix   = Unsafe.cast "ML_AF_UNIX" : int;
  val af_inet   = Unsafe.cast "ML_AF_INET" : int;

(* XXX I don't know why this doesn't work,
   though I believe it's a safe assumption that INADDR_ANY is 0 *)
(*  val inaddr_any = Unsafe.cast "INADDR_ANY" : word; *)
  val inaddr_any = Unsafe.cast "0" : SysWord.word; 

(*  val sockaddr_size = Unsafe.cast "sizeof (struct sockaddr)" : int; *)
    
  fun bind_inet (s, addr, p) =
    let
      val sys_bind_inet = Unsafe.cast "ml_bind_inet" 
          : int * SysWord.word * int -> int ;
    in
      case sys_bind_inet (s, addr, p) of
        0 => NONE
      | _ => SOME (errstr "bind_inet")
    end

  fun listen (s, log) =
    let
      val sys_listen = Unsafe.cast "listen" : int * int -> int ;
    in
      case sys_listen (s, log) of
        0 => NONE
      | _ => SOME (errstr "listen")
    end

  fun accept s =
    let
      val sys_accept = Unsafe.cast "ml_accept" 
              : int * SysWord.word ref * int ref -> int ;
      val addr = ref (0w0 : SysWord.word)
      val port = ref 0
    in
      case sys_accept (s, addr, port) of
        ~1 => INL (errstr "accept")
      | n => INR (n, (!addr, !port))
    end

  val msg_oob = Unsafe.cast "ML_MSG_OOB" : SysWord.word;
  val msg_dontroute = Unsafe.cast "ML_MSG_DONTROUTE" : SysWord.word;
  val msg_nosignal = Unsafe.cast "ML_MSG_NOSIGNAL" : SysWord.word;
  val msg_peek = Unsafe.cast "ML_MSG_PEEK" : SysWord.word;

  fun fs true x = x
    | fs false _ = 0w0 : SysWord.word

  fun sendv (sock, vec, start, len, route, oob) =
    let
      val sys_send = Unsafe.cast "ml_send" 
            : int * Word8Vector.vector * int * int * SysWord.word -> int;

      (* always sends nosignal *)
      val flags = foldl SysWord.orb msg_nosignal [fs route msg_dontroute,
                                                  fs oob msg_oob]
    in
      case sys_send (sock, vec, start, len, flags) of
        ~1 => INL (errstr "sendv")
      | n => INR n
    end

  fun senda (sock, vec, start, len, route, oob) =
    let
      val sys_send = Unsafe.cast "ml_send" 
            : int * Word8Array.array * int * int * SysWord.word -> int;

      (* always sends nosignal *)
      val flags = foldl SysWord.orb msg_nosignal [fs route msg_dontroute,
                                                  fs oob msg_oob]
    in
      case sys_send (sock, vec, start, len, flags) of
        ~1 => INL (errstr "senda")
      | n => INR n
    end

  fun recv (sock, arr, start, len, peek, oob) =
    let
      val sys_recv = Unsafe.cast "ml_recv"
            : int * Word8Array.array * int * int * SysWord.word -> int;

      val flags = foldl SysWord.orb msg_nosignal [fs peek msg_peek,
                                                  fs oob msg_oob]
    in
      case sys_recv (sock, arr, start, len, flags) of
        ~1 => INL (errstr "recv")
      | n => INR n
    end


  fun sendtoa (sock, arr, start, len, addr, port, route, oob) =
    let
	val sys_sendto = Unsafe.cast "ml_sendto"
	    : int * Word8Array.array * int * int * SysWord.word * int * SysWord.word -> int;

	(* always sends nosignal *)
	val flags = foldl SysWord.orb msg_nosignal [fs route msg_dontroute,
						    fs oob msg_oob]
    in
	case sys_sendto (sock, arr, start, len, addr, port, flags) of
	    ~1 => INL (errstr "sendtoa")
	  | n => INR n
    end

  fun sendtov (sock, arr, start, len, addr, port, route, oob) =
    let
	val sys_sendto = Unsafe.cast "ml_sendto"
	    : int * Word8Vector.vector * int * int * SysWord.word * int * SysWord.word -> int;

	(* always sends nosignal *)
	val flags = foldl SysWord.orb msg_nosignal [fs route msg_dontroute,
						    fs oob msg_oob]
    in
	case sys_sendto (sock, arr, start, len, addr, port, flags) of
	    ~1 => INL (errstr "sendtoa")
	  | n => INR n
    end

  fun recvfrom (sock, arr, start, len, peek, oob) =
      let
	  val sys_recvfrom = Unsafe.cast "ml_recvfrom"
	      : int * Word8Array.array * int * int * SysWord.word * SysWord.word ref * int ref -> int;

	  val flags = foldl SysWord.orb msg_nosignal [fs peek msg_peek,
						      fs oob msg_oob]

	  val from_addr = ref 0w0
	  val from_port = ref 0
      in
	  case sys_recvfrom (sock, arr, start, len, flags, from_addr, from_port) of
	      ~1 => INL (errstr "recvfrom")
	    | n => INR (n, !from_addr, !from_port)
      end

end
