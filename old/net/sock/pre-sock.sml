(* pre-sock.sml
 * Portions COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(* Modified for MLton by Tom 7 *)

structure PreSock =
struct

  (* only these address families are supported now *)
  datatype af = AF_UNIX | AF_INET
    
  (* the raw representation of a socket (a file descriptor for now) *)
  type socket = int
    
  (* an internet address; this is here because it is abstract in the
   * NetHostDB and IP structures.
   * 
   * We know the length of an in_addr is 4 bytes.
   *)
  type in_addr = SysWord.word
    
  (* an address family *)
  type addr_family = af
    
  (* socket types *)
  datatype sock_type = ST_STREAM | ST_DGRAM
    
  (* sockets are polymorphic; the instantiation of the type variables
     provides a way to distinguish between different kinds of sockets.
     (though the underlying representation is the same) *)
  datatype ('sock, 'af) sock = SOCK of socket

  (* if a sock_addr is INADDR, then it has an address and a port. *)
  (* a unix addr is a string. *)
  datatype 'af sock_addr = 
    INADDR of in_addr * int
  | UNADDR of string

  (** Utility functions for parsing/unparsing network addresses **)
  local

    structure SysW = SysWord
    structure SCvt = StringCvt

    fun toW (getc, strm) = 
      let
        fun scan radix strm = 
          case (SysW.scan radix getc strm)
            of NONE => NONE
          | (SOME(w, strm)) => SOME(w, strm)
      in
        case (getc strm)
          of NONE => NONE
        | (SOME(#"0", strm')) => 
            (case (getc strm')
               of NONE => SOME(0w0, strm')
             | (SOME (#"x", strm'')) => scan SCvt.HEX strm''
             | (SOME (#"X", strm'')) => scan SCvt.HEX strm''
             | _ => scan SCvt.OCT strm)
        | _ => scan SCvt.DEC strm
      end

    fun wx hi low = SysW.orb(SysW.<<(hi, 0w16), low)
    
  (* check that the word is representable in the given number of bits; raise
   * Overflow if not.
   *)
    fun chk (w, bits) =
      if (SysW.>= (SysW.>>(wx 0wxffff 0wxffff, Word.-(0w32, bits)), w))
        then w
      else raise General.Overflow

    (* Scan a sequence of numbers separated by #"." *)
    fun scan getc strm = 
      case toW (getc, strm)
        of NONE => NONE
      | SOME(w, strm') => scanRest getc ([w], strm')

    and scanRest getc (l, strm) = 
      case getc strm
        of SOME(#".", strm') => 
          (case toW (getc, strm')
             of NONE => SOME(List.rev l, strm)
           | SOME(w, strm'') => scanRest getc (w::l, strm''))
      | _ => SOME(List.rev l, strm)

  in

    fun toWords (getc : (char, 'a) StringCvt.reader) 
                (strm : 'a)
                : (SysW.word list * 'a) option = 
      case (scan getc strm)
        of SOME([a, b, c, d], strm) => 
          SOME([chk(a, 0w8), chk(b, 0w8), chk(c, 0w8), chk(d, 0w8)], strm)
      | SOME([a, b, c], strm) =>
          SOME([chk(a, 0w8), chk(b, 0w8), chk(c, 0w16)], strm)
      | SOME([a, b], strm) =>
          SOME([chk(a, 0w8), chk(b, 0w24)], strm)
      | SOME([a], strm) =>
          SOME([chk(a, 0w32)], strm)
      | _ => NONE

    fun fromBytes (a : Word8.word, 
                   b : Word8.word, 
                   c : Word8.word, 
                   d : Word8.word) : string = 
      let
        val fmt = Word8.fmt StringCvt.DEC
      in
        concat [fmt a, ".", fmt b, ".", fmt c, ".", fmt d]
      end

  end

end

(* We alias this structure to Socket so that the signature files will
  compile. We also need to keep the PreSock structure visible, so that
  structures compiled after the real Sock structure still have access
  to the representation types. *) 
structure Socket = PreSock;
