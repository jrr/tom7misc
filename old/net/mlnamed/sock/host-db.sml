
(* Portions Copyright AT&T Bell Laboratories *)

structure NetHostDB :> NET_HOST_DB =
struct

  exception Unimplemented

  structure SysW = SysWord

  type in_addr = Socket.in_addr
  type addr_family = Socket.addr_family

  datatype entry = 
    HOSTENT of 
    {name : string,
     aliases : string list,
     addrType : addr_family,
     (* invt: non empty *)
     addrs : in_addr list}

  local
    fun conc field (HOSTENT a) = field a
  in
    val name = conc #name
    val aliases = conc #aliases
    val addrType = conc #addrType
    val addrs = conc #addrs
    val addr = hd o addrs
  end

  fun getByName _ = raise Unimplemented
  fun getByAddr addr = raise Unimplemented

  structure W = SysWord
    
  fun scan getc strm = 
    let

      fun w2b w = Word8.fromInt(W.toInt w)
      fun getB (w, shft) = W.andb(W.>>(w, shft), 0wxFF)
      fun mkAddr (a, b, c, d) = 
        foldl W.orb 0w0 [W.<<(a, 0w24),
                         W.<<(b, 0w16),
                         W.<<(c, 0w8),
                         d]
    in
      case (Socket.toWords getc strm)
        of SOME([a, b, c, d], strm) =>
          SOME(mkAddr(a, b, c, d), strm)
      | SOME([a, b, c], strm) =>
          SOME(mkAddr(a, b, getB(c, 0w8), getB(c, 0w0)), strm)
      | SOME([a, b], strm) =>
          SOME(mkAddr(a, getB(b, 0w16), getB(b, 0w8), getB(b, 0w0)), strm)
      | SOME([a], strm) =>
          SOME(mkAddr(getB(a, 0w24), 
                      getB(a, 0w16), 
                      getB(a, 0w8), 
                      getB(a, 0w0)), 
               strm)
      | _ => NONE
    end
  
  val fromString = StringCvt.scanString scan
    
  fun toString addr =
    let
      fun get i = 
        Word8.fromInt 
        (SysWord.toInt 
         (SysWord.andb (0wxFF, SysWord.>>(addr, i))))
    in
      Socket.fromBytes(get 0w24, get 0w16, get 0w8, get 0w0)
    end
  
  fun getHostName () = raise Unimplemented

end
