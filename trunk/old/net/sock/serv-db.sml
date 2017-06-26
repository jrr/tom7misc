(* serv-db.sml
 * portions COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(* Modified and ported to MLton by Tom 7 *)

structure NetServDB :> NET_SERV_DB =
struct

  exception Unimplemented

  datatype entry = 
    SERVENT of {name : string,
                aliases : string list,
                port : int,
                protocol : string}
    
  local
    fun conc field (SERVENT a) = field a
  in
    val name = conc #name
    val aliases = conc #aliases
    val port = conc #port
    val protocol = conc #protocol
  end


  fun getByName _ = raise Unimplemented
  fun getByPort _ = raise Unimplemented

end

