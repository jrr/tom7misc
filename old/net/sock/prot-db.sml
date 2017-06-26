(* prot-db.sml
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

structure NetProtDB : NET_PROT_DB =
struct

  exception Unimplemented
    
  datatype entry = 
    PROTOENT of {name : string,
                 aliases : string list,
                 protocol : int }
    
  local
    fun conc field (PROTOENT a) = field a
  in
    val name = conc #name
    val aliases = conc #aliases
    val protocol = conc #protocol
  end

  (* FIXME implement these (read /etc/protocols) ... *)
  fun getByName _ = raise Unimplemented
  fun getByNumber _ = raise Unimplemented
    

end

