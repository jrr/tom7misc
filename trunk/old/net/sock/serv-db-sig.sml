(* serv-db.sig
 * portions COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(* Modified and ported to MLton by Tom 7 *)

signature NET_SERV_DB =
sig

  (* abstract type of services *)
  type entry
    
  (* retrieve name ('ftp'), aliases, port ('21'), or
     protocol ('tcp') from an entry *)
  val name : entry -> string
  val aliases : entry -> string list
  val port : entry -> int
  val protocol : entry -> string
  
  (* lookup by name (ie 'ftp') or number (ie '21') *)
  val getByName : (string  * string option) -> entry option
  val getByPort : (int  * string option) -> entry option

end

