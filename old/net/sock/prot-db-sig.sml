(* prot-db.sig
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(* Interface to the protocol database (think /etc/protocols on
   UNIX). Ported to MLton by Tom 7 *)                                       

signature NET_PROT_DB =
sig
    (* abstract type of protocol entries *)
    type entry

    (* get its name, ie 'tcp' *)
    val name : entry -> string

    (* get aliases (?) *)
    val aliases : entry -> string list

    (* get the protocol number *)
    val protocol : entry -> int

    (* look up protocols by name ('tcp') or number ('6') *)
    val getByName   : string -> entry option
    val getByNumber : int -> entry option
end
