
signature CONTEXT =
sig
    exception Absent

    type context

    val empty : context


    (* lookup operations *)
    
    (* resolve a value identifier in the current context, return its type and
       status (don't return its binding yet, since we don't need that yet) *)
    val var : context -> string -> IL.typ IL.poly * Variable.var * IL.idstatus

    (* resolve a type/con identifer in the current context, return its kind
       and binding *)
    val con : context -> string -> IL.kind * IL.con * IL.tystatus

    (* has_evar ctx n
       Does the context contain the free evar n in the type of any
       term? *)
    val has_evar : context -> int -> bool

    (* context extension operations *)

    (* bind an identifier to a variable and give that variable 
       the indicated type *)
    val bindv : context -> string -> IL.typ IL.poly -> Variable.var -> context

    (* as above, but specify special status *)
    val bindex : context -> string -> IL.typ IL.poly -> 
                  Variable.var -> IL.idstatus -> context

    (* bind an identifier to a constructor with the indicated kind *)
    val bindc : context -> string -> IL.con -> IL.kind -> IL.tystatus -> context

end
