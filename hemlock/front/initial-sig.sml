
(* The initial context. *)

signature INITIAL =
sig

    val ilint : IL.typ
    val ilchar : IL.typ
    val ilstring : IL.typ

    val initial : Context.context

    (* wrap with declarations needed by the compiler 
       (bool, exceptions) *)
    val wrap : EL.exp -> EL.exp

    val trueexp  : Pos.pos -> EL.exp
    val falseexp : Pos.pos -> EL.exp

    val truepat  : EL.pat
    val falsepat : EL.pat

    (* value of exception Match *)
    val matchexp : Pos.pos -> EL.exp

    val exnname : string

end