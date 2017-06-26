
signature CPSOPTUTIL =
sig

    exception CPSOptUtil of string

    (* eval the comparison of a binary operator on two ints *)
    val evalcmp : Primop.compare -> int -> int -> bool

    (* evaluate a binary operator on two ints, if defined *)
    val evalints : Primop.binop -> int -> int -> CPS.value option

    (* is the primop effect-free? *)
    val noeffect : Primop.primop -> bool

end