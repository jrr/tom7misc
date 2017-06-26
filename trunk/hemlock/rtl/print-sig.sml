
signature RTLPRINT =
sig

    val ctos : RTL.cond -> string
    val dtos : RTL.dst -> string
    val otos : RTL.oper -> string
    val printc : RTL.code -> unit
    val printi : RTL.inst -> unit
    val printl : string -> unit
    val printp : RTL.program -> unit
    val stos : RTL.src -> string
    val ttos : RTL.rtltype -> string
    val wtos : RTL.wordtag -> string

end