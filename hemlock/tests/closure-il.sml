
local
    structure V = Variable

    val x = V.namedvar "x"
    val y = V.namedvar "y"
    val f = V.namedvar "f"
    val g = V.namedvar "g"
    val h = V.namedvar "h"
    val z = V.namedvar "z"
    val u = V.namedvar "u"

    open IL
in

    (* let val u = ()
           fun f x = x
           and g y =
               let fun h z = f u
               in 10
               end 
       in 0
       end *)

    val e = 
        Let(Val(Mono(u, TRec[], Record[])),
            Let(Fix(Mono[{name=f,arg=[x],dom=[TRec[]],
                          cod=TRec[],body=Var x},
                         {name=g,arg=[y],dom=[TRec[]],cod=TRec[],
                          body=Let(Fix(Mono[{name=h,arg=[z],
                                             dom=[TRec[]],cod=TRec[],
                                             body=App (Var f,[Var u])}]),
                                   Int 10)}]),
                Int 0))

    val ce = ToCPS.convert nil 
        (fn x => CPS.Primop(Primop.PFinish,[x],[],[])) e

end