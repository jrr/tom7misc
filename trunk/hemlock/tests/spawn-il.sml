
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

    (* let val u = 10
       in let val z = spawn (u, u, 20)
          in 2
          end
       end *)

    val spawne = 
        Let(Val(Mono(u, TRec[], Int 10)),
            Let(Val(Mono(z, TRec[], Spawn (Record[("a", Var u), 
                                                  ("b", Var u), 
                                                  ("c", Int 20)]))),
                Int 2))

    val spawn_ce = ToCPS.convert nil 
        (fn x => CPS.Primop(Primop.PFinish,[x],[],[])) spawne

end