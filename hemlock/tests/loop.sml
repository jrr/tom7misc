
local
    structure V = Variable

    val x = V.namedvar "x"
    val y = V.namedvar "y"
    val f = V.namedvar "f"
    val g = V.namedvar "g"
    val h = V.namedvar "h"
    val z = V.namedvar "z"
    val u = V.namedvar "u"
    val v = V.namedvar "v"

    open IL
in

    (* let
           fun f(x) = f (x)
       in
           f (100)
       end
   *)


    val s = Sum[("aaa", TRec[]),
                ("bbb", TRec[]),
                ("ccc", TRec[])]

    val case_il = 
        Let(Fix(Mono[{name=f,arg=[x],dom=[TRec[]],cod=TRec[],
                      body=App(Var f, [Var x])}]),
            App(Var f, [Int 100]))

    val case_ce = ToCPS.convert nil 
        (fn x => CPS.Primop(Primop.PFinish,[x],[],[])) case_il

end