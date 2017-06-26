
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

    (* let fun f(x) = 3
           fun g(y,z) = f y
       in
           (g(10,10), "hello world", g(20,30))
       end
   *)


    val eta_il = 
        Let(Fix(Mono[{name=f,arg=[x],dom=[TRec[]],cod=TRec[],body=Int 3}]),
            Let(Fix(Mono[{name=g,arg=[y,z],dom=[TRec[]],
                          cod=TRec[],body=App(Var f, [Var y])}]),
                Record([("fst", App(Var g, [Int 10,Int 10])),
                        ("mid", String "hello world"),
                        ("snd", App(Var g, [Int 20,Int 30]))])))

    val eta_ce = ToCPS.convert nil 
        (fn x => CPS.Primop(Primop.PFinish,[x],[],[])) eta_il

    val eta_cc = Closure.convert eta_ce

    val eta_ae = CPSAlloc.convert eta_cc

end