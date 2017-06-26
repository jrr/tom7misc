
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

    (* let datatype s = aaa | bbb | ccc
           fun f(x) = aaa ()
           fun g(y,z) = f y
       in
           case g (10, 20) of
             aaa v => "hello"
             bbb v => "yes"
             _ => "no"
       end
   *)


    val s = Sum[("aaa", TRec[]),
                ("bbb", TRec[]),
                ("ccc", TRec[])]

    val case_il = 
        Let(Fix(Mono[{name=f,arg=[x],dom=[TRec[]],cod=TRec[],
                      body=Inject (s, "bbb", Record nil) }]),
            Let(Fix(Mono[{name=g,arg=[y,z],dom=[TRec[]],cod=TRec[],
                          body=App(Var f, [Var y])}]),
                Sumcase(s,
                        App(Var g, [Int 10, Int 20]),
                        v, 
                        [("aaa", String "hello"),
                         ("bbb", String "yes"),
                         ("ccc", String "okay")],
                        String "impossible")
                ))

    val case_ce = ToCPS.convert nil 
        (fn x => CPS.Primop(Primop.PFinish,[x],[],[])) case_il

end