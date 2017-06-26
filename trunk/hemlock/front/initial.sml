
structure Initial :> INITIAL =
struct

    open Variable
    structure P = Primop

    val ilint = IL.TVar (namedvar "int")
    val ilstring = IL.TVar (namedvar "string")
    val ilchar = IL.TVar (namedvar "char")
    val ilfile = IL.TVar (namedvar "file")

    val ilplus = namedvar "plus"

    val ilbool = IL.Mu(0, [(namedvar "a", IL.Sum[("true", IL.TRec nil),
                                                 ("false", IL.TRec nil)])])

    val cons = 
        [("ref", IL.Lambda (IL.TRef o hd), 1, IL.Regular),
         ("int", IL.Typ ilint, 0, IL.Regular),
         ("string", IL.Typ ilstring, 0, IL.Regular),
         ("char", IL.Typ ilchar, 0, IL.Regular),
         ("unit", IL.Typ (IL.TRec nil), 0, IL.Regular)]

    val a = namedvar "alpha"
    val b = namedvar "beta"

    fun tuple l =
        let
            fun mktup _ nil = nil
              | mktup n (h::t) = (Int.toString n, h) :: mktup (n + 1) t
        in
            IL.TRec(mktup 1 l)
        end

    (* XXX reevaluate totality of these *)

    val monofuns =
        [(* ("submit", P.PSubmit, [ilstring, ilstring], ilstring),*)

         ("<", P.B (P.PCmp P.PLess), [ilint, ilint], ilbool),
         (">", P.B (P.PCmp P.PGreater), [ilint, ilint], ilbool),
         ("<=", P.B (P.PCmp P.PLesseq), [ilint, ilint], ilbool),
         (">=", P.B (P.PCmp P.PGreatereq), [ilint, ilint], ilbool),
         ("<>", P.B (P.PCmp P.PNeq), [ilint, ilint], ilbool),
         ("=", P.B (P.PCmp P.PEq), [ilint, ilint], ilbool),

         ("concat", P.PConcat, [IL.TVec ilstring], ilstring),

         ("write", P.PWrite, [ilfile, ilstring], tuple nil),
         ("+", P.B P.PPlus, [ilint, ilint], ilint),
         ("-", P.B P.PMinus, [ilint, ilint], ilint),
         ("*", P.B P.PTimes, [ilint, ilint], ilint),
         ("div", P.B P.PDiv, [ilint, ilint], ilint),
         ("mod", P.B P.PMod, [ilint, ilint], ilint)
         ]

    val polyfuns =
        [(* actually, this one is total if it returns... *)
         ("waitall", P.PWaitall,
          IL.Quant(a, IL.Mono
                   (IL.Arrow(false, [IL.TVec (IL.Task (IL.TVar a))],
                             IL.TVec (IL.TVar a))))),

         ("!", P.PGet, IL.Quant(a, IL.Mono
                                (IL.Arrow(false, [IL.TRef (IL.TVar a)],
                                          IL.TVar a)))),

         (":=", P.PSet, IL.Quant(a, IL.Mono
                                 (IL.Arrow(false, [IL.TRef (IL.TVar a),
                                                   IL.TVar a],
                                           tuple nil)))),

         ("ref", P.PRef, IL.Quant(a, IL.Mono
                                  (IL.Arrow(false, [IL.TVar a],
                                            IL.TRef (IL.TVar a))))),

         ("array", P.PArray, IL.Quant(a, IL.Mono 
                                      (IL.Arrow(false, [ilint, IL.TVar a],
                                                IL.TVec (IL.TVar a))))),
         (* XXX I think this is total *)
         ("size", P.PArraySize, IL.Quant(a, IL.Mono
                                         (IL.Arrow(false, [IL.TVec a],
                                                   ilint)))),
         ("sub", P.PSub, 
            IL.Quant(a, IL.Mono
                     (IL.Arrow(false, [IL.TVec (IL.TVar a), ilint],
                               IL.TVar a)))),

         ("update", P.PUpdate, 
            IL.Quant(a, IL.Mono
                     (IL.Arrow (false, [IL.TVec (IL.TVar a),
                                        ilint,
                                        IL.TVar a],
                                tuple nil))))
         ]

    val vals =
        [("stdout", IL.Mono ilfile, IL.Primitive P.PStdout)
(*         ("selfcord", IL.Mono ilstring, IL.Primitive P.PSelfcord) *)
        ] @

        map (fn (name, prim, ty) =>
             (name, ty, IL.Primitive prim)) polyfuns @

        map (fn (name, prim, cod, dom) =>
             (name, IL.Mono (IL.Arrow(false, cod, dom)), 
              IL.Primitive prim)) monofuns

    val initialc = foldl (fn ((s, c, k, t), ctx) =>
                          Context.bindc ctx s c k t) Context.empty cons

    val initial = foldl (fn ((s, c, t), ctx) =>
                         Context.bindex ctx s c (namedvar "dummy") t) 
                        initialc vals

    (* we (should) disallow rebinding of true, false *)

    val truename = "true"
    val falsename = "false"

    val matchname = "Match"

    val exnname = "exn"

    fun wrap (e as (_, loc)) =
        let fun %x = (x, loc)
            fun decbool e =
                %(EL.Let(%(EL.Datatype 
                           (nil, [("bool", 
                                   [(truename, SOME(EL.TRec nil)),
                                    (falsename, SOME(EL.TRec nil))])])),
                         e))
            fun decexn e =
                %(EL.Let
                  (%(EL.Tagtype exnname),
                   %(EL.Let
                     (%(EL.Exception (matchname, SOME(EL.TRec nil))),
                      e))))
        in
            (decbool o decexn) e
        end

    fun trueexp loc = (EL.App((EL.Var truename,loc), 
                              (EL.Record nil, loc)), loc)

    fun falseexp loc = (EL.App((EL.Var falsename,loc), 
                               (EL.Record nil, loc)), loc)

    val truepat = EL.PApp (truename, EL.PRecord nil)
    val falsepat = EL.PApp (falsename, EL.PRecord nil)

    fun matchexp loc = (EL.App ((EL.Var matchname, loc), 
                                (EL.Record nil, loc)), loc)

end