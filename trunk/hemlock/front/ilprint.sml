
structure ILPrint :> ILPRINT =
struct

    fun nspaces n = StringUtil.tabulate n #" "

    open IL
    structure L = Layout
    structure V = Variable

    val $ = L.str
    val % = L.mayAlign
    val itos = Int.toString

    fun dolist s f i l = StringUtil.delimit s (List.map (fn m => f i m) l)

    fun recordortuple layout sep l r osep vals =
        let 
            val sorted = ListUtil.sort (ListUtil.byfirst String.compare) vals
        in
            if
               (* can't be length 1 *)
               length sorted <> 1 andalso
               (* must be consecutive numbers *)
               ListUtil.alladjacent (fn ((x,_), (sx,_)) => 
                                     (case (Int.fromString x, 
                                            Int.fromString sx) of
                                          (SOME xx, SOME ssxx) => xx = ssxx - 1
                                        | _ => false)) sorted andalso
               (* must be empty or start at 1 *)
               (List.null sorted orelse
                #1 (hd sorted) = "1")
            then L.listex l r osep (map (layout o #2) sorted)
            else L.recordex sep (ListUtil.mapsecond layout sorted)
        end
           

    fun ttol t =
        (case t of
           (* should print these right-associatively *)
             Arrow (b, [dom], cod) =>
                 L.paren (%[ttol dom,
                            $(if b then "=>" else "->"),
                            ttol cod])
           | Arrow (b, dom, cod) => 
                 L.paren (%[L.list (map ttol dom),
                            $(if b then "=>" else "->"),
                            ttol cod])
           | TVec t => L.paren (L.seq[ttol t, $" vector"])
           | Task t => L.paren (L.seq[ttol t, $" task"])
           | TRef t => L.paren (L.seq[ttol t, $" ref"])
           | TVar v => L.str (V.tostring v)
           | Sum ltl => L.listex "[" "]" "," (map (fn (l, t) =>
                                                   L.seq[$l, $" : ", 
                                                         ttol t]) ltl)
           | Mu (i, m) => L.paren (%[$("#" ^ itos i),
                                     $"mu",
                                     L.alignPrefix
                                       (ListUtil.mapi 
                                        (fn ((v,t),n) =>
                                         %[$(itos n), $"as",
                                           $(V.tostring v),
                                           $".", ttol t]) m,
                                       "and ")])
           | TRec nil => $"unit"
           | TRec ltl => recordortuple ttol ":" "(" ")" " *" ltl

           | TTag (t, v) => %[$"tag", ttol t, $"=>", $(V.tostring v)]
           | Evar (ref (Bound t)) => ttol t
           | Evar (ref (Free n)) => $("'a" ^ itos n))

    fun etol e =
        (case e of
             Var v => $(V.tostring v)
           | Int i => $(itos i)
           | String s => $("\"" ^ String.toString s ^ "\"")
           | Char c => $("?" ^ implode [c])
           | App (e1, [e2]) => L.paren(%[etol e1, etol e2])
           | App (e1, e2) => L.paren(%[etol e1, L.list (map etol e2)])
           | Appt (e, t) => %[etol e, L.seq[$"<", ttol t, $">"]]
           | Spawn e => L.paren(%[$"spawn", etol e])
           (* print as if n-ary *)
           | Seq _ => 
                 let 
                     fun allseqs acc (Seq (ee, er)) = allseqs (ee::acc) er
                       | allseqs acc e = (acc, e)

                     val (front, last) = allseqs nil e
                 in
                     L.listex "(" ")" ";" (rev (map etol (last::front)))
                 end
           (* also fake n-ary like above *)
           | Let (dd, ee) =>
                 let
                     fun alldecs acc (Let (dd, er)) = alldecs (dd::acc) er
                       | alldecs acc e = (acc, e)

                     val (decs', body) = alldecs nil e
                     val decs = rev (map dtol decs')
                 in
                     L.align
                     [$"let",
                      L.indent 4 (L.align decs),
                      $"in",
                      L.indent 4 (etol body),
                      $"end"]
                 end
           | Proj (l, t, e) => 
                 %[L.seq[$("#" ^ l), $"/", L.paren(ttol t)],
                   etol e]
           | Record sel => recordortuple etol "=" "(" ")" "," sel
           | Primapp (po, el, ts) =>
                 %[$"[PRIM", $(Primop.tostring po),
                   L.listex "(" ")" "," (map etol el),
                   L.listex "<" ">" "," (map ttol ts), $"]"]
           | Inject (t, l, e) => L.paren(%[$("inj_" ^ l),
                                           L.seq[$"<", ttol t, $">"],
                                           etol e])
           | Unroll e => L.paren(%[$"unroll", etol e])
           | Roll (t,e) => L.paren(%[$"roll", 
                                     L.seq[$"<", ttol t, $">"],
                                     etol e])
           | Sumcase (t, e, v, lel, def) =>
                 L.align
                 (%[$"sumcase", etol e, $":", ttol t, 
                    $"as", $ (V.tostring v)] ::
                  map (fn (l, e) => %[$"  |", $l, $"=>", etol e]) lel @
                  [%[$"  |", $"_", $"=>", etol def]])
           | Tagcase (t, e, v, vel, def) =>
                 L.align
                 (%[$"tagcase", etol e, $":", ttol t, 
                    $"as", $ (V.tostring v)] ::
                  map (fn (vv, e) => %[$"  |", $(V.tostring vv), 
                                       $"=>", etol e]) 
                         vel @
                  [%[$"  |", $"_", $"=>", etol def]])

           | Raise (t, e) => L.paren(%[$"raise", 
                                       L.seq[$"<", ttol t, $">"], etol e])
           | Tag (e1, e2) => L.paren(%[$"tag", etol e1, $"with", etol e2])
           | _ => $"???XXX???")

    and dtol d =
        (case d of
             Do e => %[$"do", etol e]
           | Tagtype v => %[$"tagtype", $(V.tostring v)]
           | Newtag (new, t, ext) => %[$"newtag", $(V.tostring new), 
                                       $"tags", ttol t, $"in", 
                                       $(V.tostring ext)]
           | Val vtep =>
                 let
                     fun up (Quant (v, p)) = 
                         %[$(V.tostring v), up p]
                       | up (Mono (v, t, e)) =
                         %[$(V.tostring v), $":",
                           ttol t, $"=", etol e]
                 in
                     %[$"val", L.indent 4 (up vtep) ]
                 end
           | Fix pfs =>
                 let
                     fun up (Quant (v, p)) =
                         %[$(V.tostring v), up p]
                       | up (Mono fl) =
                         L.alignPrefix
                         (map (fn {name, arg, dom, cod, body} =>
                               %[
                               %[if length arg <> length dom 
                                then $"XXX arg/dom mismatch!!"
                                else $"",
                                $(V.tostring name),
                                L.listex "(" ")" "," 
                                    (ListPair.map 
                                     (fn (a, t) =>
                                      %[$(V.tostring a),
                                        $":",
                                        ttol t]) (arg, dom)),
                                $":",
                                ttol cod,
                                $"="],
                                L.indent 4 (etol body)]) fl,
                          "and ")
                 in
                     %[$"fun", up pfs]
                 end)

end