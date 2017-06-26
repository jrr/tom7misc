
structure Context :> CONTEXT =
struct
    open Variable
        
    structure S = StringMap
    structure SU = StringMapUtil

    datatype context = 
        C of { vars : (IL.typ IL.poly * Variable.var * IL.idstatus) S.map,
               cons : (IL.kind * IL.con * IL.tystatus) S.map }

      exception Absent

    fun absent s =
        let in 
(*
              print "(Unbound in context: ";
              print s;
              print ")\n";
*)
            raise Absent
        end

    (* this only checks vars, not cons.
       But evars shouldn't ever appear bound to type variables, right?
       (I can't look inside lambdas, anyway.) *)
    fun has_evar (C{vars, ...}) n =
        SU.exists
        (fn (pt, _, _) =>
         let
             open IL
             fun up (Quant (_, p)) = up p
               | up (Mono t) =
                 let
                     fun has tt =
                         (case tt of
                              TVar _ => false
                            | TRec ltl => List.exists (fn (_, t) => 
                                                       has t) ltl
                            | Arrow (_, tl, t) =>
                                  has t orelse
                                  List.exists has tl
                            | Sum ltl => List.exists (fn (_, t) =>
                                                      has t) ltl
                            | Mu (_, vtl) => List.exists (fn (_, t) =>
                                                          has t) vtl
                            | Evar (ref (Free m)) => n = m
                            | Evar (ref (Bound t)) => has t
                            | TVec t => has t
                            | Task t => has t
                            | TTag (t, _) => has t
                            | TRef t => has t)
                 in
                     has t
                 end
         in
             up pt
         end) vars

        

    fun var (C {vars, ...}) sym =
        (case S.find (vars, sym) of
             SOME x => x
           | NONE => absent sym)
             
    fun con (C {cons, ...}) sym =
        (case S.find (cons, sym) of
             SOME x => x
           | NONE => absent sym)
             
    fun bindex (C {vars, cons}) sym typ var stat =
        C { vars = S.insert (vars, sym, (typ, var, stat)),
            cons = cons }
        
    fun bindv a b c d = bindex a b c d IL.Normal
        
    fun bindc (C { cons, vars }) sym con kind status =
        C { vars = vars,
            cons = S.insert (cons, sym, (kind, con, status)) }
        
    val empty = C { vars = S.empty, cons = S.empty }
          
end
