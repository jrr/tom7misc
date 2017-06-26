
structure ElabUtil :> ELABUTIL =
struct

    exception Elaborate of string

    infixr 9 `
    fun a ` b = a b

    val ltos = Pos.toString

    fun error _ msg =
        let in
            print "Error: ";
            print msg;
            print "\n";
            raise Elaborate "error"
        end

    fun new_evar () = IL.Evar ` Unify.new_ebind ()

    (* unify context location message actual expected *)
    fun unify ctx loc msg t1 t2 =
            Unify.unify ctx t1 t2
            handle Unify.Unify s => 
                let 
                    val $ = Layout.str
                    val % = Layout.mayAlign
                in
                    Layout.print
                    (Layout.align
                     [%[$"Type mismatch (", $s, $") at ", $(Pos.toString loc),
                        $": ", $msg],
                      %[$"expected:", Layout.indent 4 (ILPrint.ttol t2)],
                      %[$"actual:  ", Layout.indent 4 (ILPrint.ttol t1)]],
                     print);
                    print "\n";
                    error loc "Compilation failed."
                end

    val itos = Int.toString

    val newstr = HemlockUtil.newstr

    (* fixed. uses outer context to determine what evars
       should be generalized. *)
    (* XXX it seems that the substitution aspect of this is unnecessary; 
       imperative evars have substitution 'built-in', so we don't need
       to reconstruct the term *)
    fun polygen ctx ty =
        let val acc = ref nil
            fun go t =
                (case t of
                     IL.TRef tt => IL.TRef ` go tt
                   | IL.TVec tt => IL.TVec ` go tt
                   | IL.Task tt => IL.Task ` go tt
                   | IL.Sum ltl => IL.Sum ` ListUtil.mapsecond go ltl
                   | IL.Arrow (b, tl, tt) => IL.Arrow(b, map go tl, go tt)
                   | IL.TRec ltl => IL.TRec ` ListUtil.mapsecond go ltl
                   | IL.TVar v => t
                   | IL.TTag (tt, v) => IL.TTag (go tt, v)
                   | IL.Mu (n, vtl) => IL.Mu (n, ListUtil.mapsecond go vtl)
                   | IL.Evar er =>
                         (case !er of
                              IL.Free n =>
                                  if Context.has_evar ctx n
                                  then t
                                  else
                                      let 
                                          val tv = Variable.namedvar "poly"
                                      in
                                          acc := tv :: !acc;
                                          er := IL.Bound (IL.TVar tv);
                                          IL.TVar tv
                                      end
                            | IL.Bound ty => go ty))
        in
            (go ty, !acc)
        end

    (* (old, fixed now??) problems with polygen:

       Need to substitute through term, too, 
       since types appear in terms (like val x : t = ...)

       can't generalize all bound evars, see:

       fun f x =
          let val y = x
          in y
          end

          does NOT have type A a,b . a -> b

       ... I think the thing to do is to only generalize evars
       that don't appear in the surrounding context (ie, as
       the type of some variable). In that case we can use
       the strategy above, except we don't even need
       Substituted (we never did), as we can just make them
       be Bound (TVar new_v).

       *)


    (* XXX implement!
       only values are generalized, so always return argument (XXX
       doesn't need to be in return)
       return 
       a list of type variables (to be bound in the polytype)
       and the type, using those vars *)
    fun generalize ctx e t = (e, nil, t)

    fun evarize pt =
        let
            fun unpoly (IL.Quant (v, pt)) ts = unpoly pt (v::ts)
              | unpoly (IL.Mono mt) ts =
                let
                    (* XXX don't need to build up evs list *)
                    fun mkes nil = (nil, Variable.Map.empty)
                      | mkes (tv::rest) =
                        let val (ets, subst) = mkes rest
                            val ev = new_evar ()
                        in
                            (ev :: ets, 
                             Variable.Map.insert (subst, tv, ev))
                        end
                    
                    val (ets, subst) = mkes ts
                in
                    Subst.tsubst subst mt
                end
        in
            unpoly pt nil
        end


end