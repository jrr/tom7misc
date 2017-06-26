
structure Subst :> SUBST =
struct

    open AST

    local
        val ctr = ref 0
    in
        fun new_str s = (ctr := !ctr + 1; s ^ "-" ^ (Int.toString (!ctr)))
    end

    fun subst (x,y) e =
        let
            fun sb (Let (v, a, b)) =
                Let(v, sb a, if v = x then b else sb b)
              | sb (Var v) = (if v = x then y else Var v)
              | sb (exp as (Fix (ff,xx,tt,tc,bb))) =
                if ff = x orelse xx = x then exp
                else (Fix(ff, xx, tt, tc, sb bb))
              | sb (exp as (Lfix (ff,xx,tt,tc,bb))) =
                if ff = x orelse xx = x then exp
                else (Lfix(ff, xx, tt, tc, sb bb))
              | sb (exp as (Lfn (xx, tt, bb))) =
                    if xx = x then exp
                    else (Lfn(xx, tt, sb bb))
              | sb (Tfn(tv,e)) = Tfn (tv, sb e)
              | sb (Seq el) = Seq (map sb el)
              | sb (App (a,b)) = App (sb a, sb b)
              | sb (Lapp (a,b)) = Lapp (sb a, sb b)
              | sb (Tapp (e,t)) = Tapp (sb e, t)
              | sb (If (a, b, c)) = If (sb a, sb b, sb c)
              | sb (Ltuple sel) =
                        Ltuple (ListUtil.mapsecond sb sel)
              | sb (Tuple sel) = Tuple (ListUtil.mapsecond sb sel)
              | sb (Exp e) = Exp (sb e)
              | sb (Inj (s,e,t)) = Inj (s, sb e, t)
              | sb (Lproj(s,e)) = Lproj (s, sb e)
              | sb (Lett (ssl, e1, e2)) =
                        Lett (ssl, sb e1, 
                              if (List.exists (fn (l,v) => v = x) ssl)
                                  then e2
                              else sb e2)
              | sb (Case (e, s, sel)) =
                        Case (sb e, s,
                              if s = x then sel
                              else ListUtil.mapsecond sb sel)
              | sb (Send (a,b)) = Send (sb a, sb b)
              | sb (Recv e) = Recv (sb e)
              | sb (e as Abort t) = e
              | sb (Use (s, e1, e2)) =
			Use (s, sb e1, if s = x then e2 else sb e2)
              | sb (e as Chan t) = e
              | sb (Close e) = Close (sb e)
	      | sb (Spawn e) = Spawn (sb e)
              | sb True = True
              | sb False = False
	      | sb (e as Str s) = e
	      | sb (SC l) = SC l
	      | sb (RC l) = RC l
              | sb (Print e) = Print (sb e)
        in
	    sb e
        end


    fun rename e =
      let
        
        fun rn (Let (v, a, b)) =
              let val ns = new_str v
              in Let(ns, rn a, subst (v, Var ns) (rn b))
              end
          | rn (Var v) = Var v
          | rn (exp as (Fix (ff,xx,tt,tc,bb))) =
              let val nf = new_str ff
                  val nx = new_str xx
              in Fix(nf, nx, tt, tc, subst (ff, Var nf) (subst (xx, Var xx) (rn bb)))
              end
          | rn (exp as (Lfix (ff,xx,tt,tc,bb))) =
              let val nf = new_str ff
                  val nx = new_str xx
              in Lfix(nf, nx, tt, tc, subst (ff, Var nf) (subst (xx, Var xx) (rn bb)))
              end
          | rn (exp as (Lfn (xx, tt, bb))) =
              let val nx = new_str xx
              in Lfn (nx, tt, subst (xx, Var nx) (rn bb))
              end
          | rn (Tfn(tv,e)) = Tfn (tv, rn e)
          | rn (Seq el) = Seq (map rn el)
          | rn (App (a,b)) = App (rn a, rn b)
          | rn (Lapp (a,b)) = Lapp (rn a, rn b)
          | rn (Tapp (e,t)) = Tapp (rn e, t)
          | rn (If (a, b, c)) = If (rn a, rn b, rn c)
          | rn (Ltuple sel) =
                Ltuple (ListUtil.mapsecond rn sel)
          | rn (Tuple sel) = Tuple (ListUtil.mapsecond rn sel)
          | rn (Exp e) = Exp (rn e)
          | rn (Inj (s,e,t)) = Inj (s, rn e, t)
          | rn (Lproj(s,e)) = Lproj (s, rn e)
          | rn (Lett (ssl, e1, e2)) =
                let
                  fun f (s1,s2) = (s1,new_str s2, s2)
                  val nsss = map f ssl
                  fun dosubst nil e = e
                    | dosubst ((_,new,old)::t) e = dosubst t (subst (old, Var new) e)
                in
                  Lett (map (fn (a,b,c) => (a,b)) nsss, rn e1, dosubst nsss (rn e2))
                end
          | rn (Case (e, s, sel)) =
                let val ns = new_str s
                in Case (rn e, ns,
                         ListUtil.mapsecond (subst (s,Var ns)) 
                            (ListUtil.mapsecond rn sel))
                end
          | rn (Send (a,b)) = Send (rn a, rn b)
          | rn (Recv e) = Recv (rn e)
          | rn (e as Abort t) = e
          | rn (Use (s, e1, e2)) =
                let val ns = new_str s
                in Use (ns, rn e1, subst (s, Var ns) (rn e2))
                end
          | rn (e as Chan t) = e
          | rn (Close e) = Close (rn e)
          | rn (Spawn e) = Spawn (rn e)
          | rn True = True
          | rn False = False
          | rn (e as Str s) = e
          | rn (SC l) = SC l
          | rn (RC l) = RC l
          | rn (Print e) = Print (rn e)

      in
        rn e
      end

end