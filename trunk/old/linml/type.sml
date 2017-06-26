
structure Type :> TYPE =
struct
    infix ++

    val debug = ref false
    fun db s = if !debug then print ("debug: " ^ s ^ "\n") else ()

    open AST

    exception TypeError of string
    exception Unimplemented

    fun lookup C x = ListUtil.Alist.find op= C x
    val empty = nil
    fun C ++ p = p :: C
    val isempty = null
    fun consume C x = ListUtil.Alist.removefirst op= C x

    fun sfirstsort x = ListUtil.sort (ListUtil.byfirst String.compare) x

    (* typeok T t
       check if t is well-formed in T *)
    fun typeok T Bool = true
      | typeok T String = true
(*      | typeok T (List t) = typeok T t *)
      | typeok T (Schan t) = typeok T t
      | typeok T (Rchan t) = typeok T t
      | typeok T (Tvar v) = List.exists (fn x => x = v) T
      | typeok T (All (s,t)) = typeok (s :: T) t
      | typeok T (Sum stl) = List.all ((typeok T) o #2) stl
      | typeok T (Tensor stl) = List.all ((typeok T) o #2) stl
      | typeok T (With stl) = List.all ((typeok T) o #2) stl
      | typeok T (Lolli (a,b)) = typeok T a andalso typeok T b
      | typeok T (Arrow (a,b)) = typeok T a andalso typeok T b
      | typeok T (Bang t) = typeok T t

    fun tok T t =
        typeok T t orelse raise TypeError "ill-formed type"

    fun typeq G t1 t2 =
      (db "typeq";
       case (t1, t2) of
            (Bool, Bool) => true
	  | (String,String) => true
          | (Arrow (a,aa), Arrow (b,bb)) => typeq G a b andalso typeq G aa bb
          | (Lolli (a,aa), Lolli (b,bb)) => typeq G a b andalso typeq G aa bb
          | (With stla, With stlb) => ListUtil.all2 (fn ((s,t),(ss,tt)) => s = ss
                                                     andalso typeq G t tt) 
                (sfirstsort stla) 
                (sfirstsort stlb)
          | (Tensor stla, Tensor stlb) => ListUtil.all2 (fn ((s,t),(ss,tt)) => s = ss
                                                     andalso typeq G t tt) 
                (sfirstsort stla) 
                (sfirstsort stlb)
          | (Sum stla, Sum stlb) => ListUtil.all2 (fn ((s,t),(ss,tt)) => s = ss
                                                     andalso typeq G t tt) 
                (sfirstsort stla) 
                (sfirstsort stlb)
          | (Bang t, Bang u) => typeq G t u
          | (Schan t, Schan u) => typeq G t u
          | (Rchan t, Rchan u) => typeq G t u
          | (All (s, t), All (v,u)) => typeq (G ++ (s,v)) t u
          | (Tvar s, Tvar x) =>
                (case lookup G s of
                     (SOME ss) => ss = x
                   | NONE => s = x)
          | _ => false)

    fun teq t1 t2 = ignore (typeq empty t1 t2 orelse raise TypeError "type mismatch";
                            db "types =")

    (* merge residual contexts A and B (as if for &). 
       currently this requires equality.
       *)
    fun mergectx A B = 
        let 
            val a = sfirstsort A
            val b = sfirstsort B
            fun cmp ((s,t),(ss,tt)) =
                s = ss andalso typeq empty t tt
        in
            db "mergectx";
            ListUtil.all2 cmp a b orelse raise TypeError "resulting contexts must be the same";
            a
        end

    fun tsubst (s as (v, t)) ty =
        let 
            fun ts ty = 
                case ty of
                    Bool => Bool
                  | Arrow (t1,t2) => Arrow (ts t1, ts t2)
                  | Lolli (t1,t2) => Lolli (ts t1, ts t2)
                  | With stl => With (ListUtil.mapsecond ts stl)
                  | Tensor stl => Tensor (ListUtil.mapsecond ts stl)
                  | Sum stl => Sum (ListUtil.mapsecond ts stl)
                  | Bang t => Bang (ts t)
                  | All (a,t) => if a = v then ty else (All (a, ts t))
                  | Tvar a => if a = v then t else ty
		  | String => String
                  | Schan t => Schan (ts t)
                  | Rchan t => Rchan (ts t)
        in
            db "tsubst";
            (ts ty) before (db "tsubstdone")
        end

    (* if there exists D', a subset of D, such that
       T; G; D' |- e : t

       then tc T G D e
       returns (t, D \ D')
       otherwise, it raises TypeError.
       *)
    fun tc T G D e =
      (db ("tc: " ^ Print.ts e);
       (case e of
            Let (v, e1, e2) =>
		let
		    val (t1, DD) = tc T G D e1
		    val (t2, DDD) = tc T G (DD ++ (v, t1)) e2
		in
		    List.exists (fn (x,_) => v = x) DDD andalso
		       raise TypeError ("linear var " ^ v ^ 
					" not used in let body");
		    (t2, DDD)
		end
          | Fix (f, x, dom, cod, body) =>
                let
                    val _ = 
                        x <> f orelse raise TypeError "function name same as argument in Fix"
                    val (tb, _) = tc T (G ++ (f,Arrow(dom,cod)) ++ (x, dom)) empty body
                in
                    teq tb cod;
                    (Arrow(dom,cod), D)
                end
          | Print e =>
                (case tc T G D e of
                   (String, DD) => (Tensor [], DD)
                 | _ => raise TypeError "can only print strings")
          | Lett (ssl, e1, e2) =>
                (case tc T G D e1 of
                     (Tensor stl, DD) =>
                         let
                             val stl = sfirstsort stl
                             val ssl = sfirstsort ssl

                             fun go DDD nil nil = DDD
                               | go DDD ((s,t)::strest) ((ss,sv)::ssrest) =
                                 let in
                                     s = ss orelse raise 
                                        TypeError ("Labels do not match in lett (" ^ s ^ 
                                                   " vs. " ^ ss ^ ")");
                                     go ( DDD ++ (sv, t) ) strest ssrest
                                 end
			       | go _ _ _ = 
				 raise TypeError "labels do not match in lett: length"

			     val DDD = go DD stl ssl
			     val vs = map #2 ssl
			     val (tr, DR) = tc T G DDD e2
                         in
			     List.exists (fn (_,s) =>
					  List.exists (fn (v,_) => s = v) DR) ssl
			        andalso raise TypeError "Bound linear vars not used in lett";
                             (tr, DR)
                         end
		   | _ => raise TypeError "Lett needs Tensor")
          | Case (obj, v, sel) =>
                (case tc T G D obj of
                     (Sum stl, DX) =>
                         let
                             val stl = sfirstsort stl
                             val sel = sfirstsort sel

                             fun chk s ss v DDD =
                                 let in
                                     s = ss orelse raise TypeError ("case arms mismatch: " ^
                                                                    s ^ " vs. " ^ ss);
                                     List.exists (fn (x,_) => v = x) DDD andalso
                                       raise TypeError ("linear var " ^ v ^ 
                                                        " not used in case arm")
                                 end

                             fun f (tt, DD) nil nil = (tt, DD)
                               | f (tt, DD) ((s,t)::resttl) ((ss,e)::restel) =
                                 let val (ta, DDD) = tc T G (DX ++ (v, t)) e
                                 in
                                     teq ta tt; 
                                     chk s ss v DDD;
                                     f (ta, mergectx DD DDD) resttl restel
                                 end
                               | f _ _ _ = raise TypeError ("case arms length mismatch")
                         in
                             case (stl,sel) of
                               ((s,t)::resttl, (ss,e)::restel) =>
                                     let val (ta, DDD) = tc T G (DX ++ (v, t)) e
                                     in  
                                         chk s ss v DDD;
                                         f (ta, DDD) resttl restel
                                     end
                             | _ => raise TypeError "arms empty??"
                         end
                   | _ => raise TypeError "must case on Sum type")
          | Abort t => raise Unimplemented
          | Lproj (s, e) =>
                (case tc T G D e of
                     (With stl, DD) =>
                         (case ListUtil.Alist.find op= stl s of
                              SOME t => (t, DD)
                            | NONE => raise TypeError "Projected field not in type")
                   | _ => raise TypeError "Lproj from With types only")
          | Seq [e] => tc T G D e
          | Seq (h::t) =>
		     (case tc T G D h of
			  (Tensor [], DD) => tc T G DD (Seq t)
			| _ => 
			  raise TypeError "Non-final sequence elements must have type unit")
          | Seq nil => raise TypeError "empty sequence"
          | App (f, a) =>
                let
                    val (tf, DD) = tc T G D f
                    (* argument must not use linear resources *)
                    val (ta, _) = tc T G empty a
                in
                    case tf of
                        Arrow (dom,cod) =>
                            let in
                                teq dom ta;
                                (cod,DD)
                            end
                      | _ => raise TypeError "exp in function position of app not arrow type"
                end
          | Var v =>
                (case lookup D v of
                     NONE => (case lookup G v of
                                  NONE => raise TypeError ("unbound variable " ^ v)
                                | SOME t => (t, D))
                   | SOME t => (t, consume D v))
          | Lfn (x, dom, body) =>
                let
(*                  val (x, body) = rename x body *) (* XXX rename outside *)
                    val (tb, dd) = tc T G (D ++ (x, dom)) body
                in
                    if List.exists (fn (v,_) => v = x) dd then
                        raise TypeError ("linear argument " ^ x ^ " not used!")
                    else (Lolli (dom, tb), dd)
                end
	    (* linear fix. Argument is linear, body unrestricted, recursive. *)
          | Lfix (f, x, dom, cod, body) =>
                let
                    val (tb, dd) = tc T (G ++ (f, Lolli(dom,cod))) (empty ++ (x, dom)) body
                in
		    isempty dd orelse raise TypeError ("linear argument " ^ x ^ " not used");
		    (Lolli (dom, tb), D)
                end
          | Tfn (s, e) =>
                let val (tb, dd) = tc (T ++ s) G D e
                in (All (s, tb), dd)
                end
          | Lapp (f, a) =>
                (case tc T G D f of
                     (Lolli (dom,cod), DD) =>
                         let val (at, DDD) = tc T G DD a
                         in 
                             teq at dom;
                             (cod, DDD)
                         end
                   | _ => raise TypeError 
                         "function place of linear application must be lolli type")
          | Tapp (f, t) =>
                (case tc T G D f of
                     (All (s, ty), DD) => (tsubst (s, t) ty, DD)
                   | _ => raise TypeError "Tapp must be All type")
          | True => (Bool, D)
          | False => (Bool, D)
	  | Str s => (String, D)
          | If (c, t, f) =>
                (case tc T G D c of
                     (Bool, DD) =>
                         let
                             val (tt, DDT) = tc T G DD t
                             val (ft, DDF) = tc T G DD f
                         in
                             teq tt ft;
                             (tt, mergectx DDT DDF)
                         end
                   | _ => raise TypeError "If object must be bool")
          | Ltuple nil => raise TypeError "Ltuple length must be > 1"
          | Ltuple [_] => raise TypeError "Ltuple length must be > 1"
          | Ltuple ((hs,he)::rest) =>
                     let
                         val (ht, DD) = tc T G D he
                         fun f ((s,e), (stl, DD)) =
                             let val (t, DDD) = tc T G D e
                             in ((s,t)::stl, mergectx DD DDD)
                             end
                         val (stl, DDD) = foldl f ([(hs,ht)], DD) rest
                     in
                         (With stl, DDD)
                     end
          | Tuple sel =>
                     let
                         fun f ((s,e), (stl, D)) =
                             let val (t, DD) = tc T G D e
                             in ((s,t)::stl,DD)
                             end
                         val (stl, DD) = foldl f (nil,D) sel
                     in
                         (Tensor stl, DD)
                     end
          | Exp e => let val (te, _) = tc T G empty e
                     in (Bang te, D)
                     end
          | Inj (s, e, Sum stl) =>
                     let in 
                         tok T (Sum stl);
                         (case ListUtil.Alist.find op= stl s of
                              SOME t => 
                                  let val (te, DD) = tc T G D e
                                  in 
                                      teq t te;
                                      (Sum stl, DD)
                                  end
                            | NONE => raise TypeError "Arm doesn't exist in Sum injection...")
                     end
          | Inj _ => raise TypeError "Can only inject into Sum types"
          | Send (ch, d) => 
                (case tc T G D ch of
                     (Schan tch, D') => 
                         let val (td, DD) = tc T G D' d
                         in teq td tch;
                             (Schan tch, DD)
                         end
                   | _ => raise TypeError "Send needs Schan t")
          | Recv ch =>
                (case tc T G D ch of
                     (Rchan t, dd) => (Tensor [("data", t), ("channel", Rchan t)], dd)
                   | _ => raise TypeError "Recv needs Rchan t")
          | Use (s, e, e2) =>
                (case tc T G D e of
                     (Bang t, dd) => tc T (G ++ (s, t)) dd e2
                   | _ => raise TypeError "can only 'use' ! types")
          | Chan t =>
                let in
                    tok T t;
                    (Tensor [("s", Schan t),
                             ("r", Rchan t)],
                     D)
                end
	  | Spawn e => 
		(case tc T G D e of
		     (Lolli (Tensor [], Tensor []), DD) => (Tensor [], DD)
		   | _ => raise TypeError "Spawn needs unit -o unit function")
          | Close e =>
                (case tc T G D e of
                     (Schan t, DD) => (Tensor [], DD)
                   | (Rchan t, DD) => (Tensor [], DD)
                   | _ => raise TypeError "Close needs Schan or Rchan")
	  | SC _ => raise Fail "impossible"
	  | RC _ => raise Fail "impossible") 
          before (db "tc done"))

    fun typecheck e = 
        case tc nil nil nil e of
            (t, nil) => t
          | _ => raise TypeError "does not use all linear resources!"


end