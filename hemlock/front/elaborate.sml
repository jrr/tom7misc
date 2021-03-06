
structure Elaborate :> ELABORATE =
struct

  infixr 9 `
  fun a ` b = a b

  structure V = Variable
  structure C = Context
  structure E = EL

  open IL
  open ElabUtil

  exception Impossible

  (* XXX elabutil *)
  fun mkfn (args, dom, cod, body) =
      let val f = V.namedvar "fn"
      in
          Let(Fix(Mono[{name=f,
                        arg=args,
                        dom=dom,
                        cod=cod,
                        body=body}]),
              Var f)
      end


  fun elabt ctx loc t =
      (case t of
         E.TVar str =>
             ((case C.con ctx str of
                   (0, Typ t, _) => t
                 | (kind, _, _) => 
                       error loc (str ^ " expects " ^ 
                                  itos kind ^ " type argument"
                                  ^ (if kind = 1 then "" else "s") ^ "."))
                   handle C.Absent => error loc ("Unbound type name " ^ str))
       | E.TApp (l, str) =>
             ((case C.con ctx str of
                   (kind, Lambda f, _) =>
                       if kind = length l 
                       then f (map (elabt ctx loc) l) 
                       else error loc 
                           (str ^ " expects " ^ itos kind ^ " type argument"
                            ^ (if kind = 1 then "" else "s") ^ " -- gave " ^
                            itos (length l))
                 | _ => error loc (str ^ " is not a type constructor."))
                   handle Context.Absent => 
                       error loc ("Unbound type constructor " ^ str))
       | E.TRec ltl => let 
                           val ltl = ListUtil.sort 
                                     (ListUtil.byfirst String.compare) ltl
                       in 
                           if ListUtil.alladjacent
                                (ListUtil.byfirst op<>) ltl 
                           then TRec (ListUtil.mapsecond (elabt ctx loc) ltl)
                           else error loc "Duplicate label in record type"
                       end
       | E.TArrow (dom, cod) => Arrow (false, 
                                       [elabt ctx loc dom], 
                                       elabt ctx loc cod))

  and elab ctx ((e, loc) : EL.exp) =
    let fun %x = (x, loc)
    in
      case e of
          E.Seq (e1, e2) => 
              let val (e1, e1t) = elab ctx e1
                  val (e2, e2t) = elab ctx e2
              in (Seq(e1, e2), e2t)
              end

        (* could be primop, but we want to produce a special
           IL form, not a primop (XXX reconsider this,
           versus pattern matching the specific primop in
           tocps) *)
        | E.Submit e => 
              let
                  val (ee, t) = elab ctx e
                  val tv = new_evar ()
              in
                  unify ctx loc "submit" t (Arrow(false, [TRec nil], tv));
                  (IL.Submit ee, Task tv)
              end

        (* same as submit *)
        | E.Spawn e => 
              let
                  val (ee, t) = elab ctx e
                  val tv = new_evar ()
              in
                  unify ctx loc "submit" t (Arrow(false, [TRec nil], tv));
                  (IL.Spawn ee, Task tv)
              end

        | E.Syncall e => 
              let
                  val (ee, t) = elab ctx e
                  val tv = new_evar ()
              in
                  unify ctx loc "syncall" t (TVec ` Task tv);
                  (IL.Syncall ee, TVec tv)
              end


(*----------------------------------------------------------------------------*)
        | E.Var vv => 
          ((case C.var ctx vv of
            (pt, _, Primitive p) =>
              let 
                  fun unpoly (Quant (v, pt)) ts = unpoly pt (v::ts)
                    | unpoly (Mono (Arrow(_, dom, cod))) ts =
                      let 
                          (* turn array into
                             (where 'a means an evar)

                             let fun array_105 (n : int, init : 'a) 
                                 : 'a vec = 
                                   Primapp(array, [n, init], ['a])
                             in
                                 array_105
                             end

                             ... it will be inlined in the cps
                             optimizer if applied directly to 
                             arguments. *)

                          (* XXX use elabutil.evarize *)
                          val x = V.namedvar "pa"

                          fun mkes nil = (nil, V.Map.empty)
                            | mkes (tv::rest) =
                              let val (ets, subst) = mkes rest
                                  val ev = new_evar ()
                              in
                                  (ev :: ets, 
                                   V.Map.insert (subst, tv, ev))
                              end

                          val (ets, subst) = mkes ts

                          val ndom = map (Subst.tsubst subst) dom
                          val ncod = Subst.tsubst subst cod


                          fun tuple l =
                              let
                                  fun mktup _ nil = nil
                                    | mktup n (h::t) = 
                                      (Int.toString n, h) :: 
                                          mktup (n + 1) t
                              in
                                  IL.TRec(mktup 1 l)
                              end

                      in 
                          case ndom of
                              [_] =>
                                (* single argument. function will
                                   just take that. *)
                                (mkfn ([x],
                                       ndom,
                                       ncod,
                                       Primapp(p,
                                               [Var x],
                                               ets)),
                                 Arrow(false, ndom, ncod))

                            | _ => 

                                (* many arguments -- function will
                                   take a tuple of them *)
                                (mkfn 
                                 ([x], [tuple ndom], ncod,
                                  let
                                      fun getargn n =
                                          Proj(Int.toString (n + 1),
                                               tuple ndom,
                                               Var x)
                                  in
                                      Primapp(p,
                                              List.tabulate
                                              (length ndom, getargn),
                                              ets)
                                  end),
                                 Arrow(false, [tuple ndom], ncod))
                      end
                    (* preclude prims of type Aa.a *)
                    | unpoly (Mono (TVar any)) nil =
                          (Primapp (p, nil, nil), TVar any)
                    | unpoly _ _ = error loc 
                          "BUG: Bad type of primop."
              in
                  unpoly pt nil
              end

          | (pt, v, i) =>
              let
                  (* If polymorphic, instantiate the variable's 
                     forall-quantified type variables with new 
                     evars. Use type application on the expression 
                     to record this. *)
                  fun inst (Mono t) = (Var v, V.Map.empty, t)
                    | inst (Quant (tv, t)) =
                      let val (e, subst, ty) = inst t
                          val ev = new_evar ()
                      in (Appt (e, ev), 
                          V.Map.insert (subst, tv, ev), ty)
                      end

                  val (ee, subst, tt) = inst pt
              in
                  (ee, Subst.tsubst subst tt)
              end) handle C.Absent => 
                error loc ("Unbound identifier: " ^ vv))
        | E.Constant(E.CInt i) => (Int i, Initial.ilint)
        | E.Constant(E.CString s) => (String s, Initial.ilstring)
        | E.Constant(E.CChar c) => (Char c, Initial.ilchar)

        | E.Float _ => error loc "Unsupported: floating point"

        | E.Vector nil => 
                    let
                        val t = new_evar()
                    in
                        (Primapp(Primop.PArray0, nil, [t]), TVec t)
                    end

        (* derived form *)
        | E.Vector (first::rest) =>
               let
                   (* [| x, y, z |]

                      becomes

                      let val a = array (3, x)
                      in
                          update(array, 1, y);
                          update(array, 2, z);
                          a
                      end *)

                   fun $x = (x, loc)
                   val n = 1 + length rest
                   val arr = newstr "arr"

                   fun dowrites _ nil e = e
                     | dowrites m (h::t) e =
                       $ ` 
                       E.Seq
                       ($ `
                        E.App($ ` E.Var "update",
                              $ ` 
                              E.Record
                              [("1", $ ` E.Var arr),
                               ("2", $ ` E.Constant ` E.CInt m),
                               ("3", h)]),
                        dowrites (m + 1) t e)
               in
                   elab ctx `
                   $ `
                    E.Let
                    ($ `
                     E.Val (nil, E.PVar arr,
                            $ `
                            E.App ($ ` E.Var "array",
                                   $ `
                                   E.Record
                                   [("1", $ ` E.Constant ` E.CInt n),
                                    ("2", first)])),
                     dowrites 1 rest ` $ ` E.Var arr)
               end

        | E.Record lel =>
               let
                   val letl = map (fn (l, e) => (l, elab ctx e)) lel
                   val _ = ListUtil.alladjacent (ListUtil.byfirst op<>) 
                              (ListUtil.sort 
                               (ListUtil.byfirst String.compare) lel)
                           orelse error loc 
                              "Duplicate labels in record expression"
               in
                   (Record (map (fn (l, (e, t)) => (l, e)) letl),
                    TRec (map (fn (l, (e, t)) => (l, t)) letl))
               end

        | E.Proj (s, t, e) =>
               let
                   val (ee, tt) = elab ctx e
                   val ttt = elabt ctx loc t
               in
                   unify ctx loc "proj" tt ttt;
                   case ttt of
                       TRec ltl =>
                           (case ListUtil.Alist.find op= ltl s of
                               NONE => error loc 
                                         ("label " ^ s ^ 
                                          " not in projection object type!")
                             | SOME tres => (Proj(s, ttt, ee), tres))
                     | _ => error loc "projection must be of record type"

               end

        (* XXX oops, source language is not n-ary, so this is kind of dumb 
           (but not wrong) the way I do this: *)
        | E.App(e1, e2) => 
               let
                   val (ff, ft) = elab ctx e1
                   val (aa, at) = ListPair.unzip (map (elab ctx) [e2])
                   val dom = map (fn _ => new_evar ()) aa
                   val cod = new_evar ()
               in
                   (* XXX check same length *)
                   unify ctx loc "function" ft (Arrow (false, dom, cod));
                   ListPair.app (fn (a, d) => unify ctx loc "argument" a d)
                                (at, dom);
                   (App (ff, aa), cod)
               end

        | E.Constrain(e, t) =>
               let 
                   val (ee, tt) = elab ctx e
                   val tc = elabt ctx loc t
               in
                   unify ctx loc "constraint" tt tc;
                   (ee, tc)
               end

        | E.Andalso (a,b) =>
               elab ctx (E.If (a, b, Initial.falseexp loc), loc)

        | E.Orelse (a,b) =>
               elab ctx (E.If (a, Initial.trueexp loc, b), loc)

        | E.Andthen (a, b) => 
               elab ctx (E.If (a, (E.Seq (b, (E.Record nil, loc)), loc),
                               (E.Record nil, loc)), loc)

        | E.Otherwise (a, b) => 
               elab ctx (E.If (a, 
                               (E.Record nil, loc),
                               (E.Seq (b, (E.Record nil, loc)), loc)), loc)

        | E.If (cond, tt, ff) =>
               elab ctx 
               (E.Case ([cond],
                        [([Initial.truepat], tt),
                         ([Initial.falsepat], ff)]), loc)

        | E.Case (es, m) =>
               let 
                   fun def () =
                       let in
                           (* XXX doesn't work because we sometimes 
                              force def in order to hoist it into a 
                              function *)
                           (*
                           print ("Warning: Inexhaustive match at " ^ 
                                  ltos loc ^ "\n");*)
                           (EL.Raise (Initial.matchexp loc), loc)
                       end

                   (* force case args to be variables, if they aren't. *)
                   fun force nil nc acc =
                            Pattern.elaborate true elab elabt nc loc
                                 (rev acc, m, def)
                     | force ((E.Var v, _)::rest) nc acc = 
                            force rest nc (v::acc)
                     | force (e::rest) nc acc =
                            let
                                val (ee, tt) = elab ctx e
                                val s = newstr "case"
                                val sv = V.namedvar s
                                val nctx = C.bindv ctx s (Mono tt) sv
                                val (ein, tin) = force rest nctx (s::acc)
                            in
                                (Let(Val(Mono(sv, tt, ee)),
                                     ein), tin)
                            end
               in
                   force es ctx nil
               end

        | E.Raise e =>
            (case C.con ctx Initial.exnname of
                 (0, Typ exnt, Extensible) =>
                     let 
                         val (ee, tt) = elab ctx e
                         val ret = new_evar ()
                     in
                         unify ctx loc "raise" tt exnt;
                         (Raise (ret, ee), ret)
                     end
               | _ => error loc "exn type not declared???")

        | E.Handle (e1, pel) =>
            (case C.con ctx Initial.exnname of
              (0, Typ exnt, Extensible) =>
                let
                    val es = newstr "exn"
                    val ev = V.namedvar es
                        
                    val (ee, tt) = elab ctx e1
                        
                    val mctx = C.bindv ctx es (IL.Mono exnt) ev

                    (* re-raise exception if nothing matches *)
                    fun def () =
                        (EL.Raise (EL.Var es, loc), loc)
                        
                    val (match, mt) = 
                        Pattern.elaborate true elab elabt mctx loc
                           ([es], ListUtil.mapfirst ListUtil.list pel, def)
                in
                    unify ctx loc "handle" tt mt;
                    (Handle(ee, ev, match), tt)
                end
            | _ => error loc "exn type not declared???")

        (* makes slightly nicer code *)
        | E.Let ((E.Do e, loc), e2) => elab ctx (E.Seq(e, e2), loc)

        | E.Let (d, e) =>
               let
                   val (dd, nctx) = elabd ctx d
                   val (ee, t) = elab nctx e
               in
                   (foldr Let ee dd, t)
               end

    end

  and mktyvars ctx tyvars =
      foldl (fn (tv, ctx) => C.bindc ctx tv (Typ ` new_evar ()) 0 Regular)
            ctx tyvars

  and elabf ctx 
            (arg : string)
            (clauses : (EL.pat list * EL.typ option * EL.exp) list) 
            loc =
      let in
          (* ensure clauses all have the same length *)
          ListUtil.allpairssym (fn ((a, _, _), (b, _, _)) =>
                                length a = length b) clauses
               orelse error loc 
                  "clauses don't all have the same number of curried args";

          (* now we make big nested function *)

          (* p11 p12 p13 ... : t1 = e1 
             p21 p22 p23 ... : t2 = e2
              ...

             becomes:

             let fun f2 (x2) =
                 let fun f3 (x3) = ...

                     (case x , x2 , x3 of
                        p11 p12 p13 => e1
                        p21 p22 p23 => e2
                          ...
                        _ _ _ => raise Match)
                     : t1 : t2 : ... : tn 


                 in f3
                 end
             in f2 
             end

             *)
          (case clauses of
            [([pat], to, e)] =>
              let
                  (* base case *)
                  val (exp, tt) = 
                      Pattern.elaborate true elab elabt ctx loc
                         ([arg],
                          [([pat], e)],
                          (fn () =>
                           let in
                               print ("warning: match non-exhaustive at " ^
                                      Pos.toString loc ^ "\n");
                               (E.Raise (Initial.matchexp loc), loc)
                           end))

              in
                  (case to of
                       SOME t => unify ctx loc 
                                   "codomain type constraint on fun" tt
                                   (elabt ctx loc t)
                     | _ => ());
                  (exp, tt)
              end
          | nil => raise Elaborate "impossible: *no* clauses in fn"
          | _ =>
               let
                   (* we already have an arg since we're inside the
                      function body, so that's where all this 'tl'
                      stuff comes from *)
                   val args = arg :: 
                       map (fn p =>
                            (case p of
                                 E.PVar s => newstr s
                               | E.PAs (s,_) => newstr s
                               | _ => newstr "cur")) ` tl ` #1 ` hd clauses

                   (* all constraints on fun body *)
                   val constraints =
                       List.mapPartial #2 clauses

                   val columns = map (fn (pl, _, e) => (pl, e)) clauses

                   fun buildf nil =
                       (* build the case, slapping all of the
                          body constraints on its outside *)
                       foldr (fn (t, e) => (E.Constrain(e, t), loc)) 
                             (E.Case (map (fn a => (E.Var a, loc)) args, 
                                       columns), loc)
                             constraints
                     | buildf (x::rest) =
                       let
                           val fc = newstr "fc"
                       in
                           (E.Let((E.Fun [(nil, fc, [([E.PVar x], NONE,
                                                      buildf rest)])], loc),
                                  (E.Var fc, loc)),
                            loc)
                       end
               in
                   elab ctx ` buildf ` tl args
               end)

      end handle Pattern.Pattern s => 
            error loc ("Pattern compilation failed: " ^ s)


  (* return a new context, and an il.dec list *)
  and elabd ctx ((d, loc) : EL.dec) =  
    case d of
      E.Do e => ([Do ` #1 ` elab ctx e], ctx)
    | E.Type (nil, tv, typ) =>
          let val t = elabt ctx loc typ
          in ([], C.bindc ctx tv (Typ t) 0 Regular)
          end

    | E.Tagtype t =>
          let
              val tv = V.namedvar t
          in
              ([Tagtype tv], C.bindc ctx t (Typ (TVar tv)) 0 Extensible)
          end

    (* some day we might add something to 'ty,' like a string list
       ref so that we can track the exception's history, or at least
       a string with its name and raise point. *)
    | E.Exception (e, ty) => elabd ctx (E.Newtag(e, ty, Initial.exnname), loc)

    | E.Newtag (tag, dom, ext) =>
       (case C.con ctx ext of
          (0, Typ (cod as TVar ev), Extensible) =>
            let
                (* need to generate the tag as well as
                   a constructor function *)
                val tagv = V.namedvar (tag ^ "_tag")
                val d = elabt ctx loc 
                     (case dom of
                          NONE => 
                              error loc 
                              "all tags must be carriers. specify 'of unit'"
                        | SOME x => x)

                val ctor = V.namedvar tag
                val carg = V.namedvar "tagarg"

                (* don't put the tag in the context, since
                   user code should never access it. *)
                (* XXX not total for exns if we add locality
                   info *)
                val nctx = C.bindex ctx tag 
                            (Mono ` Arrow(true, [d], cod))
                            ctor
                            ` Tagger tagv

            in
                ([Newtag (tagv, d, ev),
                  Fix ` Mono
                      [{ name = ctor, arg = [carg],
                         dom = [d], cod = cod,
                         body = Tag(Var carg, Var tagv) }]],
                 nctx)
            end
        | _ => error loc (ext ^ " is not an extensible type"))

    | E.Datatype (atvs, unsorted) =>
          let
            (* datatype (a, b, c) t = A of t | B of a | C of t1
               and                u = D of u | E of b | F

               syntax enforces uniformity restriction.

                              tyvars       type
               Datatype of  string list * (string * 
                              ctor      members
                            (string * typ option) list) list
               *)

              (* put in sorted order. *)
              val dl =
                  ListUtil.sort (ListUtil.byfirst String.compare) unsorted

              (* check: no duplicate datatype names *)
              val _ =
                  ListUtil.alladjacent
                     (fn ((a, _), (b, _)) => a <> b) dl
                  orelse error loc "duplicate type names in datatype decl"

              (* check: no duplicate tyvars *)
              val _ =
                  ListUtil.alladjacent op <> `
                    ListUtil.sort String.compare atvs
                  orelse error loc "duplicate type vars in datatype decl"
                  
              (* check: no overlap between tyvars and datatypes *)
              val _ =
                  ListUtil.alladjacent op <> `
                    ListUtil.sort String.compare (atvs @ map #1 dl)
                  orelse error loc 
                    "tyvar and datatype share same name in datatype decl"

              (* check: arms are SOME (rewriting occurred earlier) *)
              (* XXX: actually, I don't do the rewriting yet
                 so you need to write:  | NONE of unit *)

              val dl =
                  ListUtil.mapsecond
                     (map (fn (ctor, SOME stuff) => (ctor, stuff)
                           |  _ => error loc 
                                      "all datatypes must be carriers")) dl

              (* check: no duplicated constructors *)
              val _ =
                  app (fn (dt, arms) =>
                       let
                           val sorted =
                               ListUtil.sort (ListUtil.byfirst 
                                              String.compare) arms
                       in
                           ListUtil.alladjacent
                             (fn ((a,_),(b,_)) => a <> b) sorted
                           orelse error loc
                                  ("duplicated constructor in datatype " ^
                                   dt); ()
                       end) dl

              (* augment with index and recursive var *)
              val dl =
                  ListUtil.mapi (fn ((dt, arms), n) =>
                                 (dt, arms, n,
                                  V.namedvar (dt ^ "_" ^ itos n ^ "_"))) dl

              (* augment atvs with real variables too *)
              val atvs = map (fn x => (x, V.namedvar x)) atvs

                  
              (* put tyvars in context for arms *)
                  
              val actx = 
                  foldl (fn ((s, x),c) =>
                         C.bindc c s (Typ (TVar x)) 0 Regular) ctx atvs

              (* bind each datatype name to the corresponding
                 recursive variable for the purpose of elaborating
                 the arms. *)

              val actx = 
                  foldl (fn ((dt, _, _, v), c) =>
                         C.bindc c dt (Typ (TVar v)) 0 Regular) actx dl

              (* elaborate each arm *)
              val dl =
                  map (fn (dt, arms, n, v) =>
                       (dt, 
                        ListUtil.mapsecond (elabt actx loc) arms,
                        n, v)) dl

              (* make body of mu *)
              val mubod =
                  map (fn (_, arms, _, v) => (v, Sum arms)) dl
                  
              (* make il con for each, consuming n *)
              val dl = 
                  map (fn (dt, arms, n, v) =>
                       (dt, arms, Mu(n, mubod), v)) dl
                       

              (* outside the mu, we need to substitute the mu
                 itself for the datatype type variable. *)
              val musubst =
                  Subst.tsubst `
                  Subst.fromlist
                  (map (fn (_, _, mu, v) =>
                        (v, mu)) dl)

              (* don't need v any more *)
              val dl = map (fn (dt, arms, mu, _) => (dt, arms, mu)) dl

              (* generate the wrapper Lambda that binds the
                 tyvars *)

              val kind = length atvs

              (* XXX from here on I only use #2 of atvs... *)

              fun wrapper body =
                  let
                      fun bs nil nil s = s
                        | bs ((_,v)::vrest) (tt::trest) s =
                          bs vrest trest ` V.Map.insert (s, v, tt)
                        | bs _ _ _ =
                          error loc 
                            "wrong number of type arguments to datatype"
                  in
                      Lambda (fn tl => 
                              Subst.tsubst (bs atvs tl V.Map.empty) body)
                  end

              (* setup done. now generate the new context and decls. *)

              (* first add the datatypes *)
              val nctx = 
                  foldl
                   (fn ((dt, _, mu), c) =>
                    C.bindc c dt (wrapper mu) kind Regular) ctx dl

              (* now bind all the constructors *)

              val atvs = map #2 atvs


              (* generate a list:
                 (ctor string, ctor var, ctor arrow type,
                  ctor decl) *)
              val ctors = 
                  List.concat `
                  map (fn (dt, arms, mu) =>
                       map (fn (ctor, ty) =>
                            let 
                                val dom = musubst ty

                                val cty = 
                                    foldl Quant
                                      (Mono 
                                       (Arrow 
                                        (true (* yes total! *), 
                                         [dom], mu)))
                                      atvs
                                val ctorf = V.namedvar "ctor"
                                val x = V.namedvar "xdt"
                            in
                                (ctor, ctorf,
                                 (* type of constructor *)
                                 cty,
                                 (* injection value *)
                                 Fix `
                                   foldl Quant
                                    (Mono `
                                     [{ name = ctorf,
                                        dom = [dom],
                                        cod = mu,
                                        arg = [x],
                                        body =
                                        Roll(mu,
                                             Inject
                                             (Sum arms, ctor, Var x))}])
                                    atvs)
                            end) arms) dl

              (* bind the constructors *)
              val nctx =
                  foldl
                  (fn ((ctor, v, at, _),c) =>
                   C.bindex c ctor at v Constructor) nctx ctors

          in
              (map #4 ctors, nctx)
          end

    | E.Type (tyvars, tv, typ) =>
          let
              (* XXX I can say type a t = bogus and it
                 will be accepted unless I later do
                 "int t". Should make a provisional
                 call to the lambda, with the tyvars bound
                 to evars or something, in order to
                 trigger an exception if the body is
                 bogus. *)

              val kind = length tyvars

              val con =
                  Lambda 
                  (fn l =>
                   if length l <> kind
                   then error loc "(bug) wrong number of args to Lambda"
                   else 
                       let val nc = 
                           ListPair.foldl 
                              (fn (tv, t, ctx) =>
                               C.bindc ctx tv (Typ t) 0 Regular) 
                              ctx (tyvars, l)
                       in
                           elabt nc loc typ
                       end)
          in
              ListUtil.allpairssym op<> tyvars 
                 orelse error loc "duplicate tyvars in type dec";
              ([], C.bindc ctx tv con kind Regular)
          end

    | E.Fun bundle =>
          let

              val outer_context = ctx

              val  _ = List.null bundle 
                         andalso error loc "BUG: *no* fns in bundle?"

              val _ = ListUtil.allpairssym (fn ((_, f, _), (_, g, _)) =>
                                            f <> g) bundle
                         orelse error loc 
                             "duplicate functions in fun..and"

              (* make var for each fn *)
              val binds =
                  map (fn (tv, f, b) => 
                       let val vv = V.namedvar f
                           val dom = new_evar ()
                           val cod = new_evar ()
                       in
                           (tv, f, b, vv, dom, cod)
                       end) bundle

              (* to process the body of one function, we
                 bind the existential variables that appeared
                 before the function name, and bind all functions
                 at (mono) existential arrow type. *)
              fun onectx c tv =
                  let
                      val nc = 
                          foldl (fn ((_, f, _, vv, dom, cod), ct) =>
                                 C.bindv ct f (Mono (Arrow(false, 
                                                            [dom],
                                                            cod))) vv)
                                c binds
                  in
                      mktyvars nc tv
                  end

              (* for each function, elaborate and return:
                 name, var,
                 arg variable,
                 domain type
                 codomain type
                 il expression body *)
              fun onef (tv, f, clauses, vv, dom, cod) =
                  let
                      val c = onectx ctx tv
                      val x = newstr "x"
                      val xv = V.namedvar x
                      val nc = C.bindv c x (Mono dom) xv

                      val (exp, tt) = elabf nc x clauses loc
                  in
                      unify c loc "fun body/codomain" tt cod;
                      (f, vv, xv, dom, cod, exp)
                  end

              (* collect up elaborated functions and
                 forall-quantified vars *)
              fun folder ((f, vv, x, dom, cod, exp),
                          (fs, efs, polys)) =
                  let
                      val (dom, dp) = polygen outer_context dom
                      val (cod, cp) = polygen outer_context cod
                  in
                      ({ name = vv,
                         arg = [x],
                         dom = [dom],
                         cod = cod,
                         body = exp } :: fs, 
                       (f, vv, Arrow(false, [dom], cod)) :: efs,
                       dp @ cp @ polys)
                  end

              val (fs, efs, ps) = 
                  foldl folder (nil, nil, nil) ` map onef binds

              fun mkpoly nil x = Mono x
                | mkpoly (v::rest) x = Quant (v, mkpoly rest x)

              (* rebuild the context with these functions
                 bound polymorphically *)
              fun mkcontext ((f, vv, at), cc) =
                  C.bindv cc f (mkpoly ps at) vv

          in
              ([Fix ` mkpoly ps fs], foldl mkcontext ctx efs)
          end

    | E.Val (tyvars, pat, exp) =>
          let
              (* simply bind tyvars;
                 let generalization actually determine the type. 
                 if we want to have these type vars act like SML,
                 we can check after the decls are over that each
                 one is still free and generalizable. 
                 XXX we let these sit in the exported context --
                     something needs to be done about that!
                 *)
              val nctx = mktyvars ctx tyvars

              fun epat ctx EL.PWild (Var _) tt = ([], ctx)
                | epat ctx EL.PWild ee tt = ([Do ee], ctx)
                | epat ctx (EL.PVar v) ee tt =
                  (* Did you know val x = e is just syntactic sugar 
                     for val x as _ = e ? *)
                  epat ctx (EL.PAs (v, EL.PWild)) ee tt
                | epat ctx (EL.PConstrain (p, t)) ee tt =
                  let in
                      unify ctx loc "val constraint" tt 
                         ` elabt ctx loc t;
                      epat ctx p ee tt
                  end
                | epat ctx (EL.PAs (v, p)) ee tt =
                  let

                      (* [val (x as p) = e]
                           is
                         val x = e     @
                         [val p = x]
                         *) 

                      val (ee, bound, t) = generalize ctx ee tt
                      val vv = V.namedvar v
                      val ctx = C.bindv ctx v 
                                   (foldr Quant (Mono t) bound) vv
                      val (ds, c) = epat ctx p (Var vv) tt
                  in
                      ([Val ` foldr Quant (Mono (vv, t, ee)) bound] @ ds,
                       c)
                  end
                | epat ctx (EL.PRecord spl) (ee as (Var v)) tt =
                  let
                      val tys = map (fn (s,_) => (s,new_evar ())) spl

                      (* recursively elaborate a 
                             val p = #label record_var
                         for each component *)
                      fun f ctx nil nil = (nil, ctx)
                        | f ctx ((s,p)::rest) ((_,t)::trest) =
                          let 
                              val (ds, c) = epat ctx p 
                                              (Proj (s, tt, ee)) t
                              val (rds, rc) = f c rest trest
                          in
                              (ds @ rds, rc)
                          end
                        | f _ _ _ = raise Impossible
                  in
                      (* make sure rhs has record type 
                         with the right fields *)
                      unify ctx loc "record pattern" tt (TRec tys);
                      f ctx spl tys
                  end
                | epat ctx (p as (EL.PRecord spl)) ee tt =
                  (* use as to bind tuple to a variable, 
                     then enter case above *)
                  epat ctx (EL.PAs (newstr "rec", p)) ee tt
                | epat _ _ _ _ = 
                    error loc "patterns in val dec must be irrefutable"

              val (ee, tt) = elab ctx exp
          in
              epat nctx pat ee tt
          end

end
