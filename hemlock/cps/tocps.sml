
(* Convert to Continuation Passing Style.
   The algorithm implemented is more-or-less the
   same as in "Compiling With Continuations" (Appel),
   though I have made minor changes along with changing
   the datatype. (I also pass continuations as the
   first argument to a function rather than the last.)

   Everything happens in "convert".
*)

(* XXX get rid of mutually-recursive fixes where they are not
   needed. It would be good if cpsopt could do this for us,
   but it doesn't yet *)

structure ToCPS :> TOCPS =
struct

  (* There were getting to be too many parens... *)
  infixr 9 `
  fun a`b = a b
  fun list x = [x]

  structure V = Variable
  structure I = IL
  structure LU = ListUtil
  open CPS
  open Primop

  val itos = Int.toString

  exception CPS of string

  local
    structure SM = StringMap
    val db = ref SM.empty : int SM.map ref
    val ctr = ref 0
    fun ++ c = (c := !c + 1; !c)

    val uniquesum = Params.flag true 
      (SOME ("-uniquesum", 
             "Generate unique tags for each sum label")) "uniquesum"

  in

    fun clear () = (ctr := 0; db := SM.empty)

    fun genlabel l ltl =
      case LU.position (fn (s,_) => s = l) ltl of
        NONE => NONE
      | SOME n =>
          if !uniquesum
          then SOME(case SM.find (!db, l) of
                      NONE => let val this = ++ctr 
                              in 
                                db := SM.insert(!db, l, this);
                                SymbolDB.push "sumlabel" this l;
                                this
                              end
                    | SOME i => i)
          else SOME n
  end


  (* XXX -- keep low now to detect bugs.
     with current TAL backend there is no real limit,
     so it might as well be quite high! (Unless we
     think that tuples are rebuilt frequently, in which
     case being more like lists makes them more efficient) *)
  val MAXRECORD = 3

  fun error s = (print s; print "\n"; raise CPS s)

  (* strip off quantifiers, not needed here... *)
  fun losepoly (I.Mono a) = a
    | losepoly (I.Quant (_, a)) = losepoly a

  fun unevar (IL.Evar (ref (IL.Bound tt))) = unevar tt
    | unevar t = t

  fun resolvetype G (IL.TVar v) = error "unimplemented: type vars"
    | resolvetype _ t = unevar t

  (* XXX this is crappy because we generate all the decls that occur in
     wrap, like exn tags. But what's another way? *)

  val iltrue = #1 ` Elaborate.elab Initial.initial ` 
                       Initial.wrap ` Initial.trueexp Pos.initpos
  val ilfalse = #1 ` Elaborate.elab Initial.initial ` 
                       Initial.wrap ` Initial.falseexp Pos.initpos


  (* convert does all of the work.
     It's passed an IL context G (used to look up types to do
     "type directed translation", for instance), a continuation
     c, and an IL expression. The IL expression is translated
     into a CPS value (usually a variable), and that is passed
     to c. *)

  (* nb: redefined below to pass initial context empty *)
  fun convert G c (I.Var v) = c ` Var v
    | convert G c (I.Int i) = c ` Int i
    | convert G c (I.Char ch) = c ` Int ` ord ch
    | convert G c (I.String s) = c ` String s
    | convert G c (I.Record lel) =
      let
          (* first, sort the fields alphabetically. *)
          val lel = LU.sort (LU.byfirst String.compare) lel
          (* now, maybe chain based on the value of MAXRECORD. In
             the process, strip off labels, which we won't use now.
             Note that we only process the first MAXRECORD (or MR-1)
             fields, leaving the rest to recursion...
             *)
          fun split l =
              if length l > MAXRECORD then
                  map (fn(_,e)=>e) (List.take (l, MAXRECORD - 1)) @
                  [I.Record ` List.drop (l, MAXRECORD - 1)]
              else map (fn(_,e)=>e) l

          val lel = split lel

          val v = V.namedvar "record"

          (* continuation after cpsing arguments. Just allocate away... *)
          fun k values = Alloc(TUPLE ` length lel, values, v, c ` Var v)
      in
          convertl G k lel
      end
    | convert G c (I.Proj (l, t, e)) =
      let
       val v = V.namedvar "proj"

       (* this is only tricky because we may need to chain projections. *)
       fun k value =
           case resolvetype G t of
             IL.TRec lel =>
                let
                    val lel = LU.sort (LU.byfirst String.compare) lel
                in 
                  case LU.position (fn (s,_) => s = l) lel of
                    NONE => 
                     error 
                     "Label in projection is not in the type!!"
                  | SOME i =>
                     let
                         (* generate a series of projections to 
                            reach the tail. invt n < sz *)
                         fun gproj (value, sz) n =
                             let in
(*
                                 print ("gproj from " ^ 
                                        CPSPrint.vtos value ^ 
                                        " #" ^ itos n ^ " size = " ^
                                        itos sz ^ "\n");
*)
                             if sz <= MAXRECORD
                             then (* standard representation *)
                                 Project(n, value, v, c ` Var v)
                             else if n < MAXRECORD - 1
                                  then (* has chain, but don't need it *)
                                      Project(n, value, v, c ` Var v)
                                  else (* get chain, recurse *)
                                      let val vt = V.namedvar "chain"
                                          val skip = MAXRECORD - 1
                                      in
                                          Project(skip,
                                                  value, vt,
                                                  gproj (Var vt,
                                                         sz - skip)
                                                    (n - skip))
                                      end
                             end
                     in
                         gproj (value, length lel) i
                     end
                end
         | _ => error "Type in projection is not record, or is unresolved!"
      in
          convert G k e
      end
    | convert G c (I.Seq (e1, e2)) = convert G c (I.Let(I.Do e1, e2))
    | convert G c (I.Let(I.Do e1, e2)) = 
      let fun k _ = convert G c e2
      in convert G k e1
      end
    | convert G c (I.Let(I.Val vtep, e2)) =
      let 
          val (v, _, e) = losepoly vtep

          fun k value = Primop(PBind, list value, list v, 
                               list ` convert G c e2)
      in
          convert G k e
      end
    (* Target language is untyped, so ... *)
    | convert G c (I.Unroll e) = convert G c e
    | convert G c (I.Roll (_, e)) = convert G c e
    | convert G c (I.Appt (e, _)) = convert G c e
    | convert G c (I.Inject (t, l, e)) =
      (case resolvetype G t of
           I.Sum ltl =>
               let val ltl = LU.sort (LU.byfirst String.compare) ltl
               in case genlabel l ltl of
                   NONE => error "Label in inject not part of type!!"
                 | SOME n =>
                       let
                           val v = V.namedvar "inject"
                           fun k value = Alloc(INT_T n, list value, v, 
                                               c ` Var v)
                       in
                           convert G k e
                       end
               end
         | _ => error "Type in inject is not a sum, or is unresolved!")
    | convert G c (I.Get e) =
      let
          val v = V.namedvar "get"
          fun k value = Primop(PGet, list value, list v, list ` c ` Var v)
      in convert G k e
      end
    | convert G c (I.Set (e1, e2)) =
      let
          val v = V.namedvar "result_unit"
          fun kl lvalue =
              let fun kr rvalue = 
                  Alloc(TUPLE 0, nil, v, 
                        Primop(PSet, [lvalue, rvalue], nil, list ` c ` Var v))
              in convert G kr e2
              end
      in
          convert G kl e1
      end

    | convert G c (I.Sumcase (t, ob, v, lel, def)) =
      (* straightforward translation to CPS sumcase,
         but need to reify the continuation because it
         will be referenced in each arm and the default. *)
      (case resolvetype G t of
           I.Sum ltl =>
               let
                   val ltl = LU.sort (LU.byfirst String.compare) ltl

                   val lel = LU.sort (LU.byfirst String.compare) lel
                   val _ =
                       LU.alladjacent (LU.byfirst op <>) lel
                       orelse 
                       error "bug: duplicate case labels in sumcase"

                   fun tagn l =
                       case genlabel l ltl of
                           NONE => 
                               error 
                               "label in sum arm is not in the type!!"
                         | SOME n => n

                   fun k value =
                       let val x = V.namedvar "caseresult"
                           val f = V.namedvar "casejoin"
                           fun jumpback onearm = App(Var f, [onearm])
                           val arms = map (fn (l, e) => 
                                           (tagn l, 
                                            convert G jumpback e)) lel
                       in
                           Fix([(f, list x, c `Var x)],
                               Sumswitch(value, v, arms, 
                                         convert G jumpback def))
                       end
               in
                   convert G k ob
               end
         | _ => error "type in sumcase is not sum or is unresolved!")
    | convert G c (I.App (e, el)) =
           let
               fun k function =
                   let
                       val r = V.namedvar "ret"
                       val x = V.namedvar "retarg"
                       fun kk args =
                           Fix([(r, list x, c ` Var x)],
                               App(function, Var r :: args))
                   in
                       convertl G kk el
                   end
           in
               convert G k e
           end
    | convert G c (I.Let(I.Fix polyfns, e)) =
           let
               val fns = losepoly polyfns
               fun onefn {name, arg, dom=_, cod=_, body} = 
                   let val w = V.namedvar "fret"
                       fun k r = App(Var w, list r)
                   in (name, w :: arg, convert G k body)
                   end
           in
               Fix(map onefn fns, convert G c e)
           end
    (* no type checking, so we don't care *)
    | convert G c (I.Let(I.Tagtype _, e)) = convert G c e

    (* we generate these randomly, so make a pair. 
       1 in 2^32 odds are pretty poor ... *)
    | convert G c (I.Let(I.Newtag (tag, _, _), e)) =
           let
               val tagl = V.namedvar "tagl"
               val tagr = V.namedvar "tagr"
           in
               Primop
               (PNewtag, nil, [tagl],
                list `
                Primop
                (PNewtag, nil, [tagr],
                 list `
                 Alloc(TUPLE 2,
                       [Var tagl, Var tagr],
                       tag,
                       convert G c e)))
           end

    (* unlike sumcase, we can't ever hope to do a clever job with
       tagcase. So just unroll the comparisons right here. *)
    | convert G c (I.Tagcase (t, obj, var, (v,e)::rest, def)) =
           (* v is bound to a pair of integers.
              obj is (tag, data). if v = tag, then
              run e with var = data. else, continue ... *)
           let
               val vl1 = V.namedvar "tagcasel1"
               val vl2 = V.namedvar "tagcasel2"

               val vr1 = V.namedvar "tagcaser1"
               val vr2 = V.namedvar "tagcaser2"

               val tag = V.namedvar "tag"

               val fail = V.namedvar "tagfail"

               val join = V.namedvar "tcasejoin"
               val joina = V.namedvar "joinarg"

               fun joink va = App(Var join, list va)

               fun k ov =
                Fix(list (join, [joina], c ` Var joina),
                Fix(list (fail, nil,
                     convert G joink
                      (I.Tagcase (t, obj, var, rest, def))),
                 Project
                 (0, ov, tag,
                  Project
                  (0, Var tag, vl1,
                   Project
                   (0, Var v, vl2,
                    Primop
                    (B ` PCmp PEq, [Var vl1, Var vl2],
                     nil,
                     [Project
                      (1, Var tag, vr1,
                       Project
                       (1, Var v, vr2,
                        Primop
                        (B ` PCmp PEq, [Var vr1, Var vr2],
                         nil,
                         [Project
                          (1, ov, var,
                           convert G joink e),
                          App(Var fail, nil)]))),
                      App(Var fail, nil)]))))
                    ))
           in
               convert G k obj
           end

    | convert G c (I.Tagcase (_, obj, var, nil, def)) =
           convert G c def

    | convert G c (I.Raise (_, e)) = 
           let val h = V.namedvar "currenthandler"
               fun k exnval = 
                   Primop(PGethandler, nil, list h, 
                          list ` App(Var h, list exnval))
           in convert G k e
           end

    (* note switched order of tag, value *)
    | convert G c (I.Tag (va, tg)) =
           let
               val v = V.namedvar "tagged"

               fun k vava = convert G (kk vava) tg
               and kk vava tgva =
                   Alloc(TUPLE 2, [tgva, vava], v, c ` Var v)
           in
               convert G k va
           end

    | convert G c (I.Handle (e, v, eh)) =
           (* this code is a bit complicated, but 
              conceptually it's simple.

              We make a new function "newhandler" that will serve as
              the exception handler, and install this, then continue
              evaluating e. Both the new handler and a wrapper around
              the expression e must reset the handler to the old handler
              before they exit. Since there are two possible return
              paths, we abstract the join point into one, called 
              "leavehandler". 
              *)
           (* XXX don't make an mrec fix, because the handler escapes,
              but calls to the join point are usually direct. *)
           let
               val h = V.namedvar "oldhandler"
               val n = V.namedvar "newhandler"
               val f = V.namedvar "leavehandler"
               val x = V.namedvar "leavehandlerarg"

               fun jumpback result = App(Var f, list result)
           in
               Primop(PGethandler, nil, list h,
                      list `
                      Fix([(f, list x, c ` Var x),
                           (n, list v, Primop(PSethandler, list ` Var h, nil,
                                              list ` convert G jumpback eh))],
                          Primop(PSethandler, list ` Var n, nil,
                                 list ` convert G 
                                   (fn v => Primop(PSethandler, 
                                                   list ` Var h, nil, 
                                                   list ` jumpback v)) e)))
           end



    | convert G c (I.Forget e) =
           let val v = V.namedvar "forget_unit"
               fun k value = 
                   Alloc(TUPLE 0, nil, v, 
                         Primop(PForget, list value, nil, list ` c ` Var v))
           in convert G k e
           end

    (* XXXXXX FIXME should marshall *)
    | convert G c (I.Exit e) =
           let fun k value = Primop(PReturn false, list value, nil, nil)
           in convert G k e
           end

    | convert G c (I.Primapp(PWaitall, [e], _)) =
           let
               val re = V.namedvar "waitall_res"
               val um = V.namedvar "unmarshdvec"
               fun k va =
                   Primop(PWaitall, [va], [re], list `
                          Primop(PUnmarshvec, list ` Var re, [um],
                                 list ` c ` Var um))
           in
               convert G k e
           end

    | convert G c (I.Primapp(PWaitall, _, _)) =
           raise CPS "wrong number of args to waitall"

    (* in the CPS language, comparisons are control ops, not expressions *)
    | convert G c (I.Primapp(po as B (PCmp co), [i1, i2], nil)) =
           let
               fun k va1 va2 =
                   let val x = V.namedvar "cmpresult"
                       val f = V.namedvar "cmpjoin"
                   in
                       Fix([(f, list x, c ` Var x)],
                           Primop(po,
                                  [va1, va2],
                                  nil,
                                  [convert G (fn tt => 
                                              App(Var f, [tt])) iltrue,
                                   convert G (fn ff => 
                                              App(Var f, [ff])) ilfalse]))
                   end
           in
               convert G (fn v1 => convert G (k v1) i2) i1
           end


    | convert G c (I.Primapp(B (PCmp _ ), _, _)) =
           raise CPS "should have two ints and no types to binary comparison"

    | convert G c (I.Primapp(PRef, [e], _)) =
           let val v = V.namedvar "newref"
               fun k value = Alloc(REF, list value, v, c ` Var v)
           in
               convert G k e
           end

    (* could do a similar thing for 'Write' and other ops that 
       don't have any real return value. these allocations will
       be wiped out in the opt phase *)
    | convert G c (I.Primapp(PSet, [r, v], _)) =
           let val vr = V.namedvar "setcell"
               val vv = V.namedvar "setval"
               val vu = V.namedvar "setunit"
               fun k valr = convert G (kk valr) v
               and kk valr valv =
                   Primop(PSet, [valr, valv], [],
                          list `
                          Alloc(TUPLE 0, nil, vu,
                                c ` Var vu))
           in
               convert G k r
           end

    (* other primops remain atomic *)

    (* types ignored here. *)
    | convert G c (I.Primapp(po, el, _)) =
           let val v = V.namedvar (Primop.tostring po ^ "_res")
               fun k values = Primop(po, values, list v, list ` c ` Var v)
           in
               convertl G k el
           end

    | convert G c (I.Submit e) =
           cvspawn G c
             (fn (va, vs, cs) => Primop(PSubmit, [va], vs, cs)) e

    | convert G c (I.Spawn e) =
           cvspawn G c 
           (fn (va, vs, cs) =>
            let val nd = V.namedvar "nodeps"
            in Primop(PArray0, [], [nd], list `
                      Primop(PSpawn false, [va, Var nd], vs, cs))
            end) e

    | convert G c (I.Syncall e) =
           let
               (* syncall [| e1, e2, ..., en |] 

                  becomes

                  let 
                      val ids = ... evaluate cord id vector ...
                      fun sync_cont() = 
                         let
                             val vec = getwitvec ids
                             val uvec = unmarshvec vec
                         in  ##continuation## uvec
                         end

                      val deps = ids

                      val spawnarg = marshall sync_cont

                      val cont_id = spawn (sync_cont, ids)
                  in
                      forward cont_id
                  end

                  *)

               fun k ids =
                   let
                       val sync_cont  = V.namedvar "sync_cont"
                       val spawnarg   = V.namedvar "spawnarg"
                       val deps       = V.namedvar "deps"
                       val cont_id    = V.namedvar "cont_id"
                       val vec        = V.namedvar "vec"
                       val uvec       = V.namedvar "uvec"
                   in
                       Fix([(sync_cont, [],
                             Primop(PGetwitvec, [ids], [vec],
                                    list ` 

                                    Primop(PUnmarshvec, [Var vec], [uvec],
                                           list ` c ` Var uvec)))],

                           Primop
                           (PBind (* mkdepand *), [ids], [deps], list `
                            Primop
                            (PMarshall, [Var sync_cont], [spawnarg], list `
                             Primop
                             (PSpawn true, [Var spawnarg, Var deps], [cont_id],
                              list ` Primop(PReturn true, 
                                            list ` Var cont_id, [], [])))))
                   end
           in
               convert G k e
           end


    | convert G c (I.Relaxi e) = error "Unimplemented: relaxi"

  and cvspawn G c makepo e =
      let 
          (* 
             fix ff () =
                 fix retf x = 
                     mr = marshall x
                     finish mr
                 f (retf, ()) 
             m = marshall ff
             c = spawn m

             *)
          val ff = V.namedvar "spawncode"
          val retf = V.namedvar "retanswer"
          val id = V.namedvar "cord_id"
          val x = V.namedvar "result"
          val m = V.namedvar "marshalled_arg"
          val mr = V.namedvar "marshalled_result"

          val uarg = V.namedvar "unitarg"

          fun k f =
              Fix([(ff, [],
                    Fix([(retf, [x],
                          Primop(PMarshall, list ` Var x, list mr, list `
                                 Primop(PReturn false,list ` Var mr,[],[])))],
                        Alloc(TUPLE 0, nil, uarg,
                              App(f, [Var retf, Var uarg]))))],
                  Primop(PMarshall, [Var ff], [m], list `
                         makepo (Var m, [id], [c ` Var id])))
      in
          convert G k e
      end

  (* CPS-convert a list, passing the list of values to the continuation *)
  and convertl G c l =
      let fun g (e :: rest) acc = convert G (fn v => g rest (v::acc)) e
            | g nil acc = c (rev acc)
      in g l nil
      end

  (* unless we have top-level definitions of record or sum types (option?),
     we don't need anything in the initial context.

     XXX since we don't have a way in the IL to bind a type abbreviation,
     the context is useless right now.
     *)
  val convert = fn c => fn il => convert Context.empty c il

  val translate = convert 
      (fn v =>
       let
           val mr = V.namedvar "mresult"
       in
           Primop(PMarshall, list v, list mr, list `
                  Primop(PReturn false,list ` Var mr,[],[]))
       end) 
end