
(* XXX to add:

   - source of bugs: if the same variable is bound in the same
     scope, the renamer gets confused, here. Whenever something
     is bound, we should take it out of the substitution, (or
     just raise an error if it is in the substitution).. for
     now I am trying to avoid duplicate vars. (See alphavary)

   - constant folding for conditionals, (sumcase!)
     etc.

   - argument flattening. This is very common. Look to see if a
     non-escaping function is always called directly with a tuple. If
     so, rewrite call sites to send the tuple components instead, and
     make the function prologue rebuild the tuple inside. (Usually
     the function just projects out the fields, so then those would
     become known projections and the allocation dead.) The closure
     converter has to be fixed to handle >2 arguments if this happens,
     but that's not too hard. Long argument lists can make things worse
     later, but lots of allocations do, too.
     
   - re-alloc of a tuple in the same scope. This would be best suited
     to post closure-conversion. There's no need to allocate, say,
     unit, several times in the same scope. (This probably should be
     limited to a specific scope so that we don't add unit as a free
     variable of almost every function.)

   - cse?

   - More agressive inlining. (Use "size" of body:
                               For instance, if a body is just
                               an application, inlining will
                               almost certainly be a win
                               (unless the argument list is
                               a gillion arguments long))
     ... allow inlining without deleting the definition, ie,
     for datatype constructors that also escape.

   - Constant argument removal. If a function is always called with
     the same argument (this is common with continuation arguments),
     erase it. The hard thing about this is respecting the scope of
     that variable (the function might not be in its scope, though if
     all call sites are, then the function could be moved within the
     scope of the var, too -- and, especially, the variable may look
     the same in both calls but actually be two different vars.)

*)
structure CPSOpt :> CPSOPT =
struct

  val debugopt = Params.flag false
      (SOME ("-debugopt", 
             "Debug the CPS optimizer")) "debugopt"
      
  fun debugdo f = if !debugopt then f () else ()
  fun dprint s = if !debugopt then print ("[CPSOPT] " ^ s ^ "\n") else ()

  (* bottom-up optimizations.
      * dead var elimination
      * simple eta-reductions
      * constant folding
      * constant propagation
      * unused argument elimination
      * beta reductions of functions called exactly once
        (ie, inlining)

     (DVE) dead var elimination. If at a binding site we see that a
     variable is unused, and the operation producing the binding is
     not effectful, remove the binding.

     (ETA) When we see a function declaration, if the body of the function
     is of the form App(f, args) where args is the same as the list of
     arguments to our function, then erase that function and replace it
     with f everywhere. (Some side conditions apply.)

     (INLINE) If at a function declaration we see that a function is
     called directly, and its number of free occurrences is exactly
     1, and it is not recursive, we can inline the application of
     that function. We can also inline if we have other reason to
     suspect that this will help us without hurting code size. We
     carry out the inlining by passing Inline f to the function
     returned with the call site.

     (CF) Constant Folding. If the expression supports constant
     folding, check to see if the subexpressions are constants, and
     reduce. This applies to some primops as well as intcase. In
     intcase, if the case object is a variable and that variable
     appears inside any of the arms, we can substitute the actual
     known integer value of that variable. To support constant
     folding in CPS form, we also do constant propagation. 

     (CP) When we see a binding where the right-hand-side is a simple
     constant, we replace every occurrence of that variable with the
     constant and erase the binding.

     (DROP) Drop unused arguments to functions if we know all of
     the call sites. This does work even on recursive calls to
     itself, but not transitively -- if two functions call each
     other passing an unused arg back and forth, it will be
     retained.

     (KNOWN) When we create a tuple, record its fields so that
     projections from it (while it is in scope) can be done
     statically.

     *)

  exception CPSOpt of string
  structure V = Variable
  structure O = Util.Oneshot
  structure OU = CPSOptUtil
  open CPS
  open Primop

  val itos = Int.toString

  fun chd s nil = raise CPSOpt s
    | chd _ (h::t) = h

  datatype rewrite_call = 
      Inline of V.var * V.var list * cexp
    | Remargs of int list
    | Don't

  (* augment a set of 'seen' variables.
     increment the ones that are already there,
     add the ones that weren't. *)
  fun augset s vl =
      let
          fun one (Var v, s) =
              (let val (ss, n) = V.Map.remove (s, v)
               in V.Map.insert (ss, v, n + 1)
               end handle LibBase.NotFound => V.Map.insert(s, v, 1))
            | one (_, s) = s
      in
          foldl one s vl
      end

  fun subtract s vl =
      let
          fun one (v, s) = let val (ss, _) = V.Map.remove (s, v)
                           in ss
                           end handle LibBase.NotFound => s
      in
          foldl one s vl
      end


  fun dropi dr a =
      let
          fun go nil _ = nil
            | go (h::t) n = if List.exists (fn x => x = n) dr 
                            then go t (n + 1) 
                            else h :: go t (n + 1)
      in
          go a 0
      end

  (* union two fv maps *)
  fun union s1 s2 = V.Map.unionWith op+ (s1, s2)

  fun unionc s1 s2 = V.Map.unionWith op@ (s1, s2)

  fun addcall s f g =
      let val (ss, l) = V.Map.remove (s, f)
      in V.Map.insert (ss, f, g :: l)
      end handle LibBase.NotFound => V.Map.insert (s, f, [g])

  fun takecalls s vl =
      foldl (fn (x, (s, l)) => 
             let val (ss, i) = V.Map.remove (s, x)
             in (ss, (x,i)::l)
             end handle LibBase.NotFound => (s, l)) (s, nil) vl

  fun getvar (Var v) = v
    | getvar _ = raise CPSOpt "expected var, got somethin' else"

  fun increment n (a, b, c, d) = (a, b, c, d + n)

  fun fvtos s =
      V.Map.foldli (fn (v, n, s) => 
                    s ^ " " ^ V.tostring v ^ ":" ^ itos n) "" s

  (* once tried to do eta reduction, but that didn't work because
     all call sites (ie, the App) are deferred on the way back. *)
  fun makefix (vael, e) = (Fix(vael, e),0)

  (* every time I inline a function, I should alphavary its
     arguments so that I don't get duplicate variables in the
     same scope. *)
  (* FIXME: needs to alpha vary any bindings within the body, too. *)
  fun alphavary (args, body) =
      let
          val l = map (fn a => (a, V.alphavary a)) args
          val subst = foldl (fn ((a, aa), m) =>
                             V.Map.insert (m, a, Var aa))
                            V.Map.empty l

          val nbody = CPSSubst.alphavary subst body
      in
(* (* broken: forces unset oneshot - print should handle exns? *)
          debugdo (fn () =>
                   (print "alphavary: \n";
                    print (CPSPrint.etosi 3 body);
                    print "to:\n";
                    print (CPSPrint.etosi 3 nbody)));
*)                    

          debugdo (fn () =>
                   app (fn (old, new) =>
                        print ("  " ^ V.tostring old ^ " -> " ^
                               V.tostring new ^ "\n")) l);

          (map #2 l, nbody)
      end

  (* used in order to write the result of primop translations briefly *)
  fun ethrough s kn n vs e k =
      let val (ee, fv, ca, m) = bottomup s kn e
      in (k ee, subtract fv [vs], ca, m + n)
      end

  and einstead s kn n e =
      let val (ee, fv, ca, m) = bottomup s kn e
      in (ee, fv, ca, m + n)
      end

  (* should do 'noeffect' dropping here *)
  and doprimop s kn (Primop(po, vas, vrs, rests)) =
      let
          fun dosub (va as (Var v)) = 
              (case V.Map.find (s, v) of
                   NONE => va
                 | SOME n => n)
            | dosub va = va

          val nvas = map dosub vas

          val used = augset V.Map.empty nvas

          val (rests, fv, calls, amount) =
              foldr (fn ((e,f,c,n), (es,fs,cs,nn)) =>
                     (e :: es, union f fs, unionc c cs, n + nn)) 
              (nil, used, 
               V.Map.empty, 0) 
              (map (bottomup s kn) rests)

      in
          dprint ("Primop(" ^ Primop.tostring po ^ ")...");
          dprint ("Free vars: " ^ fvtos fv);
          (case (OU.noeffect po, rests) of
             (true, [cont]) =>
                 (* if it doesn't bind any variables we need... *)
                 if List.all (fn v => not (Option.isSome 
                                           (V.Map.find (fv, v)))) vrs
                 then (* erase *) (cont, fv, calls, 
                                   amount + 5 * (1 + length nvas))
                 else (Primop(po, nvas, vrs, rests),
                       subtract fv vrs, calls, amount)
           | (true, _) => raise 
                 CPSOpt ("if primop is noeffect (" ^ Primop.tostring po ^ 
                         "), it must have exactly 1 cont")
           | _ => (Primop(po, nvas, vrs, rests), 
                   subtract fv vrs, calls, amount))
      end
    | doprimop _ _ _ = raise CPSOpt "doprimop: impossible"

  (* the main optimization pass. 

     bottomup subst known e

     subst is a substitution to apply, known is a map of bound
     tuples, and e is the expression to simplify.

      - a new simplified and substituted expression
      - its set of free variables (mapped to the number of times used)
      - a list of functions called directly, and the ML functions that
          rewrite the call sites.
      - "amount" of simplification done. If this is a positive
          integer, then the program has made progress towards an
          irreducible state. We use this to decide whether to do
          another optimization pass.

          *)
  and bottomup (s : value V.Map.map) (kn : value list V.Map.map) 
               (e : CPS.cexp) =
    let 
      fun dosub (va as (Var v)) = 
        (case V.Map.find (s, v) of
             NONE => va
           | SOME n => n)
          | dosub va = va
      fun dofun vael rest =
        let
          fun folder ((v, a, e), (l, amt)) = 
              let val (ee, fv, ca, nn) = bottomup s kn e
              in ((v, a, ee, fv, ca) :: l, nn + amt)
              end

          val (fs, done) = foldr folder (nil, 0) vael
          (* names of fns defined in this fix *)
          val friends = map #1 fs

          val _ = dprint ("Friends are: " ^ 
                          StringUtil.delimit ", "
                          (map V.tostring friends))

          (* separate the mutually recursive components
             into different fixes. XXX This could be a
             bit better by doing a topological sort of
             dependencies -- right now, if f calls g
             then they will be in the same fix, even if
             g doesn't call f back. The rest of the code
             is already set to handle this, just give it
             a list of (lists of actually
             mutually-recursive functions) in dependency
             order. *)

          (* start with a list of singletons, and union
             any lists that refer to each other. Should
             use maps and a union/find structure for
             this; right now it's quadratic in the worst
             case. But who has hundreds of
             mutually-recursively defined functions? *)

          val uf = map ListUtil.list friends

(*          val _ = dprint ("initial length uf = " ^ itos (length uf)); *)

          (* Get the set in which f resides (and the remainder). 
             It must be in some set. *)
          fun get l f =
              case List.partition (fn m => 
                                   List.exists (fn x => 
                                                V.eq(x, f)) m) l of
                  ([a], l) => (a, l)
                | _ => raise CPSOpt "get: impossible!"

          (* In the partition ll, union the sets containing f and g *)
          fun bundleunion ll f g =
              let
                  val _ = dprint ("union " ^ V.tostring f ^ 
                                  " and " ^ V.tostring g)
                  val (fs, newll) = get ll f
              in
                  if List.exists (fn z => V.eq(z, g)) fs 
                  then ll (* already unioned *)
                  else let val (gs, newerll) = get newll g
                       in (fs @ gs) :: newerll
                       end
              end

          (*       name   args?      body    freevars 
             fs : (var * var list * cexp * int Variable.Map.map
                           ??
                  * (rewrite_call -> unit) list Variable.Map.map) list *)

          fun coalesce nil l = l
            | coalesce ((h, _, _, fv, _)::t) l =
              (* for each free variable in h that's in friends,
                 union those two sets. *)
              let 
                  (* friends free in body *)
                  val freefr = 
                      List.filter (fn (x,_) => List.exists 
                                    (fn y => V.eq(x, y)) friends)
                                  (V.Map.listItemsi fv)

                  val _ = dprint ("Free friends are: " ^ 
                                  StringUtil.delimit ", "
                                  (map V.tostring friends))
              in
                  coalesce t
                    (foldl (fn ((g,_), u) => bundleunion u h g) l freefr)
              end

          val uf = coalesce fs uf

(*          val _ = dprint ("coalesced uf: " ^ itos (length uf))*)
          val uf = map (map (fn f => 
                             chd "uf" (List.filter (fn (g, _, _, _, _) => 
                                                    V.eq(g, f)) fs))) uf

          (* recurse on rest *)
          val (reste, fvs, calls, amount) = bottomup s kn rest

          (* increase done if we split up any functions *)
          val done = amount + done + (length uf - 1) * 10
        in
          null uf andalso raise CPSOpt "bug: uf was empty";
(*          dprint ("length uf = " ^ itos (length uf)); *)

          if length uf > 1 
          (* I only want to do inlining and dead code
             elimination when I'm looking at an
             individual bundle (It is a pain to look for
             calls and appearances in later bundles). So
             if I've split these up, because they don't
             call each other, I'll wait for the next
             pass to do anything. But I still need to
             call all of the functions for the call
             sites. *)
          then 
              let

                  val allcalls = 
                      foldl (fn (fs,cs) => foldl (fn ((_, _, _, _, a), b) => 
                                                  unionc a b) cs fs) calls uf
                  val allfvs = 
                      foldl (fn (fs,vs) => 
                             foldl (fn ((_, args, _, a, _), b) => 
                                    union (subtract a args) b) vs fs) fvs uf

                  (* if a call is to one of these functions, say 
                     Don't inline and subtract from set. Note these 
                     call sites might be in the function bodies! *)

                  val (newcalls, these) = takecalls allcalls friends
                  val _ = app (fn (_, fl) => app (fn f => f Don't) fl) these

                  (* if a free variable is one of these functions, 
                     don't make free. *)

                  val newfv = subtract allfvs friends

                  (* make nested fix *)
                  fun donest nil = reste
                    | donest (fs::r) =
                      Fix(map (fn (a, b, c, _, _) => (a, b, c)) fs, donest r)
              in
                  (donest uf, newfv, newcalls, done)
              end
          else 
              let val fns = chd "fns" uf
              in 
                  (* check if this is inlinable *)
                  if length fns = 1 
                      andalso 
                      let val (f, _, e, fv, cl) = chd "fns2" fns

                          val noccs = (case V.Map.find(fvs, f) of
                                            SOME n => n
                                          | NONE => 0)
                          val ncalls = (case V.Map.find(calls, f) of
                                            SOME l => length l
                                          | NONE => 0)
                      in  
                          (* we might use a gentler set of criteria here; 
                             this only allows inlining if there is exactly 
                             1 call. *)
                          
                          debugdo (fn () =>
                                   (dprint (V.tostring f ^ 
                                            " is a candidate for inlining...");
                                    dprint ("     FV inside: " ^ fvtos fv);
                                    dprint ("     FV after: " ^ fvtos fvs)));

                          (* if it's recursive, inlining is not
                             conservative (and this code might not
                             do it right) *)
                          not (isSome (V.Map.find(fv, f)))
                          (* must not escape -- all occurrences are
                             accounted for by call sites *)
                          andalso ncalls = noccs
                          andalso 
                          (* exactly one direct call *)
                          (ncalls = 1
                           orelse
                           (* looks like ctor *)
                           (case e of
                              Alloc(INT_T _, [_], _, ce) =>
                                (* this is not really safe...
                                   but the bad cases are much rarer
                                   than what we miss by not inlining! *)
                                (case ce of
                                     Deferred _ => true
                                   | App _ => true
                                   | _ => false)
                            | _ => false))
                      end
                  then 
                      let (* if inlined, we know it has this form *)
                          val (f, a, e, fv, cl) = chd "fns3" fns

                          (* do inlining *)
                          val _ = 
                              case V.Map.find (calls, f) of
                                  SOME cs => 
                                      app (fn c =>
                                           c (Inline (f, a, e))) cs
                                | NONE => () (* raise CPSOpt "inlining???" *)

                          (* better remove the call we inlined at *)
                          val (calls, _) = takecalls calls [f] 
                      in
                          (* still need to add its free variables and
                             calls, which now appear at the inlining
                             site. Set amount + 1000, because we
                             definitely want to do another pass to
                             clean up the inlining site. *)

                          (reste, union fvs (subtract fv a), 
                           unionc calls cl, amount + 1000)
                      end
                  else
                   (* can't inline. If none of the functions are
                      ever called (we don't care if they call each
                      other), then we can erase this binding,
                      otherwise we're done! *)

                   let
                     val appears = 
                         foldl (fn ((f, _, _, _, _), appeared) =>
                                appeared orelse 
                                isSome(V.Map.find(fvs, f))) false fns
                   in
                     if appears
                     then
                       let
                           (* this is similar to the n-ary case above. *)
                           val allcalls = 
                               foldl (fn ((_, _, _, _, a), b) => 
                                      unionc a b) calls fns

                           val allfvs = 
                               foldl (fn ((_, args, _, a, _), b) => 
                                      union (subtract a args) b) fvs fns

                           (* if a function doesn't use all its args, 
                              AND all of its call sites are known,
                              we can rewrite the function and call
                              sites to drop that argument. *)

                           fun dropargs (all as (f, a, body, fv, ca), 
                                         (ac, l, amt)) =
                             if List.all (fn v => 
                                          isSome (V.Map.find (fv, v))) a 
                             then (ac, all :: l, amt)
                             else 
                                let
                                    (* not all used. *)
                                    val _ = dprint 
                                        (V.tostring f ^ 
                                         " is a candidate for " ^
                                         "unused argument reduction")

                                    val (restcalls, calls) = takecalls ac [f]

                                    val calls = 
                                        getOpt(ListUtil.Alist.find 
                                                  V.eq calls f, 
                                               nil)

                                    val used = getOpt(V.Map.find (allfvs, f),
                                                      0)
                                in
                                 case Int.compare(length calls, used) of
                                     GREATER => raise CPSOpt 
                                         "impossible: more calls than used"

                                   (* some unknown *)
                                   | LESS => (ac, all :: l, amt)

                                   | EQUAL =>
                                      (* (DROP) *)
                                      (* all known callsites -- do it! *)
                                      let
                                          (* generate list of unused args. *)
                                          fun gu nil _ = nil
                                            | gu (h::t) n =
                                              if isSome (V.Map.find (fv, h))
                                              then gu t (n + 1)
                                              else n :: gu t (n + 1)
                                          val drops = gu a 0
                                      in
                                          dprint ("Removing args from " ^
                                                  V.tostring f 
                                                  ^ "!");

                                          app (fn j => 
                                               j (Remargs drops)) calls;

                                          (restcalls, 
                                           (f, dropi drops a, 
                                            body, fv, ca)::l, 
                                           amt + 100)
                                      end
                                end

                           val (newcalls, fs, aaa) = 
                               foldr dropargs (allcalls, nil, 0) fns

                           (* send Don't to anything left *)
                           val (newcalls, these) = takecalls newcalls friends

                           val _ = 
                               app (fn (_, fl) => 
                                    app (fn f => f Don't) fl) these

                           (* if a free variable is one of these functions,
                              don't make free. *)

                           val newfv = subtract allfvs friends

                           val (ee, d) = makefix 
                               (map (fn (a, b, c, _, _) => (a, b, c)) fs, 
                                reste)
                       in
                           (ee, newfv, newcalls, done + d + aaa)
                       end

                     (* really easy -- the variables aren't free, and we 
                        don't need to instantiate the call sites (if
                        there are any, they're in the function
                        bodies), so just return the continuation! *)

                     else (reste, fvs, calls, amount + 50)
                   end

              end
        end

    in
     case e of
       Deferred os => 
          (case O.deref (os()) of
               NONE => 
                   raise CPSOpt "Found unset Deferred expression in Optimize!"
             | SOME e => bottomup s kn e)
      (* XXX if I want to run this on closure-converted code, need to look for
         App (Label v, vl) as well. *)
     | App (v, vl) =>
          let
              val v = dosub v
              val vl = map dosub vl
              val fv = augset V.Map.empty (v :: vl)
              val os = O.oneshot () : cexp O.oneshot

              fun rewrite_site (Inline (f, args, body)) =
                  (* just create the bindings; the next
                     pass will take care of substitution and
                     further simplification. *)
                  let
                      val (args, body) = alphavary (args, body)

                      fun re nil nil = body
                        | re (formal::rest) (actual::more) =
                          Primop(PBind, [actual], [formal], [re rest more])
                        | re _ _ =
                          raise CPSOpt ("when inlining, function called " ^
                                        "on the wrong number of args")
                  in
                      dprint ("Inlining " ^ V.tostring f ^ "!");
                      O.set (os, re args vl)
                  end

                | rewrite_site Don't = O.set (os, App(v, vl))

                  (* (DROP) *)
                | rewrite_site (Remargs dr) = O.set (os, App(v, dropi dr vl))

              val calls = addcall V.Map.empty (getvar v) rewrite_site
          in
              (Deferred (fn () => os), fv, calls, 0)
          end
    (* rewrite bindings with substitutions *)
    | Primop(PBind, [va], [vr], [rest]) =>
          increment 5 (bottomup (V.Map.insert (s, vr, dosub va)) kn rest)

    (* various arithmetic identities *)
    | Primop(B PDiv, [Int 0, _], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => Primop(PBind, [Int 0], [vr], [e]))
    | Primop(B PTimes, [Int 0, _], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => Primop(PBind, [Int 0], [vr], [e]))
    | Primop(B PTimes, [_, Int 0], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => Primop(PBind, [Int 0], [vr], [e]))
    | Primop(B PPlus, [va, Int 0], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => Primop(PBind, [va], [vr], [e]))
    | Primop(B PPlus, [Int 0, va], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => Primop(PBind, [va], [vr], [e]))
    | Primop(B PTimes, [va, Int 1], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => Primop(PBind, [va], [vr], [e]))
    | Primop(B PTimes, [Int 1, va], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => Primop(PBind, [va], [vr], [e]))
    | Primop(B PMinus, [va, Int 0], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => Primop(PBind, [va], [vr], [e]))
    (* XXX Times (a, 2) = Plus (a, a)   (worse because more tag checks?) *)

    (* XXX check overflow *)
    | p as Primop(PNeg, [Int a], [vr], [rest]) =>
          ethrough s kn 10 vr rest (fn e => 
                                    Primop(PBind, [Int (~a)], [vr], [e]))

    | p as Primop(B po, [Int a, Int b], [vr], [rest]) =>
    (* two constants: should be an arithmetic primop *)
          (case OU.evalints po a b of
               SOME va => ethrough s kn 10 vr rest 
                            (fn e => Primop(PBind, [va], [vr], [e]))
             | NONE => doprimop s kn p)

    | p as Primop(B (PCmp po), [Int a, Int b], [], [tt, ff]) => 
    (* two continuations: a comparison operation *)
          let val (ee, fv, ca, m) = if OU.evalcmp po a b
                                    then bottomup s kn tt
                                    else bottomup s kn ff
          in (ee, fv, ca, m + 100)
          end

    (* same vars are always equal *)
    | p as Primop(B (PCmp PEq), [Var a, Var b], [], [tt, _]) =>
          if V.eq (a, b)
          then einstead s kn 100 tt
          else doprimop s kn p

    (* and never inequal *)
    | p as Primop(B (PCmp PNeq), [Var a, Var b], [], [_, ff]) => 
          if V.eq (a, b)
          then einstead s kn 100 ff
          else doprimop s kn p

    (* General case *)
    | p as Primop _ => doprimop s kn p
    (* The only thing we do for allocations is erase them (they are 
       not effectful) if the result is never used. It might be profitable
       to attempt to do the following further optimizations:
        - reuse older records in scope with exactly the same data
        XXX add: if a record is created as the eta-expansion of another
        record, just substitute. Ie:
        r = (TUPLE_3 | #0 s, #1 s, #2 s)
          (and length of s = 3)

       ... all of these seem to work better in a top-down pass.

       *)
    (* (CP) don't allocate ints or strings. The alloc phase
       later will create them at the last minute, if needed. *)
    | Alloc(INT, [Int i], v, rest) => 
          increment 50 (bottomup s kn
                        (Primop(PBind, [Int i], [v], [rest])))
    | Alloc(STRING, [String ss], v, rest) => 
          increment 10 (bottomup s kn
                        (Primop(PBind, [String ss], [v], [rest])))
    (* XXX also code/label? *)

    | Alloc(tag, vas, v, rest) => 
          let
              val nvas = map dosub vas
              (* (KNOWN) add this tuple as known while it is 
                 in scope *)
              val newkn = 
                  (case tag of
                       TUPLE _ => V.Map.insert (kn, v, nvas)
                     | _ => kn)
              val (re, fv, calls, a) = bottomup s newkn rest
          in
              case V.Map.find (fv, v) of
                  (* dead? erase it. *)
                  NONE => (re, fv, calls, a + 50)
                | _ => (Alloc(tag, nvas, v, re), 
                        augset (subtract fv [v]) nvas, calls, a)
          end

    (* Erase unused projections and carry out projections from
       known tuples. *)
    | Project(i, va, vr, rest) => 
        let
          fun regular nva =
              let
                  val (re, fv, calls, a) = bottomup s kn rest
              in
                  case V.Map.find (fv, vr) of
                      (* dead? erase it. *)
                      NONE => (re, fv, calls, a + 50)
                    | _ => (Project(i, nva, vr, re), 
                            augset (subtract fv [vr]) [nva], 
                            calls, a)
              end
        in
          case dosub va of
            Var target => 
              (case V.Map.find (kn, target) of
                   NONE => regular (Var target)
                 (* (KNOWN) projection from known tuple *)
                 | SOME vals =>
                       let in
                           dprint (V.tostring vr ^ " is # from known tuple");
                           increment 50
                           (bottomup s kn 
                            (Primop(PBind, 
                                    [List.nth (vals, i)] 
                                    handle _ => raise CPSOpt 
                                        "Bad CPS: projection index too high",
                                        [vr], [rest])))
                       end)
          | _ => raise CPSOpt 
               "Bad CPS: projection from non-variable"
        end


    (* We don't have any optimizations we can really perform here... *)
    | Sumswitch (va, v, iel, def) =>
          let val nva = dosub va
              fun folder ((i, e), (l, fvs, calls, a)) =
                  let val (ee, fvv, cc, aa) = bottomup s kn e
                  in ((i,ee)::l, union fvs (subtract fvv [v]), 
                      unionc calls cc, a + aa)
                  end
              val (ndef, dfv, dc, da) = bottomup s kn def
              val (niel, fvs, cls, amt) = 
                  foldr folder (nil, subtract dfv [v], dc, da) iel
          in (Sumswitch (nva, v, niel, ndef), augset fvs [nva], cls, amt)
          end

    | Intswitch (va, iel, def) =>
          let
              val nva = dosub va
          in
              case nva of
                  (* do constant folding *)
                  Int i =>
                      (case ListUtil.Alist.find op= iel i of
                           (* not in list, use default *)
                           NONE => increment 50 (bottomup s kn def)
                           (* in list, use it. *)
                         | SOME e => increment 50 (bottomup s kn e))
                | Var v =>
                      (* can't fold this one. But if we enter any
                         of the arms, then we can substitute that
                         integer for this variable within the body.
                         This can cause nested switches to be folded
                         away. *)
                      let
                          fun folder ((i, e), (l, fvs, calls, a)) =
                              let val (ee, fvv, cc, aa) = 
                                  bottomup (V.Map.insert (s, v, Int i)) kn e
                              in ((i,ee)::l, union fvs fvv, 
                                  unionc calls cc, a + aa)
                              end

                          val (ndef, dfv, dc, da) = bottomup s kn def
                          val (niel, fvs, cls, amt) = 
                              foldr folder (nil, dfv, dc, da) iel
                      in
                          (Intswitch (nva, niel, ndef), 
                           augset fvs [nva], cls, amt)
                      end
                | _ => raise CPSOpt "Bad CPS: intswitch on non-var/int"
          end
    | Fix (nil, rest) =>
          increment 5 (bottomup s kn rest)

    (* (ETA) Eta-reduce a common but specific form:
       Fix f (args) as
         g (args)            where f =/= g
         (side condition f \notin args not necessary because
          the formal args shadow f.)
         *)

    (* at this stage, all applications should be with Vars *)
    | Fix (vael as [(f,formal,App(Var g,actual))], rest) => 
          if not (V.eq (f, g))
             andalso ListUtil.all2 
                     (fn (fo, Var ac) => V.eq(fo,ac)
                        | _ => false) formal actual
          then (* treat it as substitution f = g *)
              let in
                  dprint ("Eta reduced " ^ V.tostring f ^
                          " = " ^ V.tostring g);
                  increment 150 (bottomup 
                                 (V.Map.insert 
                                  (s, f, dosub (Var g))) kn rest)
              end
          else dofun vael rest
    | Fix (vael, rest) =>
          dofun vael rest
    end

  (* for testing from interactive loop *)
  fun botpass x =
      let val (ne, _, calls, amt) = bottomup V.Map.empty V.Map.empty x
      in 
          V.Map.app (fn j => app (fn f => f Don't) j) calls;
          print ("Did " ^ itos amt ^ " units of optimization:\n");
          CPSPrint.printe ne;
          (* XXX hack:
             should just be ne -- this prohibits 
             printing at top level in NJ *)
          Deferred(fn () => Util.Oneshot.init ne)
      end

  (* We generate call sites when we see any application, even if that
     application was a function parameter or something. So we may not
     have told every call site to be not inlined. Any remaining ones
     should be not inlined right here: *)
  fun repbottom n x =
      let val (ne, _, calls, amt) = bottomup V.Map.empty V.Map.empty x
      in
          dprint ("  ** PASS SCORE : " ^ itos amt ^ " **");
          V.Map.app (fn j => app (fn f => f Don't) j) calls;

          debugdo (fn () =>
                   CPSPrint.printe ne);

          if n > 0 orelse amt > 50 
          then repbottom (n - 1) ne
          else ne
      end

  (* always do at least 3 (?) passes *)
  fun optimize x = 
      repbottom 3 x

end

