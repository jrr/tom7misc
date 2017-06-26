
structure Eval :> EVAL =
struct

    val debug = ref false

    exception Unimplemented
    exception Wrong

    open AST

    type process = frame list * exp

    fun begin l = map (fn x => (nil, x)) l

    datatype result = Blocked | Done | Progress | GC

    datatype chanstatus = Open | Closed

    fun newch st =
        let val a = Loc.newloc()
        in (SC a, RC a, (a, Open) :: st)
        end
    
    fun removechan l nil = raise Wrong
      | removechan l ((s as (ll,_))::r) = 
        if Loc.compare (l,ll) = EQUAL then r else (s::removechan l r)

    (* close channel. If both have closed it, remove it. *)
    fun closechan l nil = raise Wrong
      | closechan l ((s as (ll,Open))::r) = 
        if Loc.compare (l,ll) = EQUAL then (ll,Closed)::r else (s::closechan l r)
      | closechan l ((s as (ll,Closed))::r) = 
        if Loc.compare (l,ll) = EQUAL then r else (s::closechan l r)

    fun isvalue e =
        case e of
            True => true
          | False => true
          | Inj (_,e,_) => isvalue e
          | Exp e => true
          | Tuple el => ListUtil.allsecond isvalue el
          | Ltuple _ => true
          | Lfn _ => true
          | Tfn _ => true
          | Str _ => true
          | Fix _ => true
          | Lfix _ => true
          | SC _ => true
          | RC _ => true
          | _ => false

    (* atomic instructions in the machine *)

    fun stepv st (p as (fr, exp)) pr =
        case p of
            (nil, v) => (Done, nil, v, st, pr)
          | (Ftuple(sel,s,nil)::t, v) => (Progress, t,Tuple((s,v)::sel), st, pr)
          | (Ftuple(sel,s,(ss,ee)::sel')::t, v) => (Progress,
                                                    Ftuple((s,v)::sel,s,sel')::t,
                                                    ee, st, pr)
          | (F1send e::t, v) => (Progress, F2send v :: t, e, st, pr)
          | (F2send (SC l)::_,d) => (Blocked, fr, exp, st, pr)
          | (F2send _::_,_) => raise Wrong
          | (Frecv :: t, RC l) =>
                let
                    datatype ('a, 'b) sum = A of 'a | B of 'b
                    fun f acc nil = B ()
                      | f acc ((pp as ((F2send (SC ll)::st), d))::more) =
                        if Loc.compare (l, ll) = EQUAL andalso isvalue d then
                            A(rev acc, pp, more)
                        else f (pp::acc) more
                      | f acc (pp::more) = f (pp::acc) more
                in
                    case ListUtil.Alist.find (ListUtil.Alist.bycompare Loc.compare) st l of
                        NONE => raise Wrong
                      | SOME Open => 
                            (case f nil pr of
                                 B _ => (Blocked, fr, exp, st, pr)
                               | A (bfr, (F2send (SC ll)::rest, d), aft) =>
                                     let
                                         val (sc, rc, st) = newch st
                                     in
                                         (Progress, t, Tuple[("data",d),("channel", rc)],
                                          removechan l st,
                                          bfr @ ((rest, sc) :: aft))
                                     end
                               | _ => raise Wrong)
                      | SOME Closed => 
                            (* channel was closed; this recv will never succeed so
                               GC. *)
                                 (GC, fr, exp, removechan l st, pr)
                end
          | (Frecv :: _, _) => raise Wrong
          | (Fclose :: t, SC l) => 
                (Progress, t, Tuple [], closechan l st, pr)
          | (Fclose :: t, RC l) => 
                (Progress, t, Tuple [], closechan l st, pr)
          | (Fclose :: _, _) => raise Wrong
          | (Flett (ssl, e) :: t, Tuple svl) =>
                let
                    val ssl = ListUtil.sort (ListUtil.byfirst String.compare) ssl
                    val svl = ListUtil.sort (ListUtil.byfirst String.compare) svl
                    fun dosubs ((l1, var)::rssl) ((l2, vl)::rsvl) e =
                        let in
                            l1 = l2 orelse raise Wrong;
                            dosubs rssl rsvl (Subst.subst (var,vl) e)
                        end
                      | dosubs nil nil e = e
                      | dosubs _ _ _ = raise Wrong
                in
                    (Progress, t, dosubs ssl svl e, st, pr)
                end
          | (Flett _ :: _, _) => raise Wrong
          | (Flet (s,e) :: fr, v) => (Progress, fr, Subst.subst (s,v) e, st, pr)
          | (Fprint :: fr, Str s) => 
                let in
                  print s;
                  (Progress, fr, Tuple[], st, pr)
                end
          | (Fprint :: _, _) => raise Wrong
          | (Fuse (s,e) :: fr, Exp v) => (Progress, fr, Subst.subst (s,v) e, st, pr)
          | (Fuse _::_, _) => raise Wrong
          | (Fapp e :: fr, (ff as (Fix(f,x,_,_,bod)))) => 
                (Progress, fr, Subst.subst (f, ff) (Subst.subst (x, e) bod), st, pr)
          | (Fapp _ :: _, _) => raise Wrong
          | (F1lapp e :: fr, v) => (Progress, F2lapp v :: fr, e, st, pr)
          | (F2lapp (ff as (Lfix (f,x,_,_,bod))) :: fr, arg) => 
                (Progress, fr, Subst.subst (f, ff) (Subst.subst (x, arg) bod), st, pr)
          | (F2lapp (Lfn (x, _, bod)) :: fr, arg) => 
                (Progress, fr, Subst.subst (x, arg) bod, st, pr)
          | (F2lapp _ :: _, _) => raise Wrong
          (* using type erasure *)
          | (Ftapp _ :: fr, Tfn (s,v)) => (Progress, fr, v, st, pr)
          | (Ftapp _ :: _, _) => raise Wrong
          | (Fif (t,_)::fr, True) => (Progress, fr, t, st, pr)
          | (Fif (_,f)::fr, False) => (Progress,fr, f, st, pr)
          | (Fif _::_, _) => raise Wrong
(*          | (Fexp ::fr, v) => (Progress, fr, Exp v, st, pr) *)
          | (Finj (s,t)::fr, v) => (Progress, fr, Inj(s,v,t),st,pr)
          | (Flproj s::fr, Ltuple sel) =>
                (case ListUtil.Alist.find (op=) sel s of
                     NONE => raise Wrong
                   | SOME e => (Progress, fr, e, st, pr))
          | (Flproj _::_,_) => raise Wrong
          | (Fcase (s, sel)::fr, Inj(ss,v,_)) =>
                (case ListUtil.Alist.find (op=) sel ss of
                     NONE => raise Wrong
                   | SOME e => (Progress, fr, Subst.subst (s, v) e, st, pr))
          | (Fcase _::_, _) => raise Wrong
          | (Fseq [e] :: fr, v) => (Progress, fr, e, st, pr)
          | (Fseq (e::t) :: fr, v) => (Progress, Fseq t :: fr, e, st, pr)
          | (Fseq _::_, _) => raise Wrong
          | (Fspawn :: fr, Lfn (x,_,bod)) =>
                     (Progress, fr, Tuple [], st, (nil,Subst.subst (x,Tuple[]) bod)::pr)
          | (Fspawn :: fr, (ff as Lfix (f,x,_,_,bod))) =>
                     (Progress, fr, Tuple [], st, (nil,(Subst.subst (f, ff)
                                                        (Subst.subst (x,Tuple[]) bod)))::pr)
          | (Fspawn :: _,_) => raise Wrong

    fun stepe st (p as (fr, e)) =
        case e of
            Let (s,a,b) => (Flet(s,b)::fr,a,st)
          | Seq (e::t) => (Fseq t :: fr, e, st)
          | Seq _ => raise Wrong
          | Print e => (Fprint :: fr, e, st)
          | App (a,b) => (Fapp b :: fr, a,st)
          | Lapp(a,b) => (F1lapp b :: fr, a,st)
          | Tapp(a,t) => (Ftapp t :: fr, a,st)
          | If(a,b,c) => (Fif (b,c) :: fr, a,st)
          | Tuple ((s,e)::t) => (Ftuple (nil, s, t) :: fr, e,st)
          | Inj (s,e,t) => (Finj (s,t) :: fr, e,st)
          | Lproj (s,e) => (Flproj s :: fr, e,st)
          | Lett (ssl,a,b) => (Flett (ssl, b)::fr, a,st)
          | Case (e,s,sel) => (Fcase (s,sel)::fr, e,st)
          | Send (a,b) => (F1send b :: fr, a,st)
          | Recv a => (Frecv :: fr, a,st)
          | Use (s,a,b) => (Fuse (s, b)::fr, a,st)
          | Chan _ =>
                let val (s,r,st) = newch st
                in (fr, Tuple[("s",s),("r",r)],st)
                end
          | Close e => (Fclose :: fr, e,st)
          | Spawn e => (Fspawn :: fr, e,st)
          | _ => raise Wrong

    (* step state process other-processes 
       returns (result, frame stack, exp, state, other-processes)
       *)
    fun step st (p as (fr, e)) pr =
        if isvalue e then stepv st p pr
        else let val (a,b,st) = stepe st p in (Progress,a,b,st,pr) end

    (* round-robin scheduler. *)
    fun run s st nil = nil
      | run s st (h::t) =
        let in
            if !debug then
                let in
                    print "run: --------------- step: ";
                    print (Int.toString s);
                    print "\n";
                    print (Print.pss (h::t));
                    print "\n--------------------\n"
                end
            else ();
            case step st h t of
                (Progress,a,b,st,pr) => run (s+1) st (pr @ [(a,b)])
              | (Blocked,a,b,st,pr) => brun (s+1) st pr [(a,b)]
              | (Done,a,b,st,pr) => run (s+1) st pr
              | (GC,a,b,st,pr) => (print "FIXME DO GC...\n"; run (s+1) st pr)
        end
    (* accumulating blocked threads *)
    and brun s st nil bl = bl
      | brun s st (h::t) bl =
        let in
            if !debug then
                let in
                    print "brun: -------------- step: ";
                    print (Int.toString s);
                    print "\n";
                    print (Print.pss bl);
                    print "\n=== above blocked ==\n";
                    print (Print.pss (h::t));
                    print "\n--------------------\n"
                end
            else ();
            case step st h (bl @ t) of
                (Progress,a,b,st,pr) => run (s+1) st (pr @ [(a,b)])
              | (Blocked,a,b,st,pr) => brun (s+1) st pr (bl @ [(a,b)])
              | (Done,a,b,st,pr) => run (s+1) st pr
              | (GC,a,b,st,pr) => (print "FIXME DO GC...\n"; run (s+1) st pr)
        end

    val run = run 0 nil

end