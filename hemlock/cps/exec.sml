
(* interpret a closure-converted CPS expression, for debugging only *)

structure CPSExec =
struct

    open CPS
    open Primop

    exception Exec of string

    structure VM = Variable.Map
    val vs = Variable.tostring
    val itos = Int.toString

    datatype mstring =
        S of string
      (* marshalled *)
      | M of value
      (* submitted task *)
      | T of value

    and value =
        Vint of int
      | Vtup of value list
      | Vtag of int * value
      | Vlab of Variable.var
      | Vstr of mstring

    local val ctr = ref 0
    in fun newint () = (ctr := !ctr + 1; !ctr)
    end

    val hvar = Variable.namedvar "handler"

    infix ?
    infix ++

    fun G ++ (v, va) = VM.insert (G, v, va)
        
    (* hack *)
    fun sethandler G va = G ++ (hvar, va)

    fun run G funs ce =
      let
          fun getf lab =
              (case VM.find (funs, lab) of
                   SOME f => f
                 | NONE => raise Exec ("function " ^ vs lab ^ 
                                       " not def'd?"))

          fun go G e =
            let
                fun err s =
                    let in
                        print ("Error at start of instruction seq:\n" ^
                               CPSPrint.etosi 3 e);
                        print ("\n\nError was: " ^ s ^ "\n");
                        (* XXX print context, etc. *)
                        raise Exec s
                    end

                fun G ? v =
                    (case VM.find (G, v) of
                         SOME va => va
                       | NONE => err 
                             ("variable " ^ vs v ^ " was not bound"))
                         
                fun gethandler G = G ? hvar

                fun value G (Var v) = G ? v
                  | value G (Label l) = Vlab l
                  | value G (Int i) = Vint i
                  | value G (String s) = Vstr (S s)
            in
              case e of
                  (Fix _) =>
                  err "code is not closure-converted: encountered a fix"
                | (App (va, vl)) =>
                  let
                    val lab =
                      (case va of
                         Label l => l
                       | Var v => 
                           (case G ? v of
                                Vlab l => l
                              | _ => err (vs v ^ 
                                          " was not bound to " ^
                                          " a label"))
                       | _ => err ("value in app not label or var"))

                    val (ar, ce) = getf lab

                    fun binds G' nil nil = go G' ce
                      | binds G' (arg::arest) (va::vrest) =
                        binds (G' ++ (arg, value G va)) arest vrest
                      | binds _ _ _ =
                        err ("wrong number of args to " ^
                             vs lab)
                  in
                      binds VM.empty ar vl
                  end
                | Sumswitch(va, v, iel, def) =>
                   (case value G va of
                        Vtag (i, vu) =>
                            (case ListUtil.Alist.find op= iel i of
                                 SOME ee => go (G ++ (v, vu)) ee
                               | NONE => go (G ++ (v, vu)) def)
                      | _ => err ("sumswitch object not tagged"))

                | (Alloc(INT, [va], v, ce)) =>
                  go (G ++ (v, case value G va of
                            vv as Vint _ => vv
                          | _ => err ("alloc int at non int")))
                  ce
                | (Alloc (STRING, [va], v, ce)) =>
                  go (G ++ (v, case value G va of
                            vv as Vstr _ => vv
                          | _ => err ("alloc str at non str")))
                  ce
                | (Alloc (CODE, [va], v, ce)) =>
                  go (G ++ (v, case value G va of
                            vv as Vlab _ => vv
                          | _ => err ("alloc lab at non lab")))
                  ce
                | (Alloc (INT_T n, [va], v, ce)) =>
                  go (G ++ (v, Vtag (n, value G va))) ce
                | (Alloc (TUPLE n, vl, v, ce)) =>
                  let in
                      length vl = n
                          orelse err ("alloc tuple of wrong len");
                      go (G ++ (v, Vtup (map (value G) vl))) ce
                  end
                | (Alloc _) => err "bad alloc"
                | (Project (n, va, v, ce)) =>
                  (case value G va of
                       Vtup vl =>
                           let in
                               n < length vl
                                 orelse err ("project " ^ itos n ^
                                             " from tuple of len " ^
                                             itos (length vl));
                               go (G ++ (v, List.nth (vl, n))) ce
                           end
                   | _ => err "projection from non tuple")
                 | (Intswitch _) => err "intswitch not implemented"
                 | (Deferred os) =>
                       (case Util.Oneshot.deref (os()) of
                            NONE => err "unset oneshot!"
                          | SOME e => go G e)
                 | (Primop (PBind, [va], [v], [ce])) =>
                            go (G ++ (v, value G va)) ce
                 | (Primop (PNewtag, [], [v], [ce])) =>
                            go (G ++ (v, Vint (newint ()))) ce
                 | (Primop (PGethandler, [], [v], [ce])) =>
                            go (G ++ (v, gethandler G)) ce
                 | (Primop (PSethandler, [va], [], [ce])) =>
                            go (sethandler G (value G va)) ce
                 | (Primop (PMarshall, [va], [v], [ce])) =>
                            (* bogus *)
                            go (G ++ (v, Vstr (M (value G va)))) ce
                 | (Primop (PReturn _, [va], [], [])) =>
                            va
                 | (Primop (PStdout, [], [v], [ce])) =>
                            go (G ++ (v, Vint 0)) ce
                 | (Primop (PWrite, [id, s], [rr], [ce])) =>
                    let in
                        (case (value G id, value G s) of
                             (Vint 0, Vstr (S s)) => print s
                           | (Vint 0, Vstr (M _)) => print "((marshalled))"
                           | (Vint 0, Vstr (T _)) => print "((task))"
                           | _ => err "bad args to write");
                             
                        go (G ++ (rr, Vtup nil)) ce
                    end
                 | (Primop (B (PCmp PEq), [a, b], [], [tt, ff])) => 
                    (case (value G a, value G b) of
                         (Vint aa, Vint bb) =>
                             if aa = bb then go G tt
                             else go G ff
                       | _ => err "bad args to eq")

                 | (Primop (po, _, _, _)) =>
                    err ("unimplemented primop " ^ Primop.tostring po)
            end
                        
      in
          go (sethandler G (Vstr (S "initial"))) ce
      end


    fun exec (Fix (fs, ce)) =
        let
            val funs =
                foldl (fn ((v, args, e), m) =>
                       VM.insert (m, v, (args, e)))
                      VM.empty fs
        in
            run VM.empty funs ce
        end
      | exec (ce) = run VM.empty VM.empty ce


end