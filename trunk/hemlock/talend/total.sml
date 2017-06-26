
structure ToTAL :> TOTAL =
struct

    (* XXX MALLOC trashes ECX, EDX (2,3).
       right now I save them across calls to malloc,
       but this is kind of dumb since I never use
       the values in them except for function calls.
       Of course, a peephole optimization that stores
       some values in registers would probably want
       them preserved. *)

    exception ToTAL of string

    open RTL

    fun wr f s = 
        let in
            TextIO.output(f, s);
            TextIO.output(f, "\n")
        end
    fun itos n = 
      if n >= 0 then Int.toString n
      else "-" ^ Int.toString (~ n)


        (* XXX move to TAL *)
    (* code that doesn't care about the types of incoming registers,
       because it is a fatal error handler or something *)
    val handlecode =
        "All[vanswer:T4 vdep:T4 vdlis:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap] . (?hemcodex (`ttt) (junk4) vanswer vdep vdlis s1 s2 e1 e2)"

    val code_table = "ctab"

    val newstring = HemlockUtil.newstring "__"

    val error = TALUtil.error

    (* XXX this needs to be used in more places *)
    fun talify s =
        StringUtil.harden (StringUtil.charspec "[A-Za-z0-9$_]") #"?" 64 s

    fun rtos 0 = "EAX"
      | rtos 1 = "EBX"
      | rtos 2 = "ECX"
      | rtos 3 = "EDX"
      | rtos 4 = "ESI"
      | rtos 5 = "EDI"
      | rtos i = raise ToTAL ("there is no register " ^ itos i)

    (* never leaves in reg 0, because we never assume it has a value *)
    fun allregsbut l =
        List.filter (fn x => not (List.exists (fn y => y = x) l)) [1, 2, 3, 4, 5]

    datatype arg = AReg of int | AMainarg of string | ADummy | AInt of int

    fun wcode f c =
        let
            val wr = wr f
            fun wrt s = wr ("\t" ^ s)

        fun stos (Rco i) = rtos i
          | stos (Imm i) = "subsume(<B4>," ^ itos i ^ ")"
          | stos (Lab l) = talify l
          | stos (Sco 0) = "[ESP]"
          | stos (Sco i) = "[ESP+" ^ itos (i * 4) ^ "]"
          | stos (Rin i) = raise ToTAL "unimplemented"

        fun dtos (Rdst i) = rtos i
          | dtos (Sdst 0) = "DWORD PTR [ESP]"
          | dtos (Sdst i) = "DWORD PTR [ESP+" ^ itos (i * 4) ^ "]"
          | dtos (Rto i) = raise ToTAL "unimplemented"

        fun otos ADD = "ADD"
          | otos SUB = "SUB"
          | otos XOR = "XOR"
          | otos OR  = "OR"
          | otos AND = "AND"

        fun ctos E = "E"
          | ctos NE = "NE"
          | ctos L = "L"
          | ctos LE = "LE"
          | ctos G = "G"
          | ctos GE = "GE"


              
        fun call fname saves targs args dst ssize =
            let 
                val nargs = length args
                val nsaves = length saves
                fun pushargs nil _ = ()
                  | pushargs (ADummy :: t) n =
                    let in
                        wrt ("push subsume(<B4>, 0)");
                        pushargs t (n + 1)
                    end
                  | pushargs (AInt ii :: t) n =
                    let in
                        wrt ("push subsume(<B4>, " ^ itos ii ^ ")");
                        pushargs t (n + 1)
                    end
                  | pushargs (AReg src :: t) n = 
                    let in
                        wrt ("push " ^ rtos src);
                        pushargs t (n + 1)
                    end
                  | pushargs (AMainarg s :: t) n =
                    let in
                        wrt ("push dword ptr [esp + " ^
                             itos (4 * (ssize + nsaves + n +
                                        TAL.argdepth s +
                                        (* ret addr, exn handler *)
                                        1 + 1)) ^
                             "] ; " ^ s);
                        pushargs t (n + 1)
                    end
            in
                (* put saves on stack *)
                app (fn r =>
                     wrt ("push " ^ rtos r)) (rev saves);
                
                pushargs (rev args) 0;
                
                (* call it *)
                wrt ("call tapp(" ^ fname ^
                     ", <" ^ StringUtil.delimit ", " 
                         (targs @
                          ["ESP " ^ itos nargs ^ " " ^ 
                           itos (TAL.mainargs + 1 + 1 + 
                                 ssize + nsaves + nargs) ^
                           " s1", "EBP 1", "e1", "e2"])^ ">)");

                (* destination comes back in eax. put it in
                   dst if dst isn't already eax. *)
                if dst <> 0
                then wrt ("mov " ^ rtos dst ^ ", eax")
                else ();
                
                (* pop args *)
                if nargs > 0
                then wrt ("add esp, " ^ itos (4 * nargs))
                else ();
                
                (* restore saves *)
                app (fn r =>
                     wrt ("pop " ^ rtos r)) saves
                
            end

        fun wi (Op(oper, reg, src)) = wrt (otos oper ^ " " ^ rtos reg ^ 
                                           ", " ^ stos src)
          | wi (Quotrem (Rco src)) = 
            let 
              val name = newstring "i"
            in
              wrt ("UNPACK " ^ name ^ "," ^ rtos src ^ ", " ^ rtos src);
              wrt ("CMP " ^ rtos src ^ ",0");
              wrt ("JBE " ^ error 0x5358 "divide by zero");
              wrt "CDQ";
              wrt ("IDIV " ^ rtos src)
            end
          | wi (Quotrem _) = raise ToTAL "must quotrem a reg"

          | wi (Multiply src) = wrt ("IMUL " ^ stos src)
          | wi (Jc (cond, src)) =
            wrt ("J" ^ ctos cond ^ " " ^ stos src)
          | wi (Mov (dst, src)) =
            wrt ("MOV " ^ dtos dst ^ ", " ^ stos src)
          | wi (Salloc n) = wrt ("SUB ESP, " ^ itos (4 * n))
          | wi (Sdelete n) = wrt ("ADD ESP, " ^ itos (4 * n))
          | wi (Alloc(Rdst dst, wt, src)) =
            let 
                val name = newstring "asmall"
            in
                (* allocate, putting the pointer to space in dst
                   without screwing up the source register *)
                if src = Rco 0 then wrt "PUSH EAX" else ();
                
                wrt "PUSH ECX";
                wrt "PUSH EDX";
                wrt ("MALLOC " ^ name ^ ", 8");
                wrt "POP EDX";
                wrt "POP ECX";

                if dst <> 0 then wrt ("MOV " ^ rtos dst ^ ", EAX") else ();
                if src = Rco 0 then wrt "POP EAX" else ();

                (* place the tag *)
                wrt ("MOV DWORD PTR [" ^ rtos dst ^ "], " ^ itos (TAL.tagfor wt) ^
                     " ;; " ^ TAL.nameof wt);

                (* place the data *)
                wrt ("MOV DWORD PTR [" ^ rtos dst ^ " + 4], " ^ stos src);
                
                (* coerce result. *)
                wrt ("FORGETUNIQUE " ^ name);
                wrt ("COERCE rollsum(<`ttt>, forgetname(" ^ rtos dst ^ "))")
            end
          | wi (Alloc _) = raise ToTAL 
                     ("dst of Alloc must be register," ^
                      "src must be reg/immediate")

          | wi (Allocs(Rdst dst, int, Rco src)) =
            let 
                val name = newstring "asum"
            in
                (* allocate, putting the pointer to space in dst
                   without screwing up the source register *)
                if src = 0 then wrt "PUSH EAX" else ();
                wrt "PUSH ECX";
                wrt "PUSH EDX";
                wrt ("MALLOC " ^ name ^ ", 12");
                wrt "POP EDX";
                wrt "POP ECX";
                if dst <> 0 then wrt ("MOV " ^ rtos dst ^ ", EAX") else ();
                if src = 0 then wrt "POP EAX" else ();

                (* place the tag *)
                wrt ("MOV DWORD PTR [" ^ rtos dst ^ "], " ^ itos TAL.SUMTAG);

                (* place the int *)
                wrt ("MOV DWORD PTR [" ^ rtos dst ^ " + 4], " ^ stos (Imm int));

                (* place the data *)
                wrt ("MOV DWORD PTR [" ^ rtos dst ^ " + 8], " ^ rtos src);

                (* coerce result. *)
                wrt ("FORGETUNIQUE " ^ name);
                wrt ("COERCE rollsum(<`ttt>, forgetname(" ^ rtos dst ^ "))")
            end

          | wi (Allocs _) = raise ToTAL ("src/dest of Allocs must be regs")

          | wi (Checkt(wt, Rdst dst, Rco src)) =
            let 
                val name = newstring "checkt"
            in
                wrt ("COERCE unroll(" ^ rtos src ^ ")");
                wrt ("NAMEOBJ " ^ name ^ ", " ^ rtos src);
                
                (* get tag *)
                wrt ("MOV " ^ rtos dst ^ ", [" ^ rtos src ^ "]");
                wrt ("CMP " ^ rtos dst ^ ", " ^ itos (TAL.tagfor wt) ^ 
                     " ;; " ^ TAL.nameof wt);
                wrt ("JNE " ^ error 0x5603 "   ;; wrong tag");

                (* get inside *)
                wrt ("MOV " ^ rtos dst ^ ", [rec(forgetname(" ^ rtos src ^ "))+4]");
                wrt ("REMOVENAME " ^ name)
            end
          | wi (Checkt _) = raise ToTAL ("src/dest of Checkt must be regs")

          | wi (Checkit(Rdst tdst, Rdst vdst, Rco src)) =
            let 
                val name = newstring "checkit"
            in
                wrt ("COERCE unroll(" ^ rtos src ^ ")");
                wrt ("NAMEOBJ " ^ name ^ ", " ^ rtos src);
                
                (* get tag *)
                wrt ("MOV " ^ rtos vdst ^ ", [" ^ rtos src ^ "]");
                wrt ("CMP " ^ rtos vdst ^ ", " ^ itos TAL.SUMTAG);
                wrt ("JNE " ^ error 0x6355 "   ;; not sum");

                (* get inside ... XXX forgetname twice? *)
                (* (type-ok to forgetname twice) *)
                wrt ("MOV " ^ rtos tdst ^ ", [rec(forgetname(" ^ rtos src ^ "))+4]");
                wrt ("MOV " ^ rtos vdst ^ ", [rec(forgetname(" ^ rtos src ^ "))+8]");
                wrt ("REMOVENAME " ^ name)
            end
          | wi (Checkit _) = raise ToTAL "all src/dests of Checkit must be regs"

          | wi (Cmp (sa, sb)) = wrt ("CMP " ^ stos sa ^ ", " ^ stos sb)

          | wi (Checkv (Rdst dst, Rco src)) =
            let
                val cname = newstring "tup"
            in
                wrt ("COERCE unroll(" ^ rtos src ^ ")");
                wrt ("NAMEOBJ " ^ cname ^ ", " ^ rtos src);
                
                (* get tag *)
                wrt ("MOV " ^ rtos dst ^ ", [" ^ rtos src ^ "]");
                wrt ("CMP " ^ rtos dst ^ ", " ^ itos TAL.TUPLETAG);
                wrt ("JNE " ^ error 0x7919 " ;; not a tuple");

                (* get inside *)
                wrt ("MOV " ^ rtos dst ^ ", [rec(forgetname(" ^ rtos src ^ "))+4]");
                wrt ("REMOVENAME " ^ cname)
            end
          | wi (Checkv _) = raise ToTAL "src/dest in checkv must be regs"
            
          (* src must hold regular tal array *)
          | wi (Project (Rco i, Rdst dst, Rco src)) =
            let
                val aname = newstring "ar"
                val iname = newstring "idx"
            in

                wrt ("UNPACK " ^ aname ^ ", " ^ rtos src ^ ", " ^ rtos src);
                wrt ("UNPACK " ^ iname ^ ", " ^ rtos i ^ ", " ^ rtos i);

                (* bounds check *)
                wrt ("CMP " ^ rtos i ^ ", [" ^ rtos src ^ "]");
                wrt ("JAE " ^ error 0x8060 " ;; out of bounds");

                (* get data *)
                wrt ("MOV " ^ rtos src ^ ", [" ^ rtos src ^ "+4]");
                wrt ("MOV " ^ rtos dst ^ ", [" ^ rtos src ^ "+4*" ^ rtos i ^ "]")

            end
          | wi (Project _) = raise ToTAL ("srcs/dest in project must be regs")

          (* XXX should take a real tal array as vvv, not check the
             tag itself *)
          (* this is mostly the same as projection. XXX might want to
             factor out the common parts. *)
          | wi (Update (Rco arr, Rco idx, Rco vvv)) =
            let
                (* XXX uses a temporary that's not recorded *)
                val tmp = 5

                val aname = newstring "uar"
                val iname = newstring "uidx"
                val cname = newstring "ucheck"
            in
                wrt ("COERCE unroll(" ^ rtos arr ^ ")");
                wrt ("NAMEOBJ " ^ cname ^ ", " ^ rtos arr);
                
                (* get tag *)
                wrt ("MOV " ^ rtos tmp ^ ", [" ^ rtos arr ^ "]");
                wrt ("CMP " ^ rtos tmp ^ ", " ^ itos TAL.TUPLETAG);
                wrt ("JNE " ^ error 0x9130 " ;; not a tuple");

                (* get inside *)
                wrt ("MOV " ^ rtos arr ^ ", [rec(forgetname(" ^ 
                                                 rtos arr ^ "))+4]");
                wrt ("REMOVENAME " ^ cname);

                (* arr holds regular tal array now. unpack existential *)
                wrt ("UNPACK " ^ aname ^ ", " ^ rtos arr ^ ", " ^ rtos arr);
                wrt ("UNPACK " ^ iname ^ ", " ^ rtos idx ^ ", " ^ rtos idx);

                (* bounds check *)
                wrt ("CMP " ^ rtos idx ^ ", [" ^ rtos arr ^ "]");
                wrt ("JAE " ^ error 0x9738 " ;; out of bounds");

                (* get data *)
                wrt ("MOV " ^ rtos arr ^ ", [" ^ rtos arr ^ "+4]");
                wrt ("MOV [" ^ rtos arr ^ "+4*" ^ rtos idx ^ "], " ^ 
                     rtos vvv)

            end

          | wi (Update _) = raise ToTAL ("srcs must be regs in update")

          (* XXX uses a temporary that's not recorded *)
          | wi (Setref (Rco cell, Rco v)) =
            let 
                val tmp = 5
                val cname = newstring "check_ref"
            in
                wrt ("coerce unroll(" ^ rtos cell ^ ") ; setting ref");
                wrt ("nameobj " ^ cname ^ ", " ^ rtos cell);
                (* get tag *)
                wrt ("mov " ^ rtos tmp ^ ", [" ^ rtos cell ^ "]");
                wrt ("cmp " ^ rtos tmp ^ ", " ^ itos (TAL.tagfor REF));
                wrt ("jne " ^ error 0x10572 " ;; not a ref");

                (* do the dance *)
                wrt ("COERCE forgetname(" ^ rtos cell ^")");
                wrt ("REMOVENAME " ^ cname);

                (* write *)
                wrt ("mov [rec(" ^ rtos cell ^ ") + 4], " ^ rtos v)
                (* that's it! *)
            end

          | wi (Setref _) = raise ToTAL ("src/src in setref must be regs")

          | wi (Boxa (Rdst dst, Rco src)) =
            let 
                val valname = newstring "boxa"
            in
                if src = 0 then wrt "PUSH EAX" else ();

                wrt "PUSH ECX";
                wrt "PUSH EDX";
                wrt ("MALLOC " ^ valname ^ ", 8");
                wrt "POP EDX";
                wrt "POP ECX";

                if dst <> 0 then wrt ("MOV " ^ rtos dst ^ ", EAX") else ();
                if src = 0 then wrt "POP EAX" else ();

                wrt ("MOV DWORD PTR [" ^ rtos dst ^ "], " ^ 
                     itos TAL.TUPLETAG);

                wrt ("MOV [" ^ rtos dst ^ "+4], " ^ rtos src);
                wrt ("FORGETUNIQUE " ^ valname);

                wrt ("COERCE rollsum(<`ttt>, forgetname(" ^ rtos dst ^ "))")

            end

          | wi (Boxa _) = raise ToTAL "only regs for Boxa"

          (* XXX ok? We can never project from an empty tuple, and no other
             primitive lets us look inside. Better might be simply to add
             S(0) to the sum (like popcorn does ?structs), so that we don't
             even have to allocate here. *)
          | wi (Alloct (dst, nil)) = wi (Alloc(dst, INT, Imm 0))

          (* XXX uses a temporary that's not recorded *)
          | wi (Alloct (Rdst dst, stack_srcs)) =
            let
                (* tmp must be =/= 0 *)
                val tmp = 5
                val srcs =
                    map (fn (Sco s) => s
                                | _ => raise ToTAL 
                                        "only stack slots allowed in alloct srcs")
                    stack_srcs
                val n = length srcs
                val datname = newstring "tdata"
                val arrname = newstring "arr"
                val valname = newstring "val"
            in
                (* leaves address of memory in eax *)
                if dst <> 0 then wrt "PUSH EAX" else ();

                wrt ("PUSH ECX     ;; allocating " ^ itos n ^ "-tuple");
                wrt "PUSH EDX";
                wrt ("MALLOC " ^ datname ^ ", " ^ itos (n * 4) ^ ", <[" ^
                     StringUtil.delimit "," (List.tabulate (n, fn _ => ":4")) ^
                     "]>");
                wrt "POP EDX";
                wrt "POP ECX";

                foldl (fn (slot, m) =>
                       let in
                           wrt ("MOV " ^ rtos tmp ^ ", [ESP+" ^ itos (slot * 4) ^ "]");
                           wrt ("MOV [EAX+" ^ itos (m * 4) ^ "], " ^ rtos tmp);
                           m + 1
                       end) 0 srcs;

                wrt ("MOV " ^ rtos tmp ^ ", EAX");

                (* now tmp holds array data *)

                wrt "PUSH ECX";
                wrt "PUSH EDX";
                wrt ("MALLOC " ^ arrname ^ ",8,<[:4,:4]>");
                wrt "POP EDX";
                wrt "POP ECX";

                wrt ("MOV DWORD PTR [EAX], " ^ itos n);
                wrt ("FORGETUNIQUE " ^ datname);
                wrt ("MOV [EAX+4], array(0, 0, <`ttt^rw>, forgetname(" ^ 
                     rtos tmp ^ "))");
                wrt ("FORGETUNIQUE " ^ arrname);

                wrt ("COERCE pack(<" ^ itos n ^ ">, forgetname(EAX), " ^
                     "<?arr `ttt>)");

                wrt ("MOV " ^ rtos tmp ^ ", EAX");

                (* now tmp holds array *)

                (* XXX use boxarray -- watch out for push eax above *)

                wrt "PUSH ECX";
                wrt "PUSH EDX";
                wrt ("MALLOC " ^ valname ^ ", 8");
                wrt "POP EDX";
                wrt "POP ECX";

                wrt ("MOV DWORD PTR [EAX], " ^ itos TAL.TUPLETAG);
                wrt ("MOV [EAX+4], " ^ rtos tmp);
                wrt ("FORGETUNIQUE " ^ valname);

                (* now EAX holds final value *)

                if dst <> 0
                then 
                    (wrt ("MOV " ^ rtos dst ^ ", rollsum(<`ttt>,forgetname(EAX))");
                     wrt ("POP EAX"))
                else 
                    wrt "COERCE rollsum(<`ttt>, forgetname(EAX))"

            end
          | wi (Alloct _) = raise ToTAL "only register dst allowed for alloc tuple"

          | wi (Array0 (Rdst dst, ssize)) =
            call TALFns.array0 (allregsbut [dst]) nil
                [ ADummy ] dst ssize

          | wi (Array0 _) = raise ToTAL "only register args to array0"

          | wi (Getwitvec (Rdst dst, Rco src, ssize)) =
            call TALFns.getwitvec (allregsbut [dst]) nil
                [ AReg src, AMainarg "witness", AMainarg "newarray",
                  AMainarg "newstring" ] dst ssize

          | wi (Getwitvec _) = raise ToTAL "getwitvec needs registers"

          (* Random and Array should be merged. *)

          | wi (Random (Rdst dst, ssize)) =
            let
                val saves = allregsbut [dst]
                val valname = newstring "val"
            in
                app (fn r =>
                     wrt ("push " ^ rtos r)) (rev saves);

                (* any integer *)
                wrt ("push subsume(<B4>, 0)");

                wrt ("mov eax, dword ptr [esp + " ^
                     itos (4 * (ssize + length saves + 1 (* arg *) +
                                TAL.argdepth "random" + 2 (* ret/exn *))) ^
                     "] ; random");

                wrt ("call tapp(eax, <ESP 1 " ^
                     itos (TAL.mainargs + 1 (* arg *) + ssize + length saves +
                           2 (* ret/exn *)) ^ " s1, EBP 1, e1, e2>)");

                if dst <> 0
                then wrt ("mov " ^ rtos dst ^ ", eax")
                else ();

                (* pop one arg *)
                wrt ("add esp, 4");
                
                (* pop saves *)
                app (fn r =>
                     wrt ("pop " ^ rtos r)) saves

            end

          | wi (Random _) = raise ToTAL "random needs reg dst"

          | wi (Array (Rdst dst, Rco n, Rco init, ssize)) =
            let
                val saves = allregsbut [dst]
                val valname = newstring "val"
            in
                app (fn r =>
                     wrt ("push " ^ rtos r)) (rev saves);

                (* push args to new_array *)
                wrt ("push " ^ rtos init);
                wrt ("push " ^ rtos n);
                
                wrt ("mov eax, dword ptr [esp + " ^
                     itos (4 * (ssize + length saves + 2 (* args *)+
                                TAL.argdepth "newarray" + 2 (* ret/exn *))) ^
                     "] ; newarray");

                wrt ("call tapp(eax, <`ttt, ESP 2 " ^ 
                     itos (TAL.mainargs + 2 (* args *) + ssize + length saves + 
                           2 (* ret/exn *)) ^ " s1, EBP 1, e1, e2>)");

                wrt ("mov " ^ rtos n ^ ", eax");
                (* n holds array *)

                wrt ("MALLOC " ^ valname ^ ", 8");

                wrt ("MOV DWORD PTR [EAX], " ^ itos TAL.TUPLETAG);
                wrt ("MOV [EAX+4], " ^ rtos n);
                wrt ("FORGETUNIQUE " ^ valname);

                (* now EAX holds final value *)

                if dst <> 0
                then 
                    (wrt ("MOV " ^ rtos dst ^ 
                               ", rollsum(<`ttt>,forgetname(EAX))");
                     wrt ("POP EAX"))
                else 
                    wrt "COERCE rollsum(<`ttt>, forgetname(EAX))";

                (* pop two args *)
                wrt ("add esp, 8");
                
                (* pop saves *)
                app (fn r =>
                     wrt ("pop " ^ rtos r)) saves

            end

          | wi (Array _) = raise ToTAL "only register src/dst for array"

          | wi (Spawn (Rdst dst, Rco scode, Rco sdeps, ssize, cself)) =
            call TALFns.spawn (allregsbut [dst]) ["vdep", "vdlis"]
               [ AReg scode, AReg sdeps, 
                 AMainarg "submit", AMainarg "dep_one",
                 AMainarg "dep_and", AMainarg "deplist_empty",
                 AMainarg "deplist_cons",
                 if cself then AInt 1 else AInt 0 ]
               dst ssize

          | wi (Spawn _) = raise ToTAL "only registers for spawn"

          | wi (Allocsv (Rdst dst, Rco src, ssize)) =
            call TALFns.boxsv (allregsbut [dst]) nil
               [ AReg src, AMainarg "newarray" ]
               dst ssize

          | wi (Allocsv _) = raise ToTAL "only registers for allocsv"

          | wi (Unboxsv (Rdst dst, Rco src, ssize)) =
            call 
              (* fn *)
              TALFns.unboxsv
              (* registers to save *)
              (allregsbut [dst])
              (* type arguments *)
              nil
              (* arguments *)
              [ AReg src, AMainarg "newarray" ]
              (* destination *)
              dst
              (* current stack size *)
              ssize

          | wi (Unboxsv _) = raise ToTAL "args to unboxsv must be regs"

          | wi (Concat (Rdst dst, Rco src, ssize)) =
            call TALFns.concat (allregsbut [dst]) nil
              [ AReg src, AMainarg "newstring" ] dst ssize

          | wi (Concat _) = raise ToTAL "only regs to concat allowed"

          | wi (Marshall (Rdst dst, Rco src, ssize)) =
            call
              TALMarshall.marshall (allregsbut [dst]) nil
              [ AReg src, AMainarg "newstring" ]
              dst ssize

          | wi (Marshall _) = 
            raise ToTAL "only register src/dst allowed for Marshall"

          | wi (Unmarshvec(Rdst dst, Rco src, ssize)) =
            call
              TALFns.umvec (allregsbut [dst]) nil
              [ AMainarg "newarray", AMainarg "newstring", AReg src ]
              dst ssize

          | wi (Unmarshvec _) = 
            raise ToTAL "only register src/dst allowed for Unmarshvec"

          (* -just- the call. tortl takes care of pushing args, fetching
             result, etc. *)
          | wi (Callpop (what, nargs, ssize)) =
            let 
                val ax = newstring "fn"
            in

                (* check null *)
                wrt ("MOV EAX, DWORD PTR unroll(unroll([_" ^ what ^ "]))");
                wrt ("NAMEOBJ " ^ ax ^ ", EAX");
                wrt ("CMP EAX, 0");
                wrt ("JE " ^ error 0x17715 "was null");
                wrt ("COERCE forgetname(EAX)");
                wrt ("REMOVENAME " ^ ax);
                wrt ("MOV EAX, [EAX]");

                (* ESP takes num of args and then size of stack frame.
                   args is nargs, stack frame = # args to mainfn
                   + ssize (number of local vars)
                   + 1 return address + 1 hemlock handler + nargs *)
                wrt ("CALL tapp(EAX, <ESP " ^ itos nargs ^ " " ^
                     itos (TAL.mainargs + 1 + 1 + ssize + nargs) ^ 
                     " s1, EBP 1, e1, e2>)")
            end
          | wi (Push src) = wrt ("PUSH " ^ stos src)
          | wi (Pop dst) = wrt ("POP " ^ dtos dst)

        in
            case c of
                (C(i, next)) => (wi i; wcode f next)
              | (Return (fwd, Rco src)) =>
                    let in
                        (* overwrite exn handler; need to clear it anyway *)
                        wrt ("mov [esp], " ^ rtos src);

                        wrt ("mov eax, dword ptr [esp + " ^
                             itos (4 * (1 (* ret *) + 1 (* arg *) + 
                                        TAL.argdepth 
                                        (if fwd
                                         then "answer_forward"
                                         else "answer_ok"))) ^ 
                             "] ; aok");

                        wrt ("call tapp(eax, <ESP 1 " ^ 
                             itos (TAL.mainargs + 1 + 1) ^ 
                             " s1, EBP 1, e1, e2>)");
                        
                        (* destroy arg *)
                        wrt ("add esp, 4");

                        (* result already in eax *)
                            
                        wrt ("RETN")
                    end
              | (Return _) => raise ToTAL "return must be in reg"
              | (Jmp src) =>
                    wrt ("JMP tapp(" ^ stos src ^ 
                         ", <vanswer, vdep, vdlis, s1, s2, e1, e2>)")
              | (Error s) =>
                    let in
                        wrt ("JMP " ^ error 0xFFFF s)
                    end
        end

    fun writecode f code =
      let 
        val wr = wr f
        fun wrt s = wr ("\t" ^ s)

        fun labtype Internal = wr ";; internal"
          | labtype Exported =
            let in 
                wr ";; exported";
                wr ("LABELTYPE <" ^ TAL.codetype ^ ">")
            end

        fun one (lab, (topt, code)) =
            let in
                wr "";
                wr (talify lab ^ ":");
                labtype topt;
                wcode f code
            end
      in
        wr "\n\tCODE\n";
          
        app one code
      end

    fun writefoot f =
        let in
            wr f "\n\t_end_TAL";
            wr f "\tEND\n"
        end

    fun ttos TALRun.BINT = "B4"
      | ttos TALRun.BSTRING = "?str"
      | ttos TALRun.BSTRINGVEC = "(?arr (?str))"

    fun codetypeof tdom tcod =
        "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2)," ^
        "ESP: sptr (" ^
        (case tcod of
             NONE => "?Sv"
           | SOME t => "?S " ^ ttos t) ^ " " ^ 
        (StringUtil.delimit "::" (map ttos tdom)) ^
        "::se s1 s2 e1 e2)#(?E s2 e2)})^rw"

    (* these are the globals I use to store functions for the
       client runtime. *)
    fun writeglob fo fx =
        let 
            fun one (name, tdom, tcod, _, t) =
                let 
                    val tdec =
                        ["",
                         "TYPE <" ^ t ^ "? : T4 = ^T(0)`" ^ t ^ "?mem>",
                         "TYPE <" ^ t ^ "?mem : Tm 4 = *[" ^
                         codetypeof tdom tcod ^
                         "]>"]

                in
                    (* write type dec to data section and export file *)
                    app (wr fo) tdec;
                    app (wr fx) tdec;
                    
                    (* data section *)
                    wr fo "";
                    wr fo ("_" ^ name ^ ":");
                    wr fo ("LABELTYPE <^*[`" ^ t ^ "?^rw]>");
                    wr fo ("   DD  rollsum(<`" ^ t ^ "?>, 0)");

                    (* export file *)

                    wr fx ("   VAL  _" ^ name ^ ", <^*[`" ^ t ^ "?^rw]>")
                end
                
        in
            wr fo "";
            wr fo ";; runtime functions for client";

            wr fx "";
            wr fx ";; exported runtime holes";
            app one TALRun.fns
        end

    fun writehead f name =
        let in
            wr f (";; TAL implementation: " ^ name);
            wr f ";; generated by Hemlock -- do not edit!";

            app (fn s =>
                 (wr f ("\t" ^ s)))
            (["",
              "INCLUDE TAL.INC",
              "_begin_TAL",
              "",
              "TAL_IMPORT " ^ name ^ "_i.tali",
              "",
              "TAL_EXPORT " ^ name ^ "_e.tali",
             ""] @ TALMarshall.types)

            (* type ttt is now in imports *)
        end

    fun writedata f data =
        let
            val wr = wr f
            fun one (lab, (String, w8v)) =
                let 
                    val l = Word8Vector.length w8v
                in
                    wr (lab ^ ":");
                    wr ("COERCE pack(<" ^ 
                        itos l ^
                        ">,?,<?str>)");
                    wr ("\tDD " ^ itos l);
                    wr ("\tDD data_" ^ lab);
                    wr "";
                    wr ("data_" ^ lab ^ ":");
                    wr ("LABELTYPE <^*[array(" ^
                        itos l ^ ",B1^rw)]>");
                    wr "COERCE array(0,0,<B1^rw>,?)";
                    wr ("\tDB " ^
                        StringUtil.delimit ", " (Word8Vector.foldr
                                                 (fn (a, b) => 
                                                  itos 
                                                  (Word8.toInt a) :: b)
                                                 nil w8v));
                    wr ""
                end
              | one _ = raise ToTAL "unsupported type in data block"
        in
            wr "\nDATA\n";
            
            StringMap.appi one data;
            app wr TAL.dyninit_data;
            app wr TALMarshall.data;
            app wr TALFns.data;
            app wr (TALUtil.data())
        end

    (* needed during marshalling and unmarshalling, since
       we can't send code labels otherwise. *)

    fun writectab f code entry =
        let
            val wr = wr f
            fun wrt x = wr ("\t" ^ x)
            val ctd = newstring "ctab_data"

            val n = ListUtil.count (fn (_, (Exported, _)) => true 
                                     | _ => false) code

            (* write any external label =/= entry *)
            fun wrcode ((lab, (Internal, _)) :: rest, n) = wrcode (rest, n)
              | wrcode (nil, _) = ()
              | wrcode ((lab, (Exported, _))::rest, n) =
                if lab = entry 
                then (wrcode (rest, n))
                else (wrt ("DD " ^ talify lab ^ "; " ^ itos n);
                      SymbolDB.push "code" n (talify lab);
                      wrcode (rest, n + 1))

        in
            wr "";
            wr "";
            wr ";; code table";
            wr (ctd ^ ":");
            wr ("LABELTYPE <^*[array(" ^ itos n ^ 
                ",(" ^ TAL.codetype ^ ")^rw)]>");
            wr ("COERCE array(0, 0, <(" ^ TAL.codetype ^ ")^rw>,?)");

            (* main function should always have index 0 *)
            wrt ("DD " ^ talify entry ^ "; 0");
            SymbolDB.push "code" 0 (talify entry);

            wrcode (code, 1);

            wr (code_table ^ ":");
            wr ("LABELTYPE <?arr (" ^ TAL.codetype ^ ")>");
            wr ("COERCE pack(<" ^ itos n ^ ">,?,<?arr (" ^ TAL.codetype ^ ")>)");
            wr ("DD " ^ itos n);
            wr ("DD " ^ ctd);
            
            wr ""
        end
    
    fun writemain f entry =
        let
            val wr = wr f
            fun wrt s = wr ("\t" ^ s)

            val ::: = RTL.C
            infixr :::
        in
            wr "";
            wr ";; entry point for cords and client";
            wr "_main_function:";
            wr ("LABELTYPE<" ^ TAL.mainfntype ^ ">");
            app wrt (TAL.mainfn entry TALMarshall.unmarshall);
            (* call closure in EAX *)
            wcode f
            (Mov(Rdst 2, Rco 0) :::

             (* copy arg for tagcheck, since checkv coerces argument *)
             Mov(Rdst 3, Rco 0) :::
             Checkv(Rdst 0, Rco 3) :::

             Mov(Rdst 3, Imm 0) :::

             Project(Rco 3, Rdst 1, Rco 0) :::
             Checkt(CODE, Rdst 0, Rco 1) :::
             (* put closure in reg 3, critical. *)
             Mov(Rdst 3, Rco 2) :::
             (* this held something not of ttt *)
             Mov(Rdst 1, Rco 2) :::
             (* this is kind of bogus; we need to pass
                in arguments, specifically the return continuation.
                best would be if we could figure out how to
                translate this so that it automatically calls
                PFinish... XXX (automatically calls pfinish now,
                but now this part is bogus.)
                *)
             Jmp(Rco 0))
        end

    (* all imported labels that I use. *)
    fun writeimps f name =
        let
            val wr = wr f
            fun wrt s = wr ("\t" ^ s)
        in 
            wr (";; TAL imports for " ^ name);
            wr (";; Generated by Hemlock -- do not edit! ");

            app wrt
            (TAL.poptypes @
             TAL.hemtypes @
            ["",
             (* runtime errors become popcorn null pointer
                exceptions. XXX make our own? *)
             "VAL     " ^ TAL.exnpacket ^ ",<^*[?exn^rw]>",
             ""])
        end

    (* exported labels for client code *)
    fun writeexps f name =
        let
            val wr = wr f
            fun wrt s = wr ("\t" ^ s)
        in 
            wr (";; TAL exports for " ^ name ^ " (client)");
            wr (";; Generated by Hemlock -- do not edit! ");

            app wrt
            (TAL.poptypes @
             ["",
              "VAL     _main_function,<" ^ TAL.mainfntype ^ ">",
              ""])
        end

    (* exported labels for cord code *)
    fun writeexpsc f name =
        let
            val wr = wr f
            fun wrt s = wr ("\t" ^ s)
        in 
            wr (";; TAL exports for " ^ name ^ " (cord)");
            wr (";; Generated by Hemlock -- do not edit! ");

            app wrt
            (TAL.poptypes @
             ["",
              "VAL     _dyninit_main,<" ^ TAL.dyninittype ^ ">",
              ""])
        end

    fun writehand f = app (wr f) (TALUtil.code ())

    fun writeload f =
        let val wr = wr f
        in
            (* XXX put name somewhere in TAL, use same in exports *)
            wr "_dyninit_main:";
            wr ("LABELTYPE <" ^ TAL.dyninittype ^ ">");
            app wr TAL.dyninit
        end

    fun writefns f =
        let in
            wr f ";; some primitives (talfns.sml)";
            app (app (wr f)) (TALFns.code ())
        end

    fun writemarsh f =
        let val wr = wr f
        in
            wr ";; marshalling/unmarshalling (marshall.sml)";
            app (app wr) (TALMarshall.code ())
        end

    (* basic constraint: can only forward-reference Internal code. *)
    fun sortcode c =
        let
            val info = StringMap.mapi (fn (lab, stuff as (Internal, _)) => 
                                       SOME (TopoSort.node (lab, stuff))
                                       | (lab, stuff) => NONE) c

            (* now info : lab => node option *)

            (* get all of the blocks in a list *)
            val all = StringMap.foldri (fn (k, a, b) => (k, a) :: b) nil c

            (* external (known type) code can come first *)
            val (ext, int) = List.partition (fn (_, (Exported, _)) => true
                                              | _ => false) all

            (* sort internal topologically so that there are no
               backward references -- will always succeed because
               of original tree structure of CPS code *)

            (* get node from label. join because 
               lookup may fail; inside may be NONE. *)
            fun getn lab = Option.join (StringMap.find (info, lab))

            (* XXX I think this covers all the cases I actually
               generate, but if not, then add instructions here. 

               The symptom will be an error message like:

               code block ret21(15):
                 verifying instructions:
                 backward branch to label requires type 
                           (cannot be inferred): is_0__1
               *)
            fun getref (Jc (_, Lab l)) = [l]
              | getref _ = nil

            fun getrefs (Jmp(Lab l)) = [l]
              | getrefs (Jmp _) = nil
              | getrefs (Return _) = nil
              | getrefs (Error _) = nil
              | getrefs (C(i,c)) = getref i @ getrefs c

            fun getc (lab, (_, code)) =
                (case getn lab of
                     SOME this =>
                         List.mapPartial (fn dep =>
                                          Option.map (fn that => (this, that))
                                                     (getn dep)) 
                                   (getrefs code)
                   | NONE => raise ToTAL "impossible!")

            (* get constraints and nodes *)
            val cint = List.concat (map getc int)

(*
            val _ =
                app (fn (a, b) =>
                     let 
                         val ((this,_),(that,_)) = (TopoSort.get a,
                                                    TopoSort.get b)
                     in
                         print (this ^ " depends on " ^ that ^ "\n")
                     end) cint
*)

            val const = map TopoSort.constraint cint
            val nodes = List.mapPartial (fn (lab, _) => getn lab) int

            val sorted = TopoSort.sort nodes const

            val newint = map TopoSort.get sorted

(*
            val _ = print "\nSorted order:\n";
            val _ =
                app (fn (lab, _) =>
                     print (lab ^ "\n")) newint;
*)              
        in
            ext @ newint
        end handle TopoSort.TopoSort s => 
                raise ToTAL ("bug: topological sort failed: " ^ s)


    fun write ({code, entry, data} : RTL.program) file =
        let
            (* TAL needs certain ordering constraints to be
               satisfied in order to do type inference. *)
            val code = sortcode code
            (* now code is a list *)

            val out = TextIO.openOut (file ^ ".tal")
            val iout = TextIO.openOut (file ^ "_i.tali")
            val eout = TextIO.openOut (file ^ "_e.tali")
            val xout = TextIO.openOut (file ^ "_x.tali")
        in
            (* imports *)
            writeimps iout file ;
            writeexps eout file ;
            writeexpsc xout file ;

            (* header *)
            writehead out file ;

            (* all code *)
            writecode out code ;

            (* main_function is the entry point for cords and clients *)
            writemain out entry ;

            (* dyninit loader *)
            writeload out ;

            (* some primop implementation *)
            writefns out ;

            (* marshalling *)
            writemarsh out ;

            (* code for handling runtime errors *)
            writehand out ;

            (* all data blocks *)
            writedata out data ;

            (* globals that I use to store some popcorn stuff *)
            writeglob out eout ;
            
            (* table of functions for marshalling *)
            writectab out code entry ;

            (* footer *)
            writefoot out ;

            TextIO.closeOut iout;
            TextIO.closeOut eout;
            TextIO.closeOut xout;
            TextIO.closeOut out

        end handle IO.Io {cause,function,name} => 
                    raise ToTAL ("IO error: " ^ function ^ ": " ^ name)

end