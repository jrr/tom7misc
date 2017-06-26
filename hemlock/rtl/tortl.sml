
(* XXX the following comment is not completely
   up-to-date *)

(* Converts CPS language to RTL.

   Conversion is fairly straightforward. Here's
   a description of each case.

   Global invariants: Code is closure-converted
   (cps/closure.sml) and alloc-converted (cps/alloc.sml).
   
   Fix: These are only at the outermost level. We
   generate a block of code for each. A block
   body proceeds as follows:

        checks: No checks need to be performed.

        invariants at entry to a block:
          All general purpose registers are 
          filled with arguments except r0, which
          was used to jump here. If the function 
          takes fewer than NREG-1 arguments, registers 
          at the tail end will contain useless values.

          SP points to a stack with one element: the
          current exception handler. (It might be nice
          to be able to use this as an argument register
          since we don't actually need it to hold 
          information across jumps.)

        block prolog:
          If the block uses n temporaries,
          reserve n words on the stack to hold
          those temporaries. 

          Move the arguments into their stack
          positions.

        block:
          Within the block, simply translate the
          stream of instructions. No 'Fix' will
          be encountered.

        block epilog:
          No block ever "returns", so the epilog
          is built into the various primitives that
          can end a block. (See App, Primop, etc.)


   App(v,a): This is one way to end a sequence of CPS
   instructions, essentially, a jump to a label.

        checks: We must check that the argument v is
          indeed a code label. All code has the same
          type, namely {R1: t, R2: t, ... RNREG: t} 
          -> void. We may want to avoid this
          check in the case that the argument is a
          literal label (rather than allocating the
          value and then immediately destructing it!)

        invariants: No app will have more than NREG
        arguments.

        code: We simply fill the registers with their 
          arguments and generate the call. Because
          a function might take fewer actual arguments
          than NREG, we must also load the remaining
          arguments with well-typed values. (We use
          copies of the first argument.) In the
          case that there are no arguments, we must 
          generate a new value to use for all slots.

   Project(idx, val, var, rest): Pull a value out of 
   a tuple.

        checks: The value must be a tuple of length
          greater than idx. A "tuple" tag means the
          second word is a size field. (Essentially,
          tuples are arrays.)

        invariants: No tuple is longer than MAXRECORD
          elements.

        code: We index into the tuple and move the
          word into the destination pseudoregister.

   Deferred(os): A delayed expression. Simply fetch
   the contents of the oneshot and translate that.

        invariants: The oneshot has been set!

   Intswitch(val, branches, def): "fast" case on
   integer values.

        checks: Check that the value is an integer.

        invariants: There are no repeated tests in
        the list of branches.

        code: currently, just do linear search
        through the options. Binary trees or 
        jump tables would be cool some day, since
        this code is also used for sumswitches
        and datatype dispatch is a large part of
        what our sample apps do.

   Sumswitch(val, var, branches, def): case on
   object language sums.

        checks: The value must have tag INT_T.

        code: Assign the second element from the pair
        into the pseudoregister var. Then, essentially
        do an intcase on the integer tag. This is ok
        since the type of the second element is always
        t, regardless of the value of the int.
   
   Alloc(TUPLE n, vas, var, rest): Allocate a new tuple value.

        checks: Nothing needs to be checked.

        invariants: The length of vas matches n.

        code: This is the trickiest one.
          Allocation is handled differently in the
          various typed target languages. Since
          TALT's is the most restrictive, we do it
          the "TALT way."

          First, we need to move all of the values
          onto the stack. This is annoying because
          we are using the stack for temporaries;
          each time we push onto it, we need to
          adjust our frame.

          Then, we allocate the appropriate amount.
          This is:
             one word for the tag +
             one word for the tuple length +
             one word for each argument.

          Then, we pop from the stack into the
          uninitialized memory. We emit a mark
          indicating that we are done with 
          allocation.

   Alloc(other tag, vas, var rest): Allocate a regular value.

        checks: Nothing.

        invariants: Length of vas is appropriate for
          the supplied tag. (ie, 1)

        code: Other allocations can be performed 
        in registers. Just allocate, and move.

   Primop: This is where all the actual computation
   takes place. The various primops are all compiled
   differently.

     Arithmetic Primops:

        checks: Both operands must be tagged integers.
        
        code: Load both integers into registers
        EAX, EBX, and run the corresponding machine
        instruction with a result in EAX. Allocate
        an INT containing this and put it in the
        specified stack location.

        optimizations: If either operand is a literal
        integer, especially a small integer, we should 
        probably not allocate these first.

     Bind Primop: This shouldn't occur if optimizations
     have been run, but it's easy enough to implement.

        code: Load from the source and move into the
        destination.

     Ref Get:

        checks: Check that the operand has tag REF.

        code: The second component of the value
        is a pointer to an object of type t. Return
        that pointer.

     Ref Set:

        checks: Check that the operand has tag REF.
        
        code: Set the ref cell's second field to
        its new value.

        (XXX it's important that we never reconstruct
        a reference cell. Unlike a normal implementation,
        the value of the reference cell IS the cell that
        is modified. (We never need to duplicate values
        because values live in a fixed location on the
        heap and we refer to them uniquely by their 
        addresses.) If it turns out we ever need to
        duplicate values, then we'll need to add another
        level of indirection here.)

     Exception Get:

        code: Return the bottom of the stack, which
        is the current exception handler (a closure).

     Exception Set:

        check: None.

        code: Overwrite the bottom of the stack with
        the argument (a closure).

     Grid Forget:

        (XXX make a call to 'forget' in runtime. This
        means doing the normal calling convention thing.)

     Grid Finish:

        (XXX marshall result, return it in EAX?
         should the stack contain at its very bottom
         the overall return address? or do we make
         a call that never returns?)

     Grid Spawn:

        (XXX marshall argument, )

   *)


structure ToRTL :> TORTL =
struct

  exception ToRTL of string

  structure C = CPS
  structure V = Variable
  open RTL

  open Primop

  val MAIN = "__hemlock_main"

  val NREG = Architecture.NREG

  infixr 9 `
  fun a`b = a b
  fun list x = [x]
  fun K x y = x

  val ::: = C
  infixr ::: <::

  fun i <:: (code, blocks, data) = (i ::: code, blocks, data)

  fun listn 0 _ = nil
    | listn n k = k :: listn (n - 1) k

  fun getslot m v = 
      case V.Map.find (m, v) of
          NONE => raise ToRTL ("BUG: variable " ^ V.tostring v ^ 
                               " not assigned slot")
        | SOME s => s

  val getlabel = V.tostring

  local
      val ctr = ref 0
  in
      fun newlabel s = 
          (ctr := !ctr + 1;
           s ^ "__" ^ Int.toString (!ctr))
  end


  (* In this conversion, function names are labels, so we always convert
     them from variables with V.tostring. *)
  fun convert cexp =
      let 
          val (fs, main) = 
              case cexp of
                  C.Fix(fs, main) => (fs, main)
                | _ => (nil, cexp)

          (* Main is treated as a function with no arguments. *)
          val fs = (MAIN, nil, main) :: 
              map (fn (name, args, code) =>
                   (getlabel name, args, code)) fs

          val (code, data) = ListPair.unzip ` map cfn fs

          fun mkmap l = foldl StringMap.insert' StringMap.empty l
      in
          { entry = MAIN,
            code = mkmap ` List.concat code,
            data = mkmap ` List.concat data }
      end

  (* convert a function. The name of the function will be 'label',
     the args are named in 'args', and the code is 'cexp'. 

     return a list of RTL blocks labeled with their names and types,
        and a list of RTL data   labeled with their names and types.
     *)
  and cfn (label, args, cexp) =
      let
          val argset = foldl (fn (a,s) => V.Map.insert(s, a, ())) 
                             V.Map.empty args
          val slots = V.Map.unionWith (K ()) (argset, getvars cexp)

          (* Assign a stack slot for each variable in the set. 

             Also, it may make sense to grow the stack incrementally
             as we go, since deep accesses are expensive. (-- not true
             except in instruction length!) 
             *)

          val (smap, nslots) = V.Map.foldli (fn (v, (), (m, slot)) => 
                                             (V.Map.insert(m, v, slot), 
                                              slot + 1))
                                            (V.Map.empty, 0) slots


          (* val _ = print ("num: " ^ Int.toString nslots ^ "\n") *)

          (* translate instruction stream *)
          val (code, blocks, data) = converti (smap, nslots) cexp

          (* prolog: move arguments from registers into appropriate 
             stack slots. Note that this simply ignores any bogus
             arguments that were passed in. *)

          fun mkmoves nil r = code
            | mkmoves (v::t) r =
              Mov (Sdst ` getslot smap v, Rco r)
              ::: mkmoves t (r + 1)

          (* make enough space for all variables *)
          val allcode = Salloc nslots ::: mkmoves args 1

      in
          (* SOME ` Code(Junk :: List.tabulate (NREG-1, K T), [T]) *)
          ((label, (Exported,
                    allcode)) :: blocks, data)
      end

  (* create a map of variables used in this cexp. This is used to
     determine the number of stack slots we need to reserve in a
     block prolog. We examine the entire tree of expressions (for
     instance, through a switch). *)
  and getvars cexp =
      let 
          fun union (s1, s2) = V.Map.unionWith (K ()) (s1, s2)
          fun insert v s = V.Map.insert (s, v, ())
          fun fromlist l = foldl (fn (a, s) => insert a s) V.Map.empty l

          fun vals s = 
              fromlist (List.mapPartial (fn (C.Var v) => SOME v | _ => NONE) 
                        s)
      in
          case cexp of
              C.Alloc(_, va, v, c) => union (insert v ` getvars c,
                                             vals va)
            | C.Project(_, va, v, c) => union (insert v ` getvars c,
                                               vals [va])
            | C.App a => vals (op:: a)
            | C.Fix _ => raise ToRTL "BUG: saw fix in getvars"
            | C.Intswitch (va, icl, def) => 
                  union (foldl union V.Map.empty 
                           ` map getvars (def :: map #2 icl),
                         vals [va])
            (* vr doesn't count unless it's actually used -- saves us
               a stack slot and the mov instructions...

               XXX this opt would probably be more profitable for
               Primop, since many are done just for effect.  *)
            | C.Sumswitch (va, vr, icl, def) => 
                  union (foldl union V.Map.empty 
                           ` map getvars (def :: map #2 icl),
                         vals [va])
            | C.Primop (_, va, vl, cl) => 
                  union (foldl union (fromlist vl) 
                           ` map getvars cl,
                         vals va)
            | C.Deferred os => 
                  case Util.Oneshot.deref (os()) of
                      NONE => raise ToRTL "unset oneshot in getvars"
                    | SOME ce => getvars ce
      end

  (* convert an expression cexp with variable mapping (to stack slots)
     'smap'. Return that code, as well as any blocks (label, (typ, code))
     and data (label, (typ, data)) generated /en passant/. *)
  and converti (frame as (smap, ssize)) cexp =
  let

   fun getloc (C.Var v) = 
       (case V.Map.find (smap, v) of
            NONE => raise ToRTL ("no stack slot for " ^ V.tostring v)
          | SOME s => s)
     | getloc _ = raise ToRTL "tried to get location of a constant"

   (* generate a switch on 'on' using linear
      search. This could be improved with binary
      search (for a large number of clauses) or
      jump tables (?).

      for binary search, note that JCC doesn't
      modify flags on the x86, so you can CMP
      and then do two conditional jumps for
      the less/above cases.
      *)
   fun doswitch _ nil def = ci def 
     | doswitch on ((i,ce)::t) def =
       let 
           val foundl = newlabel ("is_" ^ Int.toString i)

           (* test succeeds *)
           val (fc, fb, fd) = ci ce

           (* continue ... *)
           val (cc, cb, cd) = doswitch on t def
       in

           (* label type is something like this,
              but not really because some stack slots
              may not have been filled yet, so they have
              Junk, not T. *)
           (* (SOME `Code(List.tabulate(NREG, fn r => if r = on then Int
                                                      else Junk),
                          listn (ssize + 1) T) *)

           (Cmp(Rco on, Imm i) :::
            Jc(E, Lab foundl) ::: 
            cc,
            (foundl, (Internal, fc)) :: (fb @ cb),
            cd @ fd)
       end

   and ci cexp = 
     (case cexp of
          C.Deferred os => ci ` valOf ` Util.Oneshot.deref ` os()
        | C.Fix _ => raise ToRTL "code has not been closure-converted"
        | C.Alloc (tag, vl, vr, ce) =>
           (case tag of
              C.TUPLE i =>
                  (* tuples are mainly handled by the target
                     language *)
                  Alloct(Rdst 0, map (Sco o getloc) vl) <::
                  Mov(Sdst ` getloc ` C.Var vr, Rco 0) <::
                  ci ce
            | C.INT_T t =>
                  (case vl of
                       [v as C.Var _] =>
                           Mov(Rdst 1, Sco ` getloc v) <::
                           Allocs(Rdst 0, t, Rco 1) <::
                           Mov(Sdst ` getloc ` C.Var vr, Rco 0) <::
                           ci ce
                     | _ => raise ToRTL "bad arg to alloc int_t")
            | any =>
                  let
                      fun getwt C.INT = INT
                        | getwt C.STRING = STRING
                        | getwt C.REF = REF
                        | getwt C.CODE = CODE
                        | getwt _ = raise ToRTL "getwt: impossible"

                      val wt = getwt tag

                      fun k src =
                          Alloc(Rdst 0, wt, src) <::
                          Mov(Sdst ` getloc ` C.Var vr, Rco 0) <::
                          ci ce
                  in
                      case (vl, wt) of
                          ([C.Int i], INT) => k ` Imm i
                        | ([C.String s], STRING) =>
                              let 
                                  (* XXX probably good to try to coalesce
                                     identical strings, since they are
                                     not mutable *)

                                  (* XXX probably need to include its
                                     length, too *)
                                  val slab = newlabel 
                                      ("string_" ^
                                       (StringUtil.harden
                                        (StringUtil.charspec "A-Za-z0-9")
                                        #"_" 10 s))
                                  val (code,
                                       blocks,
                                       data) = k ` Lab slab
                              in
                                  (code, blocks,
                                   (slab, (String, 
                                    Word8Vector.tabulate 
                                      (size s,
                                       fn i => Word8.fromInt ` 
                                               ord ` 
                                               CharVector.sub(s, i)))) :: data)
                              end
                        | ([C.Label l], CODE) => k ` Lab ` getlabel l
                        | ([va as C.Var v], _) => 
                             Mov(Rdst 1, Sco ` getloc va) <:: k (Rco 1)
                        | _ => raise 
                             ToRTL "bad constant or too many args inside alloc"
                  end)

        | C.Intswitch (va, clauses, def) =>
              Mov(Rdst 0, Sco ` getloc va) <::
              Checkt(INT, Rdst 1, Rco 0) <::
              doswitch 1 clauses def

        (* always bind inside, even for def *)
        | C.Sumswitch (va, vr, clauses, def) =>
              (* read case object *)
              Mov(Rdst 0, Sco ` getloc va) <::
              (* check that it is tagged int_t *)
              Checkit(Rdst 2, Rdst 1, Rco 0) <::

              (* bind contents : T, but only if the variable
                 is in the map (ie, used somewhere) *)
              (case V.Map.find (smap, vr) of
                   NONE => doswitch 2 clauses def
                 | SOME s => 
                       Mov(Sdst ` getloc ` C.Var vr, Rco 1) <::
                       (* generate switch on r2 *)
                       doswitch 2 clauses def)

        | C.Project (i, va, vr, ce) =>
              (* load arg into register *)
              Mov(Rdst 1, Sco ` getloc va) <::

              (* check tag *)
              Checkv(Rdst 0, Rco 1) <::

              (* load idx into register *)
              Mov(Rdst 2, Imm i) <::

              (* project out *)
              Project(Rco 2, Rdst 1, Rco 0) <::

              (* put back in stack slot *)
              Mov(Sdst ` getloc ` C.Var vr, Rco 1) <:: ci ce
        | C.App (what, args) =>
              let

                  (* load all arguments into regs, duplicating
                     the first argument at the tail end if
                     we don't use all arguments. *)

                  val nargs = length args

                  fun genmoves () =
                      let
                          fun gm nil =
                              (Sdelete ssize :::
                               (case what of
                                    C.Label l => Jmp ` Lab ` getlabel l
                                  | _ => Jmp ` Rco 0),
                                    nil, nil)
                            | gm ((reg, src)::t) =
                              Mov(Rdst reg, src) <:: gm t
                      in
                          if nargs > NREG - 1
                          then raise ToRTL ("too many arguments!\n" ^
                                            CPSPrint.etosi 3 cexp)
                          else if nargs = 0
                               then 
                                   (* allocate a dummy value into
                                      r 1, then generate copies for
                                      the remainder *)
                                   Alloc(Rdst 1, INT, Imm 0) <::
                                   gm `
                                   List.tabulate
                                   (NREG - 2,
                                    (fn i =>
                                     (i + 2, Rco 1)))
                               else 
                                   (* copy args, but fill remaining
                                      slots with moves from R 1. *)
                                   gm `
                                   List.tabulate 
                                   (NREG - 1,
                                    (fn i =>
                                     if i >= nargs
                                     then (i + 1, Rco 1)
                                     else (i + 1, Sco ` getloc `
                                                  List.nth (args, i))))
                      end
              in
                  case what of
                      C.Label _ => genmoves ()
                    | C.Var v => 
                          Mov(Rdst 1, Sco ` getloc what) <::
                          Checkt (CODE, Rdst 0, Rco 1) <::
                          genmoves ()
                    | _ => raise ToRTL "app of bad value"
              end

        | C.Primop (PNewtag, [], [vr], [ce]) => 
              Random(Rdst 1, ssize) <::
              Alloc(Rdst 0, INT, Rco 1) <::
              Mov (Sdst ` getloc ` C.Var vr, Rco 0) <::
              ci ce
        | C.Primop (PNewtag, _, _, _) => raise ToRTL "bad newtag"

        | C.Primop (PGethandler, [], [vr], [ce]) =>
              Mov(Rdst 0, Sco ssize) <::
              Mov(Sdst ` getloc ` C.Var vr, Rco 0) <::
              ci ce
        | C.Primop (PGethandler, _, _, _) => raise ToRTL "bad gethandler"

        | C.Primop (PSethandler, [va as C.Var _], [], [ce]) =>
              Mov(Rdst 0, Sco ` getloc va) <::
              Mov(Sdst ssize, Rco 0) <::
              ci ce
        | C.Primop (PSethandler, [C.Label l], [], [ce]) =>
              Mov(Sdst ssize, Lab ` getlabel l) <::
              ci ce
        | C.Primop (PSethandler, _, _, _) => raise ToRTL "bad sethandler"

        | C.Primop (PGet, [va], [vr], [ce]) =>
              Mov (Rdst 0, Sco ` getloc va) <::
              Checkt (REF, Rdst 1, Rco 0) <::
              Mov (Sdst ` getloc ` C.Var vr, Rco 1) <::
              ci ce
        | C.Primop (PGet, _, _, _) => raise ToRTL "bad get"

        | C.Primop (PSet, [lhs, rhs], [], [ce]) => 
              Mov (Rdst 0, Sco ` getloc rhs) <::
              Mov (Rdst 1, Sco ` getloc lhs) <::
              Setref (Rco 1, Rco 0) <::
              ci ce
        | C.Primop (PSet, _, _, _) => raise ToRTL "bad set"

        (* arithmetic *)
        | C.Primop (B bo, [a, b], [vr], [ce]) =>
           let
             (* note: these register numbers can't
                be changed! *)
             fun divmod () =
               getargs a b (fn ra =>
                            fn sb =>
                            (* XXX don't do if ra = 0 *)
                            Mov (Rdst 0, Rco ra) <::
                            Quotrem(sb) <::
                            (case bo of
                               PDiv => Alloc(Rdst 1, INT, Rco 0)
                             | PMod => Alloc(Rdst 1, INT, Rco 3)
                             | _ =>  raise ToRTL "impossible:divmod") <::
                            Mov(Sdst ` getloc ` C.Var vr, Rco 1) <::
                            ci ce)

             (* ditto. *)
             fun mul () =
               getargs a b (fn ra =>
                            fn sb =>
                            (* XXX don't do if ra = 0 *)
                            Mov (Rdst 0, Rco ra) <::
                            Multiply(sb) <::
                            Alloc(Rdst 1, INT, Rco 0) <::
                            Mov(Sdst ` getloc ` C.Var vr, Rco 1) <::
                            ci ce)

             fun regular () =
              getargs a b (fn ra =>
                           fn sb =>
                           Op (case bo of
                                   PPlus => ADD
                                 | PMinus => SUB
                                 | _ => raise ToRTL "bad arith op",
                               ra, sb) <::

                           (* alloc int *)
                           Alloc(Rdst 1, INT, Rco ra) <::
                           Mov (Sdst ` getloc ` C.Var vr, Rco 1) <::
                           ci ce)
           in
              (case bo of
                 PDiv => divmod ()
               | PMod => divmod ()
               | PTimes => mul ()
               | _ => regular ())
           end

        (* comparisons *)
        | C.Primop (B (PCmp co), [a, b], [], [tt, ff]) =>
              getargs a b (fn ra =>
                           fn sb =>
                           let 
                               val succl = newlabel "compare"
                               val (succc, succb, succd) = ci tt
                               val (failc, failb, faild) = ci ff
                           in
                               (Cmp (Rco ra, sb) :::
                                Jc (case co of
                                        PEq => E
                                      | PNeq => NE
                                      | PLess => L
                                      | PLesseq => LE
                                      | PGreater => G
                                      | PGreatereq => GE,
                                    Lab succl) :::
                                failc, 
                                (succl, (Internal, succc)) :: (failb @ succb),
                                faild @ succd)
                           end)

        | C.Primop (PStdout, [], [v], [rest]) =>
              (* use knowledge that stdout is just fd 0 *)
              Alloc(Rdst 0, INT, Imm 0) <::
              Mov(Sdst ` getloc ` C.Var v, Rco 0) <::
              ci rest
        | C.Primop (PStdout, _, _, _) => raise ToRTL "bad args to stdout prim"

        (* Return either "ok" (which=false) 
           or "forward" (which=true)

           *)
        | C.Primop (PReturn fwd, [a], [], []) =>
              Mov (Rdst 1, Sco ` getloc a) <::
              Checkt (STRING, Rdst 0, Rco 1) <::
              Sdelete ssize <::
              (Return (fwd, Rco 0), nil, nil)

        | C.Primop (PReturn _, _, _, _) => raise ToRTL "bad args to return"

        | C.Primop (PMarshall, [ttt], [sss], [c]) =>
               Mov (Rdst 1, Sco ` getloc ttt) <::
               Marshall (Rdst 0, Rco 1, ssize) <::
               Alloc(Rdst 1, STRING, Rco 0) <::
               Mov (Sdst ` getloc ` C.Var sss, Rco 1) <::
               ci c

        | C.Primop (PMarshall, _, _, _) => 
               raise ToRTL "bad args to marshall"

        (* XXX it would be nice to generalize this
           "just call a function" stuff. *)
        | C.Primop (PUnmarshvec, [aaa], [out], [c]) => 
               Mov (Rdst 0, Sco ` getloc aaa) <::
               Checkv (Rdst 1, Rco 0) <::
               Unmarshvec (Rdst 0, Rco 1, ssize) <::
               Mov (Sdst ` getloc ` C.Var out, Rco 0) <::
               ci c

        | C.Primop (PUnmarshvec, _, _, _) => 
               raise ToRTL "bad args to unmarshvec"

        | C.Primop (PGetwitvec, [ids], [v], [c]) => 
               Mov (Rdst 0, Sco ` getloc ids) <::
               Checkv (Rdst 1, Rco 0) <::
               Unboxsv (Rdst 0, Rco 1, ssize) <::
               Getwitvec (Rdst 1, Rco 0, ssize) <::
               Allocsv (Rdst 0, Rco 1, ssize) <::
               Mov (Sdst ` getloc ` C.Var v, Rco 0) <::
               ci c

        | C.Primop (PGetwitvec, _, _, _) => 
               raise ToRTL "bad args to getwitvec"

        (* if optimization is turned off, we might see this *)
        | C.Primop (PBind, [va], [vr], [c]) =>
               Mov (Rdst 0, Sco ` getloc va) <::
               Mov (Sdst ` getloc ` C.Var vr, Rco 0) <::
               ci c

        | C.Primop (PBind, _, _, _) =>
               raise ToRTL "bad args to bind"

        | C.Primop (PArraySize, [arr], [n], [c]) => 
                 Mov (Rdst 0, Sco ` getloc arr) <::
                 ArraySize (Rdst 1, Rsrc 0) <::
                 Alloc(Rdst 0, INT, Rsrc 1) <::
                 Mov (Sdst ` getloc ` C.Var n, Rco 0) <::
                 ci c

        | C.Primop (PArraySize, _, _, _) =>
                 raise ToRTL "bad args to arraysize"

        | C.Primop (PArray, [n, init], [arr], [c]) =>
               Mov (Rdst 0, Sco ` getloc n) <::
               Checkt (INT, Rdst 1, Rco 0) <::
               Mov (Rdst 2, Sco ` getloc init) <::
               Array (Rdst 0, Rco 1, Rco 2, ssize) <::
               Mov (Sdst ` getloc ` C.Var arr, Rco 0) <::
               ci c

        | C.Primop (PArray, _, _, _) =>
               raise ToRTL "bad args to array"

        (* can't just rewrite to tuple 0, because we
           do an optimization that rewrites those to
           int 0. *)
        | C.Primop (PArray0, [], [arr], [c]) =>
               Array0(Rdst 1, ssize) <::
               Boxa(Rdst 0, Rco 1) <::
               Mov(Sdst ` getloc ` C.Var arr, Rco 0) <::
               ci c

        | C.Primop (PArray0, _, _, _) => 
               raise ToRTL "bad args to array0"

        | C.Primop (PSub, [va, i], [vr], [ce]) => 
              Mov(Rdst 0, Sco ` getloc i) <::
              Checkt(INT, Rdst 1, Rco 0) <::

              (* load arg into register *)
              Mov(Rdst 2, Sco ` getloc va) <::

              (* check tag *)
              Checkv(Rdst 0, Rco 2) <::

              (* project out *)
              Project(Rco 1, Rdst 2, Rco 0) <::

              (* put back in stack slot *)
              Mov(Sdst ` getloc ` C.Var vr, Rco 2) <:: ci ce

        | C.Primop (PSub, _, _, _) =>
              raise ToRTL "bad args to sub"

        | C.Primop (PUpdate, [arr, i, a], [unit], [ce]) => 
              Mov(Rdst 0, Sco ` getloc i) <::
              Checkt(INT, Rdst 1, Rco 0) <::
              Mov(Rdst 4, Sco ` getloc a) <::
              Mov(Rdst 3, Sco ` getloc arr) <::

              Update(Rco 3, Rco 1, Rco 4) <::

              (* use 0 for unit, cheaper *)
              Alloc(Rdst 0, INT, Imm 0) <::
              Mov(Sdst ` getloc ` C.Var unit, Rco 0) <::
              ci ce

        | C.Primop (PUpdate, _, _, _) => 
              raise ToRTL "bad args to update"

        | C.Primop (PSpawn cself, [code, deps], [cord], [ce]) =>
              Mov(Rdst 0, Sco ` getloc deps) <::
              Checkv(Rdst 1, Rco 0) <::
              Unboxsv(Rdst 0, Rco 1, ssize) <::
              Mov(Rdst 2, Sco ` getloc code) <::
              Checkt(STRING, Rdst 1, Rco 2) <::
              Spawn(Rdst 2, Rco 1, Rco 0, ssize, cself) <::
              Alloc(Rdst 0, STRING, Rco 2) <::
              Mov(Sdst ` getloc ` C.Var cord, Rco 0) <::
              ci ce

        | C.Primop (PSpawn _, _, _, _) =>
              raise ToRTL "bad args to spawn"

        | C.Primop (PConcat, [vec], [res], [ce]) => 
              Mov(Rdst 1, Sco ` getloc vec) <::
              Checkv(Rdst 0, Rco 1) <::
              Unboxsv(Rdst 1, Rco 0, ssize) <::
              Concat(Rdst 0, Rco 1, ssize) <::
              Alloc(Rdst 1, STRING, Rco 0) <::
              Mov(Sdst ` getloc ` C.Var res, Rco 1) <::
              ci ce

        | C.Primop (PConcat, _, _, _) => raise ToRTL "bad args to concat"

        | C.Primop (po, args, [v], [rest]) =>
              (* all other primops are implemented 
                 with calls into runtime *)
              (case ListUtil.Alist.find op = Runtime.fns po of
                  SOME (name, dom, cod, _, _) =>
                      let
                          (* CALL trashes these. *)
                          val callersave = [2, 3, 4, 5]
                          val lcs = length callersave
                          (* used to unbox vectors *)
                          val tmp = 4

                          fun allregsbut l =
                              List.filter (fn x => not 
                                           (List.exists (fn y => y = x) l)) 
                                               [0, 1, 2, 3, 4, 5]

                          fun insns f nil i = i
                            | insns f (h::t) i = f h <:: insns f t i

                          fun prepare Runtime.BINT dst src _ c =
                              Checkt(INT, Rdst dst, Rco src) <:: c
                            | prepare Runtime.BSTRING dst src _ c =
                              Checkt(STRING, Rdst dst, Rco src) <:: c
                            | prepare Runtime.BSTRINGVEC dst src ssize c =
                                  Checkv(Rdst tmp, Rco src) <::
                                  Unboxsv(Rdst dst, Rco tmp, ssize) <::
                                  c

                          fun create Runtime.BINT dst src _ c =
                              Alloc(Rdst dst, INT, Rco src) <:: c
                            | create Runtime.BSTRING dst src _ c =
                              Alloc(Rdst dst, STRING, Rco src) <:: c
                            | create Runtime.BSTRINGVEC dst src _ c =
                                  (* box vector *)
                                  Allocsv(Rdst dst, Rco src, ssize) <:: c
                                  (* XXX allocsv should use boxa *)

                          (* for each arg, push onto stack *)
                          fun arg (b::bt) (a::at) count =
                              Mov (Rdst 1, Sco ` (count + lcs +
                                                  getloc a)) <::
                              prepare b 0 1 (ssize + lcs + count)
                              (Push ` Rco 0 <::
                               arg bt at (count + 1))
                            | arg nil nil count =
                              Callpop (name, count, ssize + lcs) <::
                              Sdelete count <::
                              (* have to restore regs before
                                 alloc, since malloc needs something
                                 to save *)
                              insns (Pop o Rdst) callersave
                              ((case cod of
                                    (* XXX we usually use Int 0 for unit *)
                                   NONE => (fn c => Alloct (Rdst 1, nil) <:: c)
                                 | SOME b => create b 1 0 ssize)
                                    (Mov (Sdst ` getloc ` C.Var v, Rco 1) <::
                                     (* restore regs *)
                                     ci rest))
                            | arg _ _ _ = 
                              raise ToRTL 
                                  "call primop with wrong number of args"
                      in
                          (* save regs. we only use regs as scratch
                             now so this is dumb. But TAL wipes
                             all these registers from the register
                             file when we make a call, and sometimes
                             we assume that registers exist (like
                             when pushing them), so this is
                             necessary until I figure out a better
                             way ... 

                             note, r0 and r1 are always trashed
                             *)

                          insns (Push o Rco) (rev callersave) `

                          (* create vectors, if necessary *)

                          (* push in reverse order; it's a stack!  *)
                          arg (rev dom) (rev args) 0
                      end

                | NONE => 
                      let 
                          val pos = Primop.tostring po
                      in
                          print ("(XXX RTL) Warning: unimplemented primop " ^
                                 pos ^ "\n");
                          (Error (pos ^ " unimplemented XXX"), nil, nil)
                      end)

        | C.Primop (po, _, _, _) => 
                   let in
                       print ("Primop is: " ^ Primop.tostring po ^ "\n");
                       raise ToRTL "(fallthrough) primop with multiple binds?"
                   end)
   (* XXX need to check int tags ! 
       - huh? am I not in putinreg? tom *)
   and getargs a b k =
       putinreg 0 a 
       (case b of
            C.Int i => k 0 (Imm i)
          | C.Var _ => 
                putinreg 1 b ` k 0 (Rco 1)
          | _ => raise ToRTL "bad rhs in binop")

   and putinreg r (v as C.Var _) rest = 
       if r = 4 
       then raise ToRTL "bug: putinreg needs r <> 4"
       else
           Mov(Rdst 4, Sco ` getloc v) <::
           Checkt (INT, Rdst r, Rco 4) <::
           rest
     | putinreg r (C.Int i) rest = Mov(Rdst r, Imm i) <:: rest
     | putinreg _ _ _ = raise ToRTL "putinreg: must be int or variable"

  in
      ci cexp
  end

end
