(* Signed comparisons *)
functor WS(W : WORD) =
struct
  fun less (x, y) = LargeInt.<(W.toLargeIntX x, W.toLargeIntX y)
  fun lesseq (x, y) = LargeInt.<=(W.toLargeIntX x, W.toLargeIntX y)
  fun greater (x, y) = LargeInt.>(W.toLargeIntX x, W.toLargeIntX y)
  fun greatereq (x, y) = LargeInt.>=(W.toLargeIntX x, W.toLargeIntX y)
end

structure OptimizeCIL :> OPTIMIZECIL =
struct
  infixr 9 `
  fun a ` b = a b

  exception OptimizeCIL of string

  open CIL

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)
  (* XXX some kind of debug mode *)
  fun eprint s = print (s ^ "\n")

  structure WS8 = WS(Word8)
  structure WS16 = WS(Word16)
  structure WS32 = WS(Word32)


  fun wordwidth Width8 = Word8 Unsigned
    | wordwidth Width16 = Word16 Unsigned
    | wordwidth Width32 = Word32 Unsigned
  fun fst (a, _) = a

  (* Must agree with tocil. *)
  val BOOL_WIDTH = Width16
  val BOOL_TYPE = Word16 Unsigned
  val LiteralTrue = Word16Literal ` Word16.fromInt 1
  val LiteralFalse = Word16Literal ` Word16.fromInt 0

  structure DeadVarsArg : CILPASSARG =
  struct
    type arg = { used: bool ref SM.map, simplified: bool ref }
    structure CI = CILIdentity(type arg = arg)
    open CI

    fun markused { used, simplified } v =
      case SM.find (used, v) of
        NONE => raise OptimizeCIL ("unbound var " ^ v)
      | SOME r => r := true

    fun case_Bind (arg as { used, simplified })
      ({ selft, selfv, selfe, selfs }, ctx) (v, t, e, s) =
      let
        val (e, te) = selfe arg ctx e
        (* XXX check t = te? *)
        val t = selft arg ctx t
        val ctx = Context.insert (ctx, v, t)
        val r = ref false
        val arg = { used = SM.insert (used, v, r), simplified = simplified }
        val s = selfs arg ctx s
      in
        if !r
        then Bind (v, t, e, s)
        else
          let in
            simplified := true;
            eprint ("Drop unused " ^ v ^ " = " ^ exptos e);
            Do (e, s)
          end
      end

    fun case_Var arg stuff s =
      let in
        markused arg s;
        CI.case_Var arg stuff s
      end
  end
  structure DeadVars = CILPass(DeadVarsArg)

  (* For each block label (key of outer map), give
     the count of uses within other blocks (label is key of inner map). *)
  type analyze_blocks_uses = int SM.map ref SM.map
  structure AnalyzeBlocksArg : CILPASSARG =
  struct
    type arg = { current : string, uses : analyze_blocks_uses ref }
    structure CI = CILIdentity(type arg = arg)
    open CI

    fun adduse { current, uses } used =
      let
        val mr =
          case SM.find (!uses, used) of
            NONE =>
              let val newmap = ref SM.empty
              in
                uses := SM.insert (!uses, used, newmap);
                newmap
              end
          | SOME r => r
      in
        case SM.find (!mr, current) of
          NONE => mr := SM.insert (!mr, current, 1)
        | SOME r => mr := SM.insert (!mr, current, r + 1)
      end

    fun case_GotoIf arg (stuff as ({ selft, selfv, selfe, selfs }, ctx))
      (cond, lab, s) =
      let in
        adduse arg lab;
        print ("use " ^ lab ^ "\n");
        CI.case_GotoIf arg stuff (cond, lab, s)
      end

    fun case_Goto arg ({ selft, selfv, selfe, selfs }, ctx) lab =
      let in
        adduse arg lab;
        Goto lab
      end
  end

  structure OptimizeBlocksArg : CILPASSARG =
  struct
    type arg = { uses : analyze_blocks_uses,
                 blocks : (string * stmt) list,
                 removeme : string list ref,
                 simplified : bool ref }
    structure CI = CILIdentity(type arg = arg)
    open CI

    fun simplify ({ simplified, ... } : arg) = simplified := true

    fun totaluses ({ uses, ... } : arg) lab =
      case SM.find (uses, lab) of
        NONE => raise OptimizeCIL ("unanalyzed label? " ^ lab)
      | SOME m => SM.foldl op+ 0 (!m)

    fun getblock ({ blocks, ... } : arg) lab =
      case ListUtil.Alist.find op= blocks lab of
        NONE => raise OptimizeCIL ("missing block " ^ lab)
      | SOME stmt => stmt

    fun markremove ({ removeme, ... } : arg) lab =
      removeme := lab :: !removeme

    (* Can't really do anything about GotoIf... *)

    fun case_Goto arg ({ selft, selfv, selfe, selfs }, ctx) lab =
      if totaluses arg lab = 1
      then
        let in
          eprint ("Inlining single-use block " ^ lab);
          simplify arg;
          markremove arg lab;
          getblock arg lab
        end
      else Goto lab
  end
  structure OptimizeBlocks =
  struct
    structure AB = CILPass(AnalyzeBlocksArg)
    structure OB = CILPass(OptimizeBlocksArg)
    (* TODO: If "body" does a goto to another label immediately,
       we can rewrite it... *)
    (* TODO: Drop blocks only used by themselves.
       (Or in general, unreachable cycles.) *)
    fun optimize simplified (Func { args, ret, body, blocks }) =
      let
        val uses : analyze_blocks_uses ref = ref SM.empty
        (* Give every block an entry, so that we later see 0-use blocks. *)
        val () = app (fn (name, _) =>
                      uses :=
                      SM.insert (!uses, name,
                                 (* But prevent anything from touching the
                                    function's entry point *)
                                 if name = body
                                 then ref ` SM.insert (SM.empty, "", 999)
                                 else ref SM.empty)) blocks

        fun analyze_one_block (name, stmt) =
          let
            val arg = { current = name, uses = uses }
          in
            ignore ` AB.converts arg CIL.Context.empty stmt
          end

        val () = app analyze_one_block blocks
        val () = SM.appi (fn (lab, u) =>
                          let in
                            print (lab ^ ":\n");
                            SM.appi (fn (user, n) =>
                                     print ("  by " ^ user ^ " x" ^
                                            Int.toString n ^ "\n")) (!u)
                          end) (!uses)

        (* We'll remove any block that has no uses, and maybe more
           because they get inlined. *)
        val removeme = ref (SM.foldli (fn (lab, m, remove) =>
                                       if SM.isempty (!m)
                                       then lab :: remove
                                       else remove) nil (!uses))

        fun optimize_one_block (name, stmt) =
          let
            val arg = { uses = !uses, blocks = blocks,
                        removeme = removeme, simplified = simplified }
          in
            (name, OB.converts arg CIL.Context.empty stmt)
          end

        val blocks = map optimize_one_block blocks
        (* Remove anything that was marked to be removed because it
           was inlined. *)
        val blocks = List.filter
          (fn (name, _) =>
           not ` List.exists (fn name' => name = name') (!removeme)) blocks
      in
        if length ` !removeme > 0
        then simplified := true
        else ();
        Func { args = args, ret = ret, body = body, blocks = blocks }
      end
  end

  fun effectless_expression e =
    case e of
      Value _ => true
    | Truncate _ => true
    | Promote _ => true
    | Plus _ => true
    | Minus _ => true
    | Times _ => true
    | LeftShift _ => true
    | RightShift _ => true
    | Greater _ => true
    | GreaterEq _ => true
    | Above _ => true
    | AboveEq _ => true
    | Less _ => true
    | LessEq _ => true
    | Below _ => true
    | BelowEq _ => true
    | Eq _ => true
    | Neq _ => true
    | And _ => true
    | Or _ => true
    | Xor _ => true
    | Not _ => true
    | Complement _ => true
    | Negate _ => true
    | Load _ => true
    (* These can trap on 0 denominator *)
    | SignedDivision _ => false
    | UnsignedDivision _ => false
    | UnsignedMod _ => false
    (* Unlimited side-effects *)
    | Call _ => false

  structure SimplifyArg : CILPASSARG =
  struct
    type arg = { known: value SM.map, simplified: bool ref }
    structure CI = CILIdentity(type arg = arg)
    open CI

    fun addknown { known, simplified } (v, va) =
      { known = SM.insert (known, v, va), simplified = simplified }
    fun markunknown { known, simplified } v =
      { known = SM.erase (known, v), simplified = simplified }

    fun case_Do (arg as { known, simplified})
                ({ selft, selfv, selfe, selfs }, ctx) (e, s) =
      let
        val (e, _) = selfe arg ctx e
        val s = selfs arg ctx s
      in
        if effectless_expression e
        then
          let in
            simplified := true;
            eprint ("Dropping effectless do " ^ exptos e ^ "\n");
            s
          end
        else Do (e, s)
      end

    fun case_Bind arg ({ selft, selfv, selfe, selfs }, ctx) (v, t, e, s) =
      let
        val (e, te) = selfe arg ctx e
        val t = selft arg ctx t
        val ctx = Context.insert (ctx, v, t)
        val arg =
          case e of
            Value value => addknown arg (v, value)
          | _ => markunknown arg v

        val s = selfs arg ctx s
      in
        Bind (v, t, e, s)
      end

    fun case_Var (arg as { known, simplified })
      ({ selft, selfv, selfe, selfs }, ctx) s =
      case Context.lookup (ctx, s) of
        NONE => raise OptimizeCIL ("unbound variable " ^ s)
      | SOME t =>
          (case SM.find (known, s) of
             NONE => (Var s, t)
           | SOME value =>
               let in
                 simplified := true;
                 (value, t)
               end)

    fun case_GotoIf (arg as { known, simplified })
                    ({ selft, selfv, selfe, selfs }, ctx) (c, lab, s) =
      let
        datatype cmp = LESS of bool | BELOW of bool | EQ | NEQ
        fun vv a = let val (a, _) = selfv arg ctx a in a end
        val (cmp, w, a, b, ctor) =
          case c of
            CLess (w, a, b) => (LESS false, w, vv a, vv b, CLess)
          | CLessEq (w, a, b) => (LESS true, w, vv a, vv b, CLessEq)
          | CBelow (w, a, b) => (BELOW false, w, vv a, vv b, CBelow)
          | CBelowEq (w, a, b) => (BELOW true, w, vv a, vv b, CBelowEq)
          | CEq (w, a, b) => (EQ, w, vv a, vv b, CEq)
          | CNeq (w, a, b) => (NEQ, w, vv a, vv b, CNeq)
        val s = selfs arg ctx s
        datatype taken = Taken | NotTaken | Unknown
        fun take true = Taken
          | take false = NotTaken
        fun check w_expected =
          if w = w_expected
          then ()
          else raise OptimizeCIL ("In GotoIf, argument values didn't " ^
                                  "agree with annotated size. Wanted " ^
                                  widthtos w_expected ^ " but saw literals " ^
                                  " of type " ^ widthtos w)
        val taken =
          case (cmp, a, b) of
            (LESS eq, Word8Literal a, Word8Literal b) =>
              let in
                check Width8;
                take ` (if eq then op <= else op <) (Word8.toIntX a,
                                                     Word8.toIntX b)
              end
          | (LESS eq, Word16Literal a, Word16Literal b) =>
              let in
                check Width16;
                take ` (if eq then op <= else op <) (Word16.toIntX a,
                                                     Word16.toIntX b)
              end
          | (LESS eq, Word32Literal a, Word32Literal b) =>
              let in
                check Width32;
                take ` (if eq then op <= else op <) (Word32.toIntX a,
                                                     Word32.toIntX b)
              end
          (* XXX BELOW, EQ, NEQ *)
          | _ => Unknown

      in
        (* XXX do something with 'taken'! *)
        GotoIf (ctor (w, a, b), lab, s)
      end

    fun case_Truncate arg ({ selft, selfv, selfe, selfs }, ctx) { src, dst, v } =
      let
        fun default v = (Truncate { src = src, dst = dst, v = v },
                         wordwidth dst)
      in
        (* Note: We could warn here when throwing away bits, which would catch
           some user errors, but also probably explicit casts like (uint8)x
           after some math... *)
        case (fst ` selfv arg ctx v, src, dst) of
          (_, Width8, _) => raise OptimizeCIL "word8 cannot be truncated"
        | (Word16Literal w16, Width16, Width8) =>
            (Value ` Word8Literal (Word8.fromInt ` Word16.toInt w16),
             Word8 Unsigned)
        | (Word32Literal w32, Width32, Width16) =>
            (Value ` Word16Literal (Word16.fromInt `
                                    Word32.toInt `
                                    Word32.andb (w32, 0wxFFFF)),
             Word16 Unsigned)
        | (Word32Literal w32, Width32, Width8) =>
            (Value ` Word8Literal (Word8.fromInt `
                                   Word32.toInt `
                                   Word32.andb (w32, 0wxFF)),
             Word8 Unsigned)
        | (v as Var _, _, _) => default v
        | _ => raise OptimizeCIL "illegal truncation?"
      end

    fun case_Promote arg ({ selft, selfv, selfe, selfs }, ctx)
                         { src, dst, v, signed } =
      let
        fun default v = (Promote { src = src, dst = dst,
                                   v = v, signed = signed },
                         wordwidth dst)
      in
        case (fst ` selfv arg ctx v, src, dst, signed) of
          (_, Width32, _, _) => raise OptimizeCIL "word8 cannot be promoted"

        | (Word16Literal w16, Width16, Width32, true) =>
            (Value ` Word32Literal ` Word32.fromInt ` Word16.toIntX w16,
             Word32 Unsigned)
        | (Word16Literal w16, Width16, Width32, false) =>
            (Value ` Word32Literal ` Word32.fromInt ` Word16.toInt w16,
             Word32 Unsigned)

        | (Word8Literal w8, Width8, Width32, true) =>
            (Value ` Word32Literal ` Word32.fromInt ` Word8.toIntX w8,
             Word32 Unsigned)
        | (Word8Literal w8, Width8, Width32, false) =>
            (Value ` Word32Literal ` Word32.fromInt ` Word8.toInt w8,
             Word32 Unsigned)

        | (Word8Literal w8, Width8, Width16, true) =>
            (Value ` Word16Literal ` Word16.fromInt ` Word8.toIntX w8,
             Word16 Unsigned)
        | (Word8Literal w8, Width8, Width16, false) =>
            (Value ` Word16Literal ` Word16.fromInt ` Word8.toInt w8,
             Word16 Unsigned)

        | (v as Var _, _, _, _) => default v
        | _ => raise OptimizeCIL "illegal promotion?"
      end

    fun comparison ctor { a8, a16, a32 }
      (arg as { known, simplified })
      ({ selft, selfv, selfe, selfs } : selves, ctx) (w, a, b) =
      let
        val (a, _) = selfv arg ctx a
        val (b, _) = selfv arg ctx b

        datatype kc =
          Unknown
        | K8 of Word8.word
        | K16 of Word16.word
        | K32 of Word32.word

        fun kcvalue (Var s) =
          (* XXX should have already been handled.. *)
          Unknown
          | kcvalue (Word8Literal w) = K8 w
          | kcvalue (Word16Literal w) = K16 w
          | kcvalue (Word32Literal w) = K32 w
          | kcvalue _ = Unknown
      in
        (* PERF some partially-known comparisons are always true or false,
           like x <= 0xFF *)
        case (kcvalue a, kcvalue b) of
          (Unknown, _) => (ctor (w, a, b), BOOL_TYPE)
        | (_, Unknown) => (ctor (w, a, b), BOOL_TYPE)
        | (K8 wa, K8 wb) =>
            let in
              simplified := true;
              eprint "known 8-bit comparison\n";
              (Value (if a8 (wa, wb) then LiteralTrue else LiteralFalse),
               BOOL_TYPE)
            end
        | (K16 wa, K16 wb) =>
            let in
              simplified := true;
              eprint "known 8-bit comparison\n";
              (Value (if a16 (wa, wb) then LiteralTrue else LiteralFalse),
               BOOL_TYPE)
            end
        | (K32 wa, K32 wb) =>
            let in
              simplified := true;
              eprint "known 8-bit comparison\n";
              (Value (if a32 (wa, wb) then LiteralTrue else LiteralFalse),
               BOOL_TYPE)
            end
        | _ => raise OptimizeCIL "argument width mismatch to comparison"
      end

    val case_Above = comparison Above
      { a8 = Word8.>, a16 = Word16.>, a32 = Word32.> }
    val case_AboveEq = comparison AboveEq
      { a8 = Word8.>=, a16 = Word16.>=, a32 = Word32.>= }
    val case_Below = comparison Below
      { a8 = Word8.<, a16 = Word16.<, a32 = Word32.< }
    val case_BelowEq = comparison BelowEq
      { a8 = Word8.<=, a16 = Word16.<=, a32 = Word32.<= }
    val case_Eq = comparison Eq
      { a8 = op =, a16 = op =, a32 = op = }
    val case_Neq = comparison Neq
      { a8 = op <>, a16 = op <>, a32 = op <> }
    val case_Greater = comparison Greater
      { a8 = WS8.greater, a16 = WS16.greater, a32 = WS32.greater }
    val case_GreaterEq = comparison GreaterEq
      { a8 = WS8.greatereq, a16 = WS16.greatereq, a32 = WS32.greatereq }
    val case_Less = comparison Less
      { a8 = WS8.less, a16 = WS16.less, a32 = WS32.less }
    val case_LessEq = comparison LessEq
      { a8 = WS8.lesseq, a16 = WS16.lesseq, a32 = WS32.lesseq }
  end
  structure Simplify = CILPass(SimplifyArg)

  (* Optimize a basic block without any contextual information.
     Blocks are closed code that manifest effects through loads
     and stores to local/global addresses. *)
  fun optimize_block simplified (block : stmt) : stmt =
    let
      val ctx = CIL.Context.empty
      val block = Simplify.converts
        ({ known = SM.empty, simplified = simplified }) ctx block

      val block = DeadVars.converts
        ({ used = SM.empty, simplified = simplified }) ctx block
      (* ... more optimizations here ... *)
    in
      block
    end

  fun optimize_function simplified (name, func) =
    let
      val Func { args, ret, body, blocks } =
        OptimizeBlocks.optimize simplified func
    in
      (name,
       Func { args = args, ret = ret, body = body,
              blocks = ListUtil.mapsecond (optimize_block simplified) blocks })
    end

  fun optimize (Program { functions, globals }) =
    let
      val () = print "\n----------- optimization round -------------\n"
      val simplified = ref false
      val functions = map (optimize_function simplified) functions

      val prog = Program { functions = functions, globals = globals }
    in
      print ("\nNow:\n" ^ CIL.progtos prog ^ "\n");

      if !simplified
      then optimize prog
      else prog
    end

end

(* TODO:
   - Often the value of a LOAD will already be known (like it's
     in a variable) -- use it!
   - Drop unused loads and stores!
   - If a basic block just does an unconditional GOTO to
     another block, rewrite calls to it.
   - If we have !cond, see if we can reverse the definition of
     cond (e.g. a < b becomes a >= b).
   - If we have AND32(x, 1), there's no need to first convert
     x to 32 bits. Just do AND16.
   - No need to promote argument before GotoIf on it (truncate could
     drop one bits though).
     *)
