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
      ({ selft, selfv, selfe, selfs }, ctx) (v, e, s) =
      let
        val (e, t) = selfe arg ctx e
        val ctx = Context.insert (ctx, v, t)
        val r = ref false
        val arg = { used = SM.insert (used, v, r), simplified = simplified }
        val s = selfs arg ctx s
      in
        if !r
        then Bind (v, e, s)
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

  structure AnalyzeBlocksArg : CILPASSARG =
  struct
    (* For each block label (key of outer map), give
       the count of uses within other blocks (label is key of inner map). *)
    type arg = { current: string, uses: int SM.map ref SM.map }
    structure CI = CILIdentity(type arg = arg)
    open CI

    fun adduse { current, uses } used user =
      let
        val mr =
          case SM.find (uses, used) of
            NONE =>
              let val newmap = ref SM.empty
              in
                SM.insert (uses, used, newmap);
                newmap
              end
          | SOME r => r
      in
        case SM.find (!mr, used) of
          NONE => mr := SM.insert (!mr, used, 1)
        | SOME r => mr := SM.insert (!mr, used, r + 1)
      end

    (* XXX finish me... *)
    (*
    fun case_GotoIf arg ({ selft, selfv, selfe, selfs }, ctx) (v, lab, s) =
      let
        val (v, _) = selfv arg ctx v
        val s = selfs arg ctx s
      in
        GotoIf (v, lab, s)
      end
    fun case_Goto arg ({ selft, selfv, selfe, selfs }, ctx) = Goto
    *)
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

    fun case_Bind arg ({ selft, selfv, selfe, selfs }, ctx) (v, e, s) =
      let
        val (e, t) = selfe arg ctx e
        val ctx = Context.insert (ctx, v, t)
        val arg =
          case e of
            Value value => addknown arg (v, value)
          | _ => markunknown arg v

        val s = selfs arg ctx s
      in
        Bind (v, e, s)
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
                    ({ selft, selfv, selfe, selfs }, ctx) (v, lab, s) =
      let
        datatype kb =
            Unknown
          | Zero
          | Nonzero
        fun kbvalue (Var s) =
          (case SM.find (known, s) of
             NONE => Unknown
           | SOME v => kbvalue v)
          | kbvalue (Word8Literal w) =
             if w = Word8.fromInt 0 then Zero else Nonzero
          | kbvalue (Word16Literal w) =
             if w = Word16.fromInt 0 then Zero else Nonzero
          | kbvalue (Word32Literal w) =
             if w = Word32.fromInt 0 then Zero else Nonzero
          (* PERF I guess this could be a string/address literal, in
             which case it is nonzero, yeah? *)
          | kbvalue _ = Unknown

        val (v, _) = selfv arg ctx v
      in
        case kbvalue v of
          Unknown => GotoIf (v, lab, selfs arg ctx s)
        | Zero =>
            let in
              eprint ("dropping always-false gotoif " ^ lab);
              simplified := true;
              selfs arg ctx s
            end
        | Nonzero =>
            let in
              eprint ("gotoif " ^ lab ^ " is always taken");
              simplified := true;
              Goto lab
            end
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

    (* TODO: Promote *)
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

    (* XXX rest of comparison operators *)
    val case_Greater = comparison Greater
      { a8 = Word8.>, a16 = Word16.>, a32 = Word32.> }
    val case_GreaterEq = comparison GreaterEq
      { a8 = Word8.>=, a16 = Word16.>=, a32 = Word32.>= }
    val case_Less = comparison Less
      { a8 = Word8.<, a16 = Word16.<, a32 = Word32.< }
    val case_LessEq = comparison LessEq
      { a8 = Word8.<=, a16 = Word16.<=, a32 = Word32.<= }
  end
  structure Simplify = CILPass(SimplifyArg)

  (* Optimize a basic block without any contextual information.
     Blocks are closed code that manifest effects through loads
     and stores to local/global addresses. *)
  fun optimize_block (block : stmt) : stmt =
    let
      val simplified = ref false
      val ctx = CIL.Context.empty
      val block = Simplify.converts
        ({ known = SM.empty, simplified = simplified }) ctx block

      val block = DeadVars.converts
        ({ used = SM.empty, simplified = simplified }) ctx block
      (* ... more optimizations here ... *)
    in
      (* Keep going if we had any simplifications. *)
      if !simplified
      then optimize_block block
      else block
    end

  fun optimize (Program { functions, globals }) =
    let
      fun one_function (name, Func { args, ret, body, blocks }) =
        (name,
         Func { args = args, ret = ret, body = body,
                blocks = ListUtil.mapsecond optimize_block blocks })
    in
      Program { functions = map one_function functions,
                globals = globals }
    end

end

(* TODO:
   - Drop unreachable blocks!
   - Drop unused loads and stores!
   - If a basic block just does an unconditional GOTO to
     another block, rewrite calls to it.
   - If we have !cond, see if we can reverse the definition of
     cond (e.g. a < b becomes a >= b).
 *)