structure OptimizeCIL :> OPTIMIZECIL =
struct
  infixr 9 `
  fun a ` b = a b

  exception OptimizeCIL of string

  open CIL

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  fun wordwidth Width8 = Word8 Unsigned
    | wordwidth Width16 = Word16 Unsigned
    | wordwidth Width32 = Word32 Unsigned
  fun fst (a, _) = a

  structure SimplifyArg : CILPASSARG =
  struct
    type arg = { known: value SM.map, simplified: bool ref }
    structure CI = CILIdentity(type arg = arg)
    open CI

    fun addknown ({ known, simplified }) (v, va) =
      { known = SM.insert (known, v, va), simplified = simplified }
    fun markunknown ({ known, simplified }) v =
      { known = SM.erase (known, v), simplified = simplified }

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
  end
  structure Simplify = CILPass(SimplifyArg)

  (* TODO: Dead code removal! *)

  (* Optimize a basic block without any contextual information.
     Blocks are closed code that manifest effects through loads
     and stores to local/global addresses. *)
  fun optimize_block (block : stmt) : stmt =
    let
      val simplified = ref false
      val ctx = CIL.Context.empty
      val block = Simplify.converts
        ({ known = SM.empty, simplified = simplified }) ctx block
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
         Func { args = args, ret = ret, body = optimize_block body,
                blocks = ListUtil.mapsecond optimize_block blocks })
    in
      Program { functions = map one_function functions,
                globals = globals }
    end

end
