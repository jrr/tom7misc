structure Tactics =
struct

  exception Tactics of string
  structure M = Machine
  datatype slot = datatype M.slot
  type mach = M.mach

  infixr 9 `
  fun a ` b = a b

  open X86
  infix <- <~

  val PRINT_LOW : Word8.word = 0wx20
  val PRINT_HIGH : Word8.word =  0wx7e

  fun dprint (f : unit -> string) = ()
  (* fun dprint f = print (f ()) *)

  fun ftos f = Real.fmt (StringCvt.FIX (SOME 2)) f

  fun w16tow8s (w : Word16.word) : Word8.word * Word8.word =
    let
      val wh = Word8.fromInt ` Word16.toInt ` Word16.andb(Word16.>>(w, 0w8),
                                                          Word16.fromInt 0xFF)
      val wl = Word8.fromInt ` Word16.toInt ` Word16.andb(w,
                                                          Word16.fromInt 0xFF)
    in
      (wh, wl)
    end

  (* Find printable C such that AND (AL, C) = VL; of course not always possible. *)
  fun inverse_and (al, vl) =
    let
      (* PERF -- don't need to search all chars. *)
      fun r 0wx7f = NONE
        | r c = if Word8.andb(al, c) = vl
                then SOME c
                else r (Word8.+(c, 0w1))
    in
      r 0wx20
    end

  structure Acc :>
  sig
    type acc
    (* Create accumulator from machine state and
       empty instruction queue *)
    val empty : Machine.mach -> acc
    (* TODO: Execute the instruction, updating the machine state. *)
    (* val exec : acc * ins -> acc *)
    (* Append instruction. Doesn't interpret it. *)
    val // : acc * ins -> acc
    (* Get instructions in forward order *)
    val insns : acc -> ins list
    (* Clear instructions *)
    val clear : acc -> acc
    (* Apply transformation to machine. *)
    val ?? : acc * (mach -> mach) -> acc
    val mach : acc -> mach
  end =
  struct
  (* instruction list is in reverse order *)
    type acc = mach * ins list
    fun empty m : acc = (m, nil)
    fun // ((m, l), i) = (m, i :: l)
    fun insns (m, l) = rev l
    fun clear (m, _) = (m, nil)
    fun ?? ((m, l), mf) = (mf m, l)
    fun mach (m, _) = m
  end

  (* Maybe these should just have the following types in Machine. *)
  fun forget_reg32 r m = M.forget_reg32 m r
  fun forget_reg16 r m = M.forget_reg32 m r
  fun forget_slot r s m = M.forget_slot m r s

  fun learn_reg32 r w m = M.learn_reg32 m r w
  fun learn_reg16 r w m = M.learn_reg16 m r w
  fun learn_slot r s w m = M.learn_slot m r s w

  open Acc
  infix // ??

  (* Load an arbitrary value into AX.
     If the requested value in AH is NONE ("don't care"), the existing value is preserved.
     Can trash flags. Only AX is modified.

     PERF! Should do some kind of search with dynamic programming, etc.
     There are still lots of really bad sequences in here (36 bytes to
     go from 0 to 80!!) *)
  (* Note that loading EAX (or any register) can be easily accomplished by pushing two
     16-bit literals with this routine, then POP with the operand size prefix. *)
  (* PERF: IMUL on an immediate is useful here too. *)
  local
    (* May not change AH, since we use this in 16-bit loads. We could be more aggressive
       (IMUL, etc.) if we did't care about preserving AH. *)
    fun load_al_known acc al vl : acc =
      let
        val () = dprint (fn () =>
                         "load_al_known have: " ^ Word8.toString al ^
                         " v: " ^ Word8.toString vl ^ " with machine:\n" ^
                         M.debugstring (mach acc) ^ "\n")
        val () =
          case M.slot (mach acc) M.EAX ---@ of
            NONE => raise Tactics "must really know al in load_al_known"
          | SOME al' => if al <> al'
                        then raise Tactics "wrong known al in load_al_known?"
                        else ()
      in
        if al = vl
        (* We already have the value we wanted! *)
        then acc
        else
          (* Best is if we can emit a single byte instruction INC or
             DEC to transform it. We don't DEC 0 nor INC FF, because
             these also change AH. Note that this issue does not
             apply to SUB, because it is an 8-bit operation. *)
          if al <> 0wxFF andalso vl = Word8.+ (al, 0w1)
          then acc // INC AX ?? learn_slot M.EAX ---@ vl
          else
            if al <> 0wx00 andalso vl = Word8.- (al, 0w1)
            then acc // DEC AX ?? learn_slot M.EAX ---@ vl
            else
              (* If we know AL, we can often get the result we want
                 immediately by XOR, SUB, or AND with an immediate. *)
              let val x = Word8.xorb (al, vl)
              in
                if x >= PRINT_LOW andalso x <= PRINT_HIGH
                then acc // (XOR_A_IMM ` I8 x) ?? learn_slot M.EAX ---@ vl
                else
                  let
                    (* we want VL = AL - s,
                           s + VL = AL
                           s = AL - VL *)
                    val s = Word8.- (al, vl)
                  in
                    if s >= PRINT_LOW andalso s <= PRINT_HIGH
                    then acc // (SUB_A_IMM ` I8 s) ?? learn_slot M.EAX ---@ vl
                    else
                      (* AND is occasionally a good way, for example, if
                         AL already contains 0xFF and VL is printable. *)
                      case inverse_and (al, vl) of
                        SOME c => acc // (AND_A_IMM ` I8 c) ?? learn_slot M.EAX ---@ vl
                      | NONE =>
                          let
                            (* XXX PERF be smarter here. This does terminate for
                               all pairs, but takes as many as 36 bytes (maybe more),
                               like for 0xFF -> 0x80. *)
                            val s1 = 0wx7e
                            val new_al = Word8.- (al, s1)
                            val acc = acc // (SUB_A_IMM ` I8 s1) ?? learn_slot M.EAX ---@ new_al
                          in
                            (* PERF if we do know AH, recursing to the general routine
                               could occasionally produce better paths (e.g. 16-bit DEC) *)
                            load_al_known acc new_al vl
                          end
                  end
              end
      end

    (* Like above, but allowed to trash AL; really just here for two-part 16-bit loads. *)
    fun load_ah_known acc (ah, al) vh : acc =
      if ah = vh
      then acc
      else
      (* Loading something into AH. One reasonably brief way to do this is to misalign the stack.
         Load the desired value into AL, so that we have AH=ah?, AL=vh.
         PUSH AX, giving
                  AL AH
                   |  |
                   v  v
            SP -> vh ah? ?? ?? ?? ...
         DEC SP, giving
            SP -> ?? vh ah? ?? ?? ?? ...
         POP AX, giving
            SP -> ?? vh ah? ?? ?? ?? ...
                   |  |
                   v  v
                  AL AH
         INC SP (keep stack aligned; avoid unbounded growth) *)
        let
          val () = dprint (fn () =>
                           "load_ah_known have: " ^ Word8.toString ah ^ ", " ^ Word8.toString al ^
                           " v: " ^ Word8.toString vh ^ " with machine:\n" ^
                           M.debugstring (mach acc) ^ "\n")
          val acc = load_al_known acc al vh
        in
          acc //
          PUSH AX //
          DEC SP //
          POP AX //
          INC SP ??
          (* We don't know what's after the stack, so knowledge of AL is lost.
             ESP stays the same. *)
          forget_slot M.EAX ---@ ??
          learn_slot M.EAX --@- vh
        end

    fun load_al acc vl : acc =
      case M.slot (mach acc) M.EAX ---@ of
        SOME al => load_al_known acc al vl
      | NONE =>
          let
            val acc = acc //
              (AND_A_IMM ` I8 0wx40) //
              (AND_A_IMM ` I8 0wx20) ??
              learn_slot M.EAX ---@ 0w0
          in
            load_al_known acc 0w0 vl
          end

    (* Fully general load of 16-bit literal; fallback case. *)
    fun load_general acc (ah, al) (vh, vl) : acc =
      let
        val acc = load_ah_known acc (ah, al) vh
        val acc = load_al acc vl
      in
        acc
      end

    (* Load AX, knowing that it currently contains some value. *)
    fun load_ax16_known (acc : acc) (a : Word16.word) (v : Word16.word) : acc =
      let
        val (ah, al) = w16tow8s a
        val (vh, vl) = w16tow8s v

        fun fallback () = load_general acc (ah, al) (vh, vl)

        val inc_distance = Word16.toInt ` Word16.-(v, a)
        val dec_distance = Word16.toInt ` Word16.-(a, v)

        val _ = (inc_distance >= 0 andalso dec_distance >= 0)
          orelse raise Tactics "impossible!"

        (* PERF higher is probabably better here, but this is conservative
           since stuff like XOR_A_IMM I16 is 3 bytes. Instead, should just
           try a few things and compare. *)
        val MAX_DISTANCE = 3

        (* PERF: This should consider multi-instruction sequences. *)

        (* PERF rarely, we could find I8 versions of these that also work.
           we do search some of those in the general case if AH already
           contains VH, but for example we still might eagerly perform a
           16-bit AND here when an 8-bit one would do. *)
        fun binops () =
          let
            val xh = Word8.xorb (ah, vh)
            val xl = Word8.xorb (al, vl)
          in
            if xh >= PRINT_LOW andalso xh <= PRINT_HIGH andalso
               xl >= PRINT_LOW andalso xl <= PRINT_HIGH
            then
              let
                val xx = Word16.fromInt (Word8.toInt xh * 256 + Word8.toInt xl)
              in
                acc // (XOR_A_IMM ` I16 xx) ?? learn_reg16 M.EAX v
              end
            else
              let
                val s = Word16.-(a, v)
                val (sh, sl) = w16tow8s s
              in
                if sh >= PRINT_LOW andalso sh <= PRINT_HIGH andalso
                   sl >= PRINT_LOW andalso sl <= PRINT_HIGH
                then acc // (SUB_A_IMM ` I16 s) ?? learn_reg16 M.EAX v
                else
                  case inverse_and (ah, vh) of
                    NONE => fallback ()
                  | SOME ch =>
                    (case inverse_and (al, vl) of
                       NONE => fallback ()
                     | SOME cl =>
                         acc // (AND_A_IMM ` I16 `
                                 Word16.fromInt (Word8.toInt ch * 256 + Word8.toInt cl)) ??
                         learn_reg16 M.EAX v)
              end
          end

        fun rep 0 acc i = acc
          | rep n acc i = rep (n - 1) (acc // i) i
      in
        (* inc/dec_distance 0 covers the case that they are equal. *)
        if inc_distance <= MAX_DISTANCE
        then rep inc_distance acc (INC AX) ?? learn_reg16 M.EAX v
        else if dec_distance <= MAX_DISTANCE
             then rep dec_distance acc (DEC AX) ?? learn_reg16 M.EAX v
             else binops ()
      end
  in
    fun load_ax16 acc (w : Word16.word) : acc =
      case M.reg16 (mach acc) M.EAX of
        SOME ax => load_ax16_known acc ax w
      | NONE =>
          (* PERF: If we end up going the load_general route, then zeroing
             this is kinda pointless, because we still have to zero AL after
             the POP reads some unknown byte from the stack. Could compare the
             two approaches. *)
          (* PERF: Case where we know one of AH or AL, just not the other one *)
          (* Put *some* known value in AX so that we can use the routine
             above. PERF: Obviously some choices are better than others, and it
             depends on the value we're trying to load. For now, zero. *)
          let
            val acc = acc //
              (AND_A_IMM ` I16 ` Word16.fromInt 0x4040) //
              (AND_A_IMM ` I16 ` Word16.fromInt 0x2020) ??
              learn_reg16 M.EAX (Word16.fromInt 0)
          in
            load_ax16_known acc (Word16.fromInt 0) w
          end

    (* XXX: 8-bit versions *)
  end

  (* XXX maybe in X86.sml? *)
  fun reg_to_multireg16 r =
    case r of
      A => AX
    | C => CX
    | D => DX
    | B => BX
    | AH_SP => SP
    | CH_BP => BP
    | DH_SI => SI
    | BH_DI => DI

  fun reg_to_machreg r =
    case r of
      A => M.EAX
    | C => M.ECX
    | D => M.EDX
    | B => M.EBX
    | AH_SP => M.ESP
    | CH_BP => M.EBP
    | DH_SI => M.ESI
    | BH_DI => M.EDI

  (* Load a 16-bit register with the specified value.
     May use stack but restores it.
     Can trash flags.
     Modifies AX. *)
  fun load_reg16 (acc : acc) (A : reg) (v : Word16.word) : acc = load_ax16 acc v
    | load_reg16 acc r v =
    let
      fun general () =
        load_ax16 acc v //
        PUSH AX //
        POP (reg_to_multireg16 r) ??
        learn_reg16 (reg_to_machreg r) v
    in
      (* PERF see if we have the value in any reg, then PUSH/POP *)
      (* PERF use temporaries *)
      case M.reg16 (mach acc) ` reg_to_machreg r of
        (* just in case we already have it... *)
        SOME oldv => if v = oldv then acc else general ()
      | _ => general ()
    end

  fun check_printable_modrm modrm =
    let
      val w =
        case modrm of
          IND_EAX_DISP8 w => w
        | IND_ECX_DISP8 w => w
        | IND_EDX_DISP8 w => w
        | IND_EBX_DISP8 w => w
        | IND_SIB_DISP8 _ => raise Tactics "unimplemented"
        | IND_EPB_DISP8 w => w
        | IND_ESI_DISP8 w => w
        | IND_EDI_DISP8 w => w
        | _ => raise Tactics "mov16ind8 only works with [REG]+disp8 addressing mode."
    in
      if w >= 0wx20 andalso w <= 0wx7e then ()
      else raise Tactics "disp8 is non-printable in move16ind8"
    end

  (* As if the MOV instruction with [REG]+disp8 addressing.
     We first do AND with the destination to make it zero.
     We then XOR the destination with the source.

     Uses the stack temporarily and trashes flags, but nothing else
     is modified. *)
  fun move16ind8 acc (modrm <~ reg) : acc =
    let
      val () = check_printable_modrm modrm
      (* PERF if AX is already 0, or we don't need to keep it, then can avoid push/pop *)
      val acc = acc // PUSH AX
      val acc = load_ax16 acc (Word16.fromInt 0)
    in
      (* XXX this should track the value of the temporary too. *)
      acc //
      AND (S16, modrm <~ A) //
      POP AX //
      XOR (S16, modrm <~ reg)
    end
  (*
   | move16ind8 (reg <- modrm) =
    let
      val () = check_printable_modrm modrm
      val (known_ax, ins0) = load_reg16 ((NONE, NONE), (NONE, NONE)) (Word16.fromInt 0)
    in
      (* PERF if AX is already 0, or we don't need to keep it, then can avoid push/pop *)
      PUSH AX ::
      ins0 @
      [AND (S16, modrm <~ A),
       POP AX,
       XOR (S16, modrm <~ reg)]
    end
*)

  (* Binary NOT of the AX register. Trashes BP, flags *)
  fun not_ax16 acc : acc =
  (* Strategy here is to do AX <- XOR(AX, OxFFFF).
     Generating FFFF is pretty cheap (0 - 1). We have to put it in a temporary
     in order to do it; we'll use [EBX]+0x20. *)
    let
      val acc = acc // PUSH AX
      val acc = load_reg16 acc CH_BP (Word16.fromInt 0xFFFF)
      (* Forget EAX due to POP. It will have whatever value we started with,
         but we probably don't know it or why would we be computing NOT? *)
      val acc = acc // POP AX ?? forget_reg16 M.EAX
      val acc = move16ind8 acc (IND_EBX_DISP8 0wx20 <~ CH_BP)
      val acc = acc //
        XOR (S16, A <- IND_EBX_DISP8 0wx20) ??
        forget_reg16 M.EAX
    in
      acc
    end

  (* This sets up the initial invariants.
     - EBX = 0x00000100. We don't have any register-to-register operations, but we can do
       some indirect addressing, for example
          MOV ESI <- [EBX]
       or
          MOV ESI <- [EBX]+'a'     (of course note we do not have MOV instruction)
       EBX is a fairly useless register, since it can't be the REG argument except
       when using [REG]+disp32 as R/M, and all registers can be used that way. By
       keeping it a constant value, we can use the following slots as temporaries:
          [EBX], i.e. DS:00000100 (available registers are ESP, EBP, ESI, EDI)
          [EBX]+0x20, i.e. DS:00000120 (all registers available)
          ...
          [EBX]+0x7e, i.e. DS:0000017e (all registers available)

       0 would be an obvious choice for the constant value, but DS:0000 contains 256
       bytes of the PSP, which we want to keep:
          - it contains the command line and maybe some other useful stuff from the
            program's environment.
          - it's guaranteed to contain INT 21; RETF (at DS:0050), which we could use
            to do system calls without self-modifying code if we could get the right
            stuff on the stack and somehow get control here (it's in range of a JNO 0x7e
            at the end of the address space, so this seems possible.)
       The base value basically doesn't matter. I think everything here works fine with
       different values TEMP_START (up to 0xFFFF - 0x7E). We may need to preserve other
       parts of the data segment, though, such as the region right before the address space
       wraps.

       This is lots of temporaries, so that's nice. It's also critical because it's
       the only way we can do math ops like XOR on non-immediate values. Example:
          Get DS:0100 to contain V2
          PUSH AX
          POP BP
          XOR BP <- [EBX]
          PUSH BP
          POP AX
       or
          Get DS:0120 to contain V2
          XOR AX <- [EBX]+0x20

       Note that we also have disp32 available, but this is basically useless in
       real mode because all printable values will overflow the segment.
       *)
  val TEMP_START = Word16.fromInt 0x0100
  fun initialize () : acc =
    let
      val acc = empty M.all_unknown
      val acc = load_ax16 acc (Word16.fromInt 0)
      val acc = acc // PUSH AX
      val acc = load_ax16 acc TEMP_START
      val acc = acc // PUSH AX // POP EBX ??
        learn_reg32 M.EBX (Word32.fromInt ` Word16.toInt TEMP_START)
    (* TODO: Other stuff? *)
    in
      acc
    end

  (* Generate code that prints the string. Uses the interrupt instruction, so non-ASCII. *)
  fun printstring acc s : acc =
    let
      fun emit acc nil = acc
        | emit acc (c :: rest) =
        let
          (* Load AH=06, AL=char *)
          val value = Word16.fromInt (0x06 * 256 + ord c)
          val acc = load_ax16 acc value
          (* PERF we actually only need to set DL *)
          val acc = acc // PUSH AX // POP DX ??
            learn_reg16 M.EDX value
          val acc = acc // INT 0wx21 ??
          (* interrupt returns character written in AL.
             Not known whether it preserves DX? *)
            forget_reg16 M.EDX ??
            learn_reg16 M.EAX value
        in
          emit acc rest
        end
    in
      emit acc (explode s)
    end

  (* Exits the program. Uses the interrupt instruction, so non-ASCII. *)
  fun exit mach () : acc =
    load_ax16 mach (Word16.fromInt 0x4c00) //
    INT 0wx21

end
