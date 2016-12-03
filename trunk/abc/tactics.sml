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

  (* High, low. NONE means don't know (on input) or don't care (on output) *)
  (* XXX rewrite the next function to not need this oddity *)
  type reg16value = Word8.word option * Word8.word option

  structure Instructions :>
  sig
    type insns
    val empty : insns
    val // : insns * ins -> insns
    val /// : insns * insns -> insns
    val get : insns -> ins list
  end =
  struct
    (* in reverse *)
    type insns = ins list
    val empty : insns = nil
    fun // (l, i) = i :: l
    (* eg if we have   lfirst                      and lsecond
                       empty // PUSH AX // INC BX  empty // POP BX // DEC CX
                  =    inc :: ax :: nil            dec :: pop :: nil
       then we want    empty // PUSH AX // INC BX // POP BX // DEC CX
       which is dec :: pop :: inc :: push :: nil
       so this is just lsecond @ lfirst *)
    fun /// (lfirst, lsecond) = lsecond @ lfirst
    fun get l = rev l
  end
  open Instructions
  infix // ///

  (* Load an arbitrary value into AX.
     If the requested value in AH is NONE ("don't care"), the existing value is preserved.
     Can trash flags. Only AX is modified.

     PERF! Should do some kind of search with dynamic programming, etc.
     There are still lots of really bad sequences in here (36 bytes to
     go from 0 to 80!!) *)
  (* Note that loading EAX (or any register) can be easily accomplished by pushing two
     16-bit literals with this routine, then POP with the operand size prefix. *)
  (* PERF: IMUL on an immediate is useful here too. *)
  (* XXX probably better to separate this out into routines that do small parts,
     like one that loads ah with some known values. *)
  local
    (* May not change AH, since we use this in 16-bit loads. We could be more aggressive
       (IMUL, etc.) if we did't care about preserving AH. *)
    fun load_al_known mach al vl : mach * insns =
      let
        val () = print ("load_al_known have: " ^ Word8.toString al ^
                        " v: " ^ Word8.toString vl ^ " with machine:\n" ^
                        M.debugstring mach ^ "\n")
        val () =
          case M.slot mach M.EAX ---@ of
            NONE => raise Tactics "must really know al in load_al_known"
          | SOME al' => if al <> al'
                        then raise Tactics "wrong known al in load_al_known?"
                        else ()
      in
        if al = vl
        (* We already have the value we wanted! *)
        then (mach, empty)
        else
          (* Best is if we can emit a single byte instruction INC or
             DEC to transform it. We don't DEC 0 nor INC FF, because
             these also change AH. Note that this issue does not
             apply to SUB, because it is an 8-bit operation. *)
          if al <> 0wxFF andalso vl = Word8.+ (al, 0w1)
          then (M.learn_slot mach M.EAX ---@ vl, empty // INC AX)
          else
            if al <> 0wx00 andalso vl = Word8.- (al, 0w1)
            then (M.learn_slot mach M.EAX ---@ vl, empty // DEC AX)
            else
              (* If we know AL, we can often get the result we want
                 immediately by XOR, SUB, or AND with an immediate. *)
              let val x = Word8.xorb (al, vl)
              in
                if x >= PRINT_LOW andalso x <= PRINT_HIGH
                then (M.learn_slot mach M.EAX ---@ vl, empty // XOR_A_IMM ` I8 x)
                else
                  let
                    (* we want VL = AL - s,
                           s + VL = AL
                           s = AL - VL *)
                    val s = Word8.- (al, vl)
                  in
                    (* print ("al: " ^ Word8.toString al ^
                       " vl: " ^ Word8.toString vl ^
                       " s: " ^ Word8.toString s ^ "\n"); *)
                    if s >= PRINT_LOW andalso s <= PRINT_HIGH
                    then (M.learn_slot mach M.EAX ---@ vl, empty // SUB_A_IMM ` I8 s)
                    else
                      (* AND is occasionally a good way, for example, if
                         AL already contains 0xFF and VL is printable. *)
                      case inverse_and (al, vl) of
                        SOME c => (M.learn_slot mach M.EAX ---@ vl, empty // AND_A_IMM ` I8 c)
                      | NONE =>
                          let
                            (* XXX PERF be smarter here. This does terminate for
                               all pairs, but takes as many as 36 bytes (maybe more),
                               like for 0xFF -> 0x80. *)
                            val s1 = 0wx7e
                            val new_al = Word8.- (al, s1)
                            val mach = M.learn_slot mach M.EAX ---@ new_al
                            val insns = empty // SUB_A_IMM ` I8 s1

                            (* PERF if we do know AH, recursing to the general routine
                               could occasionally produce better paths (e.g. 16-bit DEC) *)
                            val (mach, tins) = load_al_known mach new_al vl
                          in
                            (mach, insns /// tins)
                          end
                  end
              end
      end

    (* Like above, but allowed to trash AL; really just here for two-part 16-bit loads. *)
    fun load_ah_known mach (ah, al) vh : mach * insns =
      if ah = vh
      then (mach, empty)
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
          val () = print ("load_ah_known have: " ^ Word8.toString ah ^ ", " ^ Word8.toString al ^
                          " v: " ^ Word8.toString vh ^ " with machine:\n" ^
                          M.debugstring mach ^ "\n")
          val (mach, inst_low) = load_al_known mach al vh
          (* We don't know what's after the stack, so knowledge of AL is lost.
             ESP stays the same. *)
          val mach : mach = M.forget_slot mach M.EAX ---@
          val mach : mach = M.learn_slot mach M.EAX --@- vh
        in
          (mach,
           inst_low //
           PUSH AX //
           DEC SP //
           POP AX //
           INC SP)
        end

    fun load_al mach vl : mach * insns =
      case M.slot mach M.EAX ---@ of
        SOME al => load_al_known mach al vl
      | NONE =>
          let
            val insns = empty //
              (AND_A_IMM ` I8 0wx40) //
              (AND_A_IMM ` I8 0wx20)
            val mach = M.learn_slot mach M.EAX ---@ 0w0
            val (mach, t) = load_al_known mach 0w0 vl
          in
            (mach, insns /// t)
          end

    (* Fully general load of 16-bit literal; fallback case. *)
    fun load_general mach (ah, al) (vh, vl) =
      let
        val (mach, ih) = load_ah_known mach (ah, al) vh
        val (mach, il) = load_al mach vl
      in
        (mach, ih /// il)
      end

    (* Load AX, knowing that it currently contains some value. *)
    fun load_ax16_known (mach : M.mach) (ax : Word16.word) (v : Word16.word) : mach * insns =
      let
        val (ah, al) = w16tow8s ax
        val (vh, vl) = w16tow8s v
      in
        (* PERF 16-bit tricks first! *)
        load_general mach (ah, al) (vh, vl)
      end
  in
    fun load_ax16 (mach : M.mach) (w : Word16.word) : mach * insns =
      case M.reg16 mach M.EAX of
        SOME ax => load_ax16_known mach ax w
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
            val insns = empty //
              (AND_A_IMM ` I16 ` Word16.fromInt 0x4040) //
              (AND_A_IMM ` I16 ` Word16.fromInt 0x2020)

            val mach = M.learn_reg16 mach M.EAX (Word16.fromInt 0)
            val (mach, t) = load_ax16_known mach (Word16.fromInt 0) w
          in
            (mach, insns /// t)
          end

    (* XXX: 8-bit versions *)
  end


(* some 16-bit tricks *)
(*
                    | ((SOME ah, SOME al), target as (SOME vh, SOME vl)) =>
         (* PERF: This should consider multi-instruction sequences. *)
         let
           val a = Word16.fromInt (Word8.toInt ah * 256 + Word8.toInt al)
           val v = Word16.fromInt (Word8.toInt vh * 256 + Word8.toInt vl)

           val inc_distance = Word16.toInt ` Word16.-(v, a)
           val dec_distance = Word16.toInt ` Word16.-(a, v)

           val _ = (inc_distance >= 0 andalso dec_distance >= 0)
             orelse raise Tactics "impossible!"

           (* PERF higher is probabably better here, but this is conservative
              since stuff like XOR_A_IMM I16 is 3 bytes. *)
           val MAX_DISTANCE = 3

           fun fallback () = load_all16 (SOME ah, SOME al) (vh, vl)

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
               then (target, [XOR_A_IMM ` I16 (Word16.fromInt (Word8.toInt xh * 256 +
                                                               Word8.toInt xl))])
               else
                 let
                   val s = Word16.-(a, v)
                   val (sh, sl) = w16tow8s s
                 in
                   if sh >= PRINT_LOW andalso sh <= PRINT_HIGH andalso
                      sl >= PRINT_LOW andalso sl <= PRINT_HIGH
                   then (target, [SUB_A_IMM ` I16 ` s])
                   else
                     case inverse_and (ah, vh) of
                       NONE => fallback ()
                     | SOME ch =>
                       (case inverse_and (al, vl) of
                          NONE => fallback ()
                        | SOME cl =>
                            (target, [AND_A_IMM ` I16 `
                                      Word16.fromInt (Word8.toInt ch * 256 + Word8.toInt cl)]))
                 end
             end
         in
           (* inc/dec_distance 0 covers the case that they are equal. *)
           if inc_distance <= MAX_DISTANCE
           then (target, List.tabulate (inc_distance, fn _ => INC AX))
           else if dec_distance <= MAX_DISTANCE
                then (target, List.tabulate (dec_distance, fn _ => DEC AX))
                else binops()
         end

      | ((known_ah, known_al), (SOME vh, SOME vl)) =>
         load_all16 (known_ah, known_al) (vh, vl)
    end
*)

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
  fun load_reg16 (mach : mach) (A : reg) (v : Word16.word) = load_ax16 mach v
    | load_reg16 mach r v =
    let
      fun general () =
        let
          val (mach, axins) = load_ax16 mach v
          val mach = M.learn_reg16 mach (reg_to_machreg r) v
        in
          (mach,
           axins //
           PUSH AX //
           POP (reg_to_multireg16 r))
        end
    in
      (* PERF see if we have the value in any reg, then PUSH/POP *)
      (* PERF use temporaries *)
      case M.reg16 mach ` reg_to_machreg r of
        (* just in case we already have it... *)
        SOME oldv => if v = oldv then (mach, empty) else general ()
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
  fun move16ind8 mach (modrm <~ reg) =
    let
      val () = check_printable_modrm modrm
      (* PERF if AX is already 0, or we don't need to keep it, then can avoid push/pop *)
      val insns = empty // PUSH AX
      val (mach, axins) = load_ax16 mach (Word16.fromInt 0)
      (* XXX this should track the value of the temporary *)
    in
      (mach,
       insns ///
       axins //
       AND (S16, modrm <~ A) //
       POP AX //
       XOR (S16, modrm <~ reg))
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
  fun not_ax16 mach : mach * insns =
  (* Strategy here is to do AX <- XOR(AX, OxFFFF).
     Generating FFFF is pretty cheap (0 - 1). We have to put it in a temporary
     in order to do it; we'll use [EBX]+0x20. *)
    let
      val insns = empty // PUSH AX
      val (mach, neg1ins) = load_reg16 mach CH_BP (Word16.fromInt 0xFFFF)
      val insns = insns /// neg1ins // POP AX
      val mach = M.forget_reg16 mach M.EAX
      val (mach, movins) = move16ind8 mach (IND_EBX_DISP8 0wx20 <~ CH_BP)
      val insns = insns /// movins // XOR (S16, A <- IND_EBX_DISP8 0wx20)
      val mach = M.forget_reg16 mach M.EAX
    in
      (mach, insns)
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
  fun initialize () : (mach * insns) =
    let
      val mach = M.all_unknown
      val (mach, inshigh) = load_ax16 mach (Word16.fromInt 0)
      val insns = inshigh // PUSH AX
      val (mach, inslow) = load_ax16 mach TEMP_START
      val insns = insns /// inslow // PUSH AX // POP EBX
      val mach = M.learn_reg32 mach M.EBX (Word32.fromInt ` Word16.toInt TEMP_START)
    (* TODO: Other stuff? *)
    in
      (mach, insns)
    end

  (* XXX this should return the known values *)
  (* Generate code that prints the string. Uses the interrupt instruction, so non-ASCII. *)
  fun printstring mach s : mach * insns =
    let
      fun emit mach nil insns = (mach, insns)
        | emit mach (c :: rest) insns =
        let
          (* Load AH=06, AL=char *)
          val value = Word16.fromInt (0x06 * 256 + ord c)
          val (mach, axins) = load_ax16 mach value
          (* PERF we actually only need to set DL *)
          val insns = insns /// axins // PUSH AX // POP DX
          val mach = M.learn_reg16 mach M.EDX value
          val insns = insns // INT 0wx21
          (* interrupt returns character written in AL.
             Not known whether it preserves DX? *)
          val mach = M.forget_reg16 mach M.EDX
          val mach = M.learn_reg16 mach M.EAX value

        in
          emit mach rest insns
        end
    in
      emit mach (explode s) empty
    end

  (* Exits the program. Uses the interrupt instruction, so non-ASCII.
     Doesn't return mach since it's, like, undefined *)
  fun exit mach () : insns =
    let
      val (mach, insns) = load_ax16 mach (Word16.fromInt 0x4c00)
    in
      insns // INT 0wx21
    end

end
