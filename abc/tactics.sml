structure Tactics =
struct

  exception Tactics of string
  structure M = Machine

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
    val get : insns -> ins list
  end =
  struct
    (* in reverse *)
    type insns = ins list
    val empty : insns = nil
    fun // (l, i) = i :: l
    fun get l = rev l
  end

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
  fun load_ax16 (ax : reg16value) (v : reg16value) : (reg16value * ins list) =
    let
      (* Fully general load of 16-bit literal; fallback case. *)
      fun load_all16 (known_ah, known_al) (vh, vl) =
        (* If AH already has what we need, we can skip a lot. *)
        if known_ah = SOME vh
        then load_ax16 (known_ah, known_al) (NONE, SOME vl)
        else
          let
            val (known, load_high) = load_ax16 (known_ah, known_al) (SOME vh, NONE)
            val (known, load_low) = load_ax16 known (NONE, SOME vl)
          in
            (known, load_high @ load_low)
          end
    in
      case (ax, v) of
        (* Somehow we don't care what the value is at all *)
        (known, (NONE, NONE)) => (known, nil)
      | (known as (known_ah, SOME al), (NONE, SOME vl)) =>
          (* Loading AL only. *)
          if al = vl
          (* We already have the value we wanted! *)
          then ((known_ah, SOME vl), nil)
          else
            (* Best is if we can emit a single byte instruction INC or
               DEC to transform it. We don't DEC 0 nor INC FF, because
               these also change AH. Note that this issue does not
               apply to SUB, because it is an 8-bit operation.
               PERF: If we really don't care about the value of AH, or
               just want to propagate what we know about it, we could
               use 16-bit inc/dec here... *)
            if al <> 0wxFF andalso vl = Word8.+ (al, 0w1)
            then ((known_ah, SOME vl), [INC AX])
            else if al <> 0wx00 andalso vl = Word8.- (al, 0w1)
                 then ((known_ah, SOME vl), [DEC AX])
                 else
            (* If we know AL, we can often get the result we want
               immediately by XOR, SUB, or AND with an immediate. *)
            let val x = Word8.xorb (al, vl)
            in
              if x >= PRINT_LOW andalso x <= PRINT_HIGH
              then ((known_ah, SOME vl), [XOR_A_IMM ` I8 x])
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
                  then ((known_ah, SOME vl), [SUB_A_IMM ` I8 s])
                  else
                    (* AND is occasionally a good way, for example, if
                       AL already contains 0xFF and VL is printable. *)
                    case inverse_and (al, vl) of
                      SOME c => ((known_ah, SOME vl), [AND_A_IMM ` I8 c])
                    | NONE =>
                        let
                          (* XXX PERF be smarter here. This does terminate for
                             all pairs, but takes as many as 36 bytes (maybe more),
                             like for 0xFF -> 0x80. *)
                          val s1 = 0wx7e
                          val (kt, t) =
                            load_ax16 (known_ah, SOME (Word8.- (al, s1))) (NONE, SOME vl)
                        in
                          (kt, SUB_A_IMM ` I8 s1 :: t)
                        end
                end
            end
      | ((known_ah, NONE), value as (NONE, SOME vl)) =>
        (* Put *some* known value in AL so that we can do the routine above.
           PERF: Of course, some choices are better than others here! If
           we had a precomputed table of src*dst pairs, we could try to
           compute a good one. *)
            let val (kt, t) = load_ax16 (known_ah, SOME 0w0) value
            in
              (kt,
               AND_A_IMM ` I8 0wx40 ::   (* ASCII '@', dec 64 *)
               (* AL either contains 0x40 or 0x00. *)
               AND_A_IMM ` I8 0wx20 ::   (* ASCII ' ', dec 32 *)
               (* Now AL definitely contains 00. *)
               t)
            end

      (* PERF: Loading AH when something is known. *)

      | (known, value as (SOME vh, NONE)) =>
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
           val (known, load_low) = load_ax16 known (NONE, SOME vh)
         in
           ((SOME vh, NONE),
           load_low @
            [PUSH AX,
             DEC SP,
             POP AX,
             INC SP])
         end

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

  (* Load a 16-bit register (!= ax) with the specific value.
     May use stack but restores it.
     Can trash flags.
     Only AX and target register are modified. *)
  fun load_reg16 (ax : reg16value, rx : reg16value) (A : reg) (v : Word16.word) =
    raise Tactics "load_reg16 can't take AX. use load_ax16."
    | load_reg16 (ax, rx) r v =
      (* PERF! Check whether we already have the value in r! *)
      let
        val (vh, vl) = w16tow8s v
        val (known_ax, axins) = load_ax16 ax (SOME vh, SOME vl)
      in
        (known_ax, (SOME vh, SOME vl),
         axins @
         [PUSH AX,
          POP (reg_to_multireg16 r)])
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
  fun move16ind8 (modrm <~ reg) =
    let
      val () = check_printable_modrm modrm
      val (known_ax, ins0) = load_ax16 (NONE, NONE) (SOME 0w0, SOME 0w0)
    in
      (* PERF if AX is already 0, or we don't need to keep it, then can avoid push/pop *)
      PUSH AX ::
      ins0 @
      [AND (S16, modrm <~ A),
       POP AX,
       XOR (S16, modrm <~ reg)]
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
  fun not_ax16 (bp : reg16value) : (reg16value * reg16value * ins list) =
  (* Strategy here is to do AX <- XOR(AX, OxFFFF).
     Generating FFFF is pretty cheap (0 - 1). We have to put it in a temporary
     in order to do it; we'll use [EBX]+0x20. *)
    let
      val (known_ax, known_bp, neg1ins) = load_reg16 ((NONE, NONE), bp) CH_BP (Word16.fromInt 0xFFFF)
    in
      (* XXX if we actually know AX's value, just do a load_. *)
      ((NONE, NONE), (SOME 0wxFF, SOME 0wxFF),
       PUSH AX ::
       neg1ins @
       [POP AX] @
        (* note: DOSBox shows the disassembly hint ss:[0120] = blah here, but it
           seems to execute ds:[0120] as expected. *)
        (* MOV (S16, IND_EBX_DISP8 0wx20 <~ CH_BP), *)
       move16ind8 (IND_EBX_DISP8 0wx20 <~ CH_BP) @
       [XOR (S16, A <- IND_EBX_DISP8 0wx20)])
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
  (* XXX return machine state *)
  fun initialize () =
    let
      val (th, tl) = w16tow8s TEMP_START
      val (known_ax, inshigh) = load_ax16 (NONE, NONE) (SOME 0w0, SOME 0w0)
      val (known_ax, inslow) = load_ax16 (SOME 0w0, SOME 0w0) (SOME th, SOME tl)
    (* TODO: Other stuff? *)
    in
      inshigh @
      [PUSH AX] @
      inslow @
      [PUSH AX,
       POP EBX]
    end

  (* XXX this should return the known values *)
  (* Generate code that prints the string. Uses the interrupt instruction, so non-ASCII. *)
  fun printstring s =
    let
      fun emit known_ax known_dx nil = nil
        | emit (known_ax : reg16value) (known_dx : reg16value) (c :: rest) =
        let
          (* Load AH=06, AL=char *)
          val (known_ax, axins) =
            load_ax16 known_ax (SOME 0wx06, SOME ` Word8.fromInt ` ord c)
          (* PERF we actually only need to set DL *)
          val (known_ax, known_dx, dxins) =
            load_reg16 (known_ax, known_dx) D (Word16.fromInt (0x06 * 256 + ord c))
        in
          axins @
          dxins @
          [INT 0wx21] @
          (* interrupt returns character written in AL.
             Not known whether it preserves DX? *)
          emit (SOME 0wx06, SOME ` Word8.fromInt ` ord c) (NONE, NONE) rest
        end
    in
      emit (NONE, NONE) (NONE, NONE) (explode s)
    end

  (* Exits the program. Uses the interrupt instruction, so non-ASCII. *)
  fun exit () =
    [XOR (S16, A <- Register A),
     XOR_A_IMM (I16 (Word16.fromInt 0x4c00)),
     INT 0wx21]

end
