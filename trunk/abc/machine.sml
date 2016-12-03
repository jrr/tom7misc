structure Machine :> MACHINE =
struct

  datatype slot = @--- | -@-- | --@- | ---@
  datatype reg = EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI

  (* PERF: Could use a word32 and a word4 (but chunk 'em) for this. *)
  type kb = Word8.word option
  (* MSB to LSB *)
  type kr = kb * kb * kb * kb
  val UNK : kr = (NONE, NONE, NONE, NONE)

  datatype mach = M of
    { regs :
      { eax : kr,
        ecx : kr,
        edx : kr,
        ebx : kr,
        esp : kr,
        ebp : kr,
        esi : kr,
        edi : kr } }

  val all_unknown = M { regs = { eax = UNK, ecx = UNK, edx = UNK, ebx = UNK,
                                 esp = UNK, ebp = UNK, esi = UNK, edi = UNK } }

  fun update_reg (M { regs = { eax, ecx, edx, ebx, esp, ebp, esi, edi } }) reg32 f =
    let
      val regs =
        case reg32 of
          EAX => { eax = f eax, ecx = ecx, edx = edx, ebx = ebx,
                   esp = esp, ebp = ebp, esi = esi, edi = edi }
        | ECX => { eax = eax, ecx = f ecx, edx = edx, ebx = ebx,
                   esp = esp, ebp = ebp, esi = esi, edi = edi }
        | EDX => { eax = eax, ecx = ecx, edx = f edx, ebx = ebx,
                   esp = esp, ebp = ebp, esi = esi, edi = edi }
        | EBX => { eax = eax, ecx = ecx, edx = edx, ebx = f ebx,
                   esp = esp, ebp = ebp, esi = esi, edi = edi }
        | ESP => { eax = eax, ecx = ecx, edx = edx, ebx = ebx,
                   esp = f esp, ebp = ebp, esi = esi, edi = edi }
        | EBP => { eax = eax, ecx = ecx, edx = edx, ebx = ebx,
                   esp = esp, ebp = f ebp, esi = esi, edi = edi }
        | ESI => { eax = eax, ecx = ecx, edx = edx, ebx = ebx,
                   esp = esp, ebp = ebp, esi = f esi, edi = edi }
        | EDI => { eax = eax, ecx = ecx, edx = edx, ebx = ebx,
                   esp = esp, ebp = ebp, esi = esi, edi = f edi }
    in
      M { regs = regs }
    end

  fun forget_reg32 mach reg =
    let
      fun up _ = UNK
    in
      update_reg mach reg up
    end

  fun forget_reg16 mach reg =
    let
      fun up (a, b, _, _) = (a, b, NONE, NONE)
    in
      update_reg mach reg up
    end

  fun forget_slot mach reg slot =
    let
      fun up (a, b, c, d) =
        case slot of
          @--- => (NONE, b, c, d)
        | -@-- => (a, NONE, c, d)
        | --@- => (a, b, NONE, d)
        | ---@ => (a, b, c, NONE)
    in
      update_reg mach reg up
    end

  fun learn_slot mach reg slot w =
    let
      fun up (a, b, c, d) =
        case slot of
          @--- => (SOME w, b, c, d)
        | -@-- => (a, SOME w, c, d)
        | --@- => (a, b, SOME w, d)
        | ---@ => (a, b, c, SOME w)
    in
      update_reg mach reg up
    end

  fun learn_reg16 mach reg w =
    let
      val wh = Word8.fromInt (Word16.toInt (Word16.andb(Word16.>>(w, 0w8),
                                                        Word16.fromInt 0xFF)))
      val wl = Word8.fromInt (Word16.toInt (Word16.andb(w,
                                                        Word16.fromInt 0xFF)))
      fun up (a, b, _, _) = (a, b, SOME wh, SOME wl)
    in
      update_reg mach reg up
    end

  fun learn_reg32 mach reg w =
    let
      val wa = Word8.fromInt (Word32.toInt (Word32.andb(Word32.>>(w, 0w24), 0wxFF)))
      val wb = Word8.fromInt (Word32.toInt (Word32.andb(Word32.>>(w, 0w16), 0wxFF)))
      val wc = Word8.fromInt (Word32.toInt (Word32.andb(Word32.>>(w, 0w8), 0wxFF)))
      val wd = Word8.fromInt (Word32.toInt (Word32.andb(w, 0wxFF)))
      fun up _ = (SOME wa, SOME wb, SOME wc, SOME wd)
    in
      update_reg mach reg up
    end

  fun get_reg (M { regs = { eax, ecx, edx, ebx, esp, ebp, esi, edi } }) reg32 =
    case reg32 of
      EAX => eax
    | ECX => ecx
    | EDX => edx
    | EBX => ebx
    | ESP => esp
    | EBP => ebp
    | ESI => esi
    | EDI => edi


  fun reg32 mach reg =
    case get_reg mach reg of
      (SOME a, SOME b, SOME c, SOME d) =>
        let
          val a = Word32.fromInt (Word8.toInt a)
          val b = Word32.fromInt (Word8.toInt b)
          val c = Word32.fromInt (Word8.toInt c)
          val d = Word32.fromInt (Word8.toInt d)
          val || = Word32.orb
          infix ||
        in
          SOME (Word32.<<(a, 0w24) ||
                Word32.<<(b, 0w16) ||
                Word32.<<(c, 0w8) ||
                d)
        end
    | _ => NONE

  fun slot mach reg s =
    case (s, get_reg mach reg) of
      (@---, (a, _, _, _)) => a
    | (-@--, (_, b, _, _)) => b
    | (--@-, (_, _, c, _)) => c
    | (---@, (_, _, _, d)) => d

  fun reg16 mach reg =
    case get_reg mach reg of
      (_, _, SOME c, SOME d) =>
        let
          val c = Word16.fromInt (Word8.toInt c)
          val d = Word16.fromInt (Word8.toInt d)
        in
          SOME (Word16.orb(Word16.<<(c, 0w8), d))
        end
    | _ => NONE


  fun kbs NONE = "??"
    | kbs (SOME w) =
    let val s = Word8.toString w
    in
      if size s = 1
      then "0" ^ s
      else s
    end

  fun krs (a, b, c, d) = String.concat [kbs a, kbs b, kbs c, kbs d]

  fun debugstring (M { regs = { eax, ecx, edx, ebx,
                                esp, ebp, esi, edi } }) =
    let
    in
      String.concat ["EAX ", krs eax, "  ECX ", krs ecx, "  EDX ", krs edx, "  EBX ", krs ebx, "\n",
                     "ESP ", krs esp, "  EBP ", krs ebp, "  ESI ", krs esi, "  EDI ", krs edi]

    end

end