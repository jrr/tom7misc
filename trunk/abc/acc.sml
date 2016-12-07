structure Acc :> ACC =
struct

  exception Acc of string

  type ctx = X86.ctx
  type ins = X86.ins
  type mach = Machine.mach
  datatype slot = datatype Machine.slot

  (* instruction list is in reverse order
     claimed is a bitmask using the masks below.
     The representation invariant is that a register and its friend are never both
     claimed (e.g. NAND(!!(claimed & AX), !!(claimed & EAX))).
 *)
  datatype acc = A of { ctx : ctx, mach : mach, ins : ins list, insbytes : int,
                        claimed : Word32.word }
  fun empty c m : acc = A { ctx = c, mach = m, ins = nil,
                            insbytes = 0, claimed = 0w0 }
  fun // (A { ctx, mach, ins, insbytes, claimed }, i) =
    A { ctx = ctx, mach = mach, ins = i :: ins,
        insbytes = insbytes + X86.encoded_size ctx i,
        claimed = claimed }
  fun insns (A { ins, ... }) = rev ins
  fun clear_insns (A { ctx, mach, ins = _, insbytes = _, claimed }) =
    A { ctx = ctx, mach = mach, ins = nil, insbytes = 0, claimed = claimed }
  fun insbytes (A { insbytes = b, ... }) = b
  fun ?? (A { ctx, mach, ins, insbytes, claimed }, mf) =
    A { ctx = ctx, mach = mf mach, ins = ins, insbytes = insbytes,
        claimed = claimed }
  fun mach (A { mach, ... }) = mach

  fun dumpstate (A { ctx, mach, ins, insbytes, claimed }) =
    let
      val recent = map X86.insstring (rev (ListUtil.takeupto 25 ins))
    in
      "== Machine ==\n" ^
      Machine.debugstring mach ^ "\n" ^
      "== Claimed ==\n" ^
      (* XXX *)
      Word32.toString claimed ^ "\n" ^
      "== Recent Instructions (total " ^ Int.toString insbytes ^ " bytes) ==\n" ^
      StringUtil.delimit "\n" recent ^ "\n"
    end

  fun friend mr =
    case mr of
      X86.AX  => X86.EAX
    | X86.CX  => X86.ECX
    | X86.DX  => X86.EDX
    | X86.BX  => X86.EBX
    | X86.SP  => X86.ESP
    | X86.BP  => X86.EBP
    | X86.SI  => X86.ESI
    | X86.DI  => X86.EDI
    | X86.EAX => X86.AX
    | X86.ECX => X86.CX
    | X86.EDX => X86.DX
    | X86.EBX => X86.BX
    | X86.ESP => X86.SP
    | X86.EBP => X86.BP
    | X86.ESI => X86.SI
    | X86.EDI => X86.DI

  (* Returns the bit for this register, and the bit for its friend (AX's
friend is EAX, etc.) *)
  fun multimask mr : Word32.word * Word32.word =
    case mr of
      X86.AX  => (0wx01, 0wx010000)
    | X86.CX  => (0wx02, 0wx020000)
    | X86.DX  => (0wx04, 0wx040000)
    | X86.BX  => (0wx08, 0wx080000)
    | X86.SP  => (0wx10, 0wx100000)
    | X86.BP  => (0wx20, 0wx200000)
    | X86.SI  => (0wx40, 0wx400000)
    | X86.DI  => (0wx80, 0wx800000)
    | X86.EAX => (0wx010000, 0wx01)
    | X86.ECX => (0wx020000, 0wx02)
    | X86.EDX => (0wx040000, 0wx04)
    | X86.EBX => (0wx080000, 0wx08)
    | X86.ESP => (0wx100000, 0wx10)
    | X86.EBP => (0wx200000, 0wx20)
    | X86.ESI => (0wx400000, 0wx40)
    | X86.EDI => (0wx800000, 0wx80)

  fun ++ (A { ctx, mach, ins, insbytes, claimed }, mr) =
    let
      val (regmask, friendmask) = multimask mr
    in
      if 0w0 <> Word32.andb (claimed, regmask)
      then raise Acc ("Can't claim already-claimed " ^ X86.multiregstring mr)
      else ();
      if 0w0 <> Word32.andb (claimed, friendmask)
      then raise Acc ("Can't claim " ^ X86.multiregstring mr ^ " because " ^
                      X86.multiregstring (friend mr) ^ " is already claimed")
      else ();
      A { ctx = ctx, mach = mach, ins = ins, insbytes = insbytes,
          claimed = Word32.orb (claimed, regmask) }
    end

  fun -- (A { ctx, mach, ins, insbytes, claimed }, mr) =
    let
      val (regmask, _) = multimask mr
    in
      if 0w0 = Word32.andb (claimed, regmask)
      then raise Acc ("Can't release unclaimed " ^ X86.multiregstring mr)
      else ();
      A { ctx = ctx, mach = mach, ins = ins, insbytes = insbytes,
          claimed = Word32.andb (claimed, Word32.notb regmask) }
    end

  fun blocking_claim (A { claimed, ... }) mr =
    let
      val (regmask, friendmask) = multimask mr
    in
      if 0w0 <> Word32.andb (claimed, regmask)
      then SOME mr
      else if 0w0 <> Word32.andb (claimed, friendmask)
           then SOME (friend mr)
           else NONE
    end

  fun can_be_claimed acc mr = Option.isSome (blocking_claim acc mr)

  fun assert_claimed (acc as (A { claimed, ... })) mr =
    let
      val (regmask, friendmask) = multimask mr
      (* If this is a 16-bit register, then we're ok if either the
         16- or 32-bit register is claimed. *)
      val mask =
        case X86.decode_multireg mr of
          (X86.S16, _) => Word32.orb (regmask, friendmask)
        | (X86.S32, _) => regmask
        | _ => raise Acc "bug: multireg has 16- or 32-bit size."
    in
      if 0w0 = Word32.andb (claimed, mask)
      then raise Acc ("assert_claimed failed: " ^ X86.multiregstring mr ^
                      " is not claimed.\n" ^
                      dumpstate acc)
      else ()
    end
  fun assert_unclaimed (acc as (A { claimed, ... })) mr =
    let
      val (regmask, friendmask) = multimask mr
    in
      if 0w0 <> Word32.andb (claimed, regmask)
      then raise Acc ("assert_unclaimed failed: " ^ X86.multiregstring mr ^
                      " is claimed.\n" ^
                      dumpstate acc)
      else ();
      if 0w0 <> Word32.andb (claimed, friendmask)
      then raise Acc ("assert_unclaimed failed: " ^ X86.multiregstring mr ^
                      "'s friend " ^ X86.multiregstring (friend mr) ^
                      " is claimed.\n" ^
                      dumpstate acc)
      else ()
    end

  fun forget_reg32 r m = Machine.forget_reg32 m r
  fun forget_reg16 r m = Machine.forget_reg32 m r
  fun forget_multireg mr m =
    case mr of
      X86.AX  => forget_reg16 Machine.EAX m
    | X86.CX  => forget_reg16 Machine.ECX m
    | X86.DX  => forget_reg16 Machine.EDX m
    | X86.BX  => forget_reg16 Machine.EBX m
    | X86.SP  => forget_reg16 Machine.ESP m
    | X86.BP  => forget_reg16 Machine.EBP m
    | X86.SI  => forget_reg16 Machine.ESI m
    | X86.DI  => forget_reg16 Machine.EDI m
    | X86.EAX => forget_reg32 Machine.EAX m
    | X86.ECX => forget_reg32 Machine.ECX m
    | X86.EDX => forget_reg32 Machine.EDX m
    | X86.EBX => forget_reg32 Machine.EBX m
    | X86.ESP => forget_reg32 Machine.ESP m
    | X86.EBP => forget_reg32 Machine.EBP m
    | X86.ESI => forget_reg32 Machine.ESI m
    | X86.EDI => forget_reg32 Machine.EDI m

  fun forget_slot r s m = Machine.forget_slot m r s

  fun learn_reg32 r w m = Machine.learn_reg32 m r w
  fun learn_reg16 r w m = Machine.learn_reg16 m r w
  fun learn_slot r s w m = Machine.learn_slot m r s w
  fun set_slot r s wo m = Machine.set_slot m r s wo

end