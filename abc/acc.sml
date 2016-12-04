structure Acc :> ACC =
struct

  exception Acc of string

  type ins = X86.ins
  type mach = Machine.mach
  datatype slot = datatype Machine.slot

  (* instruction list is in reverse order
     claimed is a bitmask using the masks below.
     The representation invariant is that a register and its friend are never both
     claimed (e.g. NAND(!!(claimed & AX), !!(claimed & EAX))).
 *)
  type acc = { mach : mach, ins : ins list, claimed : Word32.word }
  fun empty m : acc = { mach = m, ins = nil, claimed = 0w0 }
  fun // ({ mach, ins, claimed }, i) = { mach = mach, ins = i :: ins, claimed = claimed }
  fun insns { mach, ins, claimed } = rev ins
  fun clear_insns { mach, ins = _, claimed } = { mach = mach, ins = nil, claimed = claimed }
  fun ?? ({ mach, ins, claimed }, mf) = { mach = mf mach, ins = ins, claimed = claimed }
  fun mach { mach, ins = _, claimed = _ } = mach

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

  (* Returns the bit for this register, and the bit for its friend (AX's friend is EAX, etc.) *)
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

  fun ++ ({ mach, ins, claimed }, mr) =
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
      { mach = mach, ins = ins, claimed = Word32.orb (claimed, regmask) }
    end

  fun -- ({ mach, ins, claimed }, mr) =
    let
      val (regmask, _) = multimask mr
    in
      if 0w0 = Word32.andb (claimed, regmask)
      then raise Acc ("Can't release unclaimed " ^ X86.multiregstring mr)
      else ();
      { mach = mach, ins = ins, claimed = Word32.andb (claimed, Word32.notb regmask) }
    end

  fun assert_claimed ({ mach, ins, claimed }, mr) =
    let
      val (regmask, _) = multimask mr
    in
      if 0w0 = Word32.andb (claimed, regmask)
      then raise Acc ("assert_claimed failed: " ^ X86.multiregstring mr ^ " is not claimed.")
      else ()
    end
  fun assert_unclaimed ({ mach, ins, claimed }, mr) =
    let
      val (regmask, friendmask) = multimask mr
    in
      if 0w0 <> Word32.andb (claimed, regmask)
      then raise Acc ("assert_unclaimed failed: " ^ X86.multiregstring mr ^ " is claimed.")
      else ();
      if 0w0 <> Word32.andb (claimed, friendmask)
      then raise Acc ("assert_unclaimed failed: " ^ X86.multiregstring mr ^ "'s friend " ^
                      X86.multiregstring (friend mr) ^ " is claimed.")
      else ()
    end


  fun forget_reg32 r m = Machine.forget_reg32 m r
  fun forget_reg16 r m = Machine.forget_reg32 m r
  fun forget_slot r s m = Machine.forget_slot m r s

  fun learn_reg32 r w m = Machine.learn_reg32 m r w
  fun learn_reg16 r w m = Machine.learn_reg16 m r w
  fun learn_slot r s w m = Machine.learn_slot m r s w
  fun set_slot r s wo m = Machine.set_slot m r s wo

end