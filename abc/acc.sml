structure Acc :> ACC =
struct

  type ins = X86.ins
  type mach = Machine.mach
  datatype slot = datatype Machine.slot

  (* instruction list is in reverse order *)
  type acc = mach * ins list
  fun empty m : acc = (m, nil)
  fun // ((m, l), i) = (m, i :: l)
  fun insns (m, l) = rev l
  fun clear_insns (m, _) = (m, nil)
  fun ?? ((m, l), mf) = (mf m, l)
  fun mach (m, _) = m

  fun forget_reg32 r m = Machine.forget_reg32 m r
  fun forget_reg16 r m = Machine.forget_reg32 m r
  fun forget_slot r s m = Machine.forget_slot m r s

  fun learn_reg32 r w m = Machine.learn_reg32 m r w
  fun learn_reg16 r w m = Machine.learn_reg16 m r w
  fun learn_slot r s w m = Machine.learn_slot m r s w
  fun set_slot r s wo m = Machine.set_slot m r s wo


end