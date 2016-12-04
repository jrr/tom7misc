(* State for tactics, called "acc" for accumulator. This keeps track of:
   - the machine state (Machine), which
   - An accumulated instruction queue
   - Status of registers and temporaries as "claimed" (like callee-saved) [TODO]

It's purely functional and has a semi-monadic structure so that it can be
used for assembly like sequences of instructions and annotations. *)
signature ACC =
sig
  type acc
  (* Create accumulator from machine state. Empty instruction queue and no
     claimed registers. *)
  val empty : Machine.mach -> acc
  (* Append instruction. Doesn't interpret it. *)
  val // : acc * X86.ins -> acc
  (* Get instructions in forward order. *)
  val insns : acc -> X86.ins list
  (* Clear instructions. *)
  val clear_insns : acc -> acc
  (* Get the machine *)
  val mach : acc -> Machine.mach
  (* Apply transformation to machine. *)
  val ?? : acc * (Machine.mach -> Machine.mach) -> acc

  (* Curried such that mach is in the last slot; suitable for ?? op.
     Maybe these should just have the following types in Machine? *)
  val forget_reg32 : Machine.reg -> Machine.mach -> Machine.mach
  val forget_reg16 : Machine.reg -> Machine.mach -> Machine.mach
  val forget_slot : Machine.reg -> Machine.slot -> Machine.mach -> Machine.mach
  val learn_reg32 : Machine.reg -> Word32.word -> Machine.mach -> Machine.mach
  val learn_reg16 : Machine.reg -> Word16.word -> Machine.mach -> Machine.mach
  val learn_slot : Machine.reg -> Machine.slot -> Word8.word -> Machine.mach -> Machine.mach
  val set_slot : Machine.reg -> Machine.slot -> Word8.word option -> Machine.mach -> Machine.mach

  (* TODO: Execute the instruction, updating the machine state. *)
  (* val exec : acc * ins -> acc *)

  (*
    infix // ??
  *)
end