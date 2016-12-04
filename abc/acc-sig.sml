(* State for tactics, called "acc" for accumulator. This keeps track of:
   - the machine state (Machine), which
   - An accumulated instruction queue
   - Status of registers and temporaries as "claimed" (like callee-saved) [TODO]

It's purely functional and has a semi-monadic structure so that it can be
used for assembly like sequences of instructions and annotations. *)
signature ACC =
sig
  exception Acc of string

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

  (* Claim a 16-bit or 32-bit register.
     Claiming an already-claimed register is fatal, including claiming the 16 bit version
     when the 32-bit one is claimed, or vice versa. *)
  val ++ : acc * X86.multireg -> acc
  (* Release a 16-bit or 32-bit register. Releasing an already-free register is fatal.
     Must release at the same width as it was claimed. *)
  val -- : acc * X86.multireg -> acc

  (* Checks that neither the register nor its 16/32-bit are claimed. *)
  val assert_unclaimed : acc * X86.multireg -> unit
  val assert_claimed : acc * X86.multireg -> unit

  (* TODO: Get free 32-bit or 16-bit register. Maybe in tactics? *)

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
    infix // ?? ++ --
  *)
end