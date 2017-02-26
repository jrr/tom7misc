(* State for tactics, called "acc" for accumulator. This keeps track of:
   - the machine state (Machine), which
   - An accumulated instruction queue
   - Status of registers as "claimed"

   It's purely functional and has a semi-monadic structure so that it can be
   used for assembly like sequences of instructions and annotations. *)
signature ACC =
sig
  exception Acc of string

  type acc
  (* Create accumulator from machine state. Empty instruction queue and no
     claimed registers. *)
  val empty : X86.ctx -> Machine.mach -> acc
  (* Append instruction. Doesn't interpret it. *)
  val // : acc * X86.ins -> acc
  (* Get instructions in forward order. *)
  val insns : acc -> X86.ins list
  (* Get encoded instructions. *)
  val encoded : acc -> Word8Vector.vector
  (* Clear instructions. *)
  val clear_insns : acc -> acc
  (* Get the number of bytes of encoded instructions. *)
  val insbytes : acc -> int
  (* Get the machine *)
  val mach : acc -> Machine.mach
  (* Apply transformation to machine. *)
  val ?? : acc * (Machine.mach -> Machine.mach) -> acc

  (* Claiming registers. A 16-bit register's friend is the 32-bit version,
     and vice versa.
     A register and its friend cannot be simultaneously claimed. *)
  val friend : X86.multireg -> X86.multireg
  (* Claim a 16-bit or 32-bit register.
     Claiming a register that's already claimed, or that whose friend is
     claimed, is fatal. *)
  val ++ : acc * X86.multireg -> acc
  (* Release a 16-bit or 32-bit register. Releasing an already-free register
     is fatal.
     Must release at the same width as it was claimed. *)
  val -- : acc * X86.multireg -> acc

  (* True if the register and its friend are both unclaimed *)
  val can_be_claimed : acc -> X86.multireg -> bool
  (* Returns SOME mr for the multireg mr that, if unclaimed,
     makes the argument register claimable. This can only be the
     argument register or its friend. If both the register and
     its friend are unclaimed, returns NONE (this means it can
     be claimed). *)
  val blocking_claim : acc -> X86.multireg -> X86.multireg option

  (* Checks that neither the register nor its 16/32-bit friend are claimed. *)
  val assert_unclaimed : acc -> X86.multireg -> unit
  (* Assert that the register is claimed. AX is claimed if EAX is claimed
     (since it is a proper subregister), but not vice versa. *)
  val assert_claimed : acc -> X86.multireg -> unit

  (* TODO: Get free 32-bit or 16-bit register. Maybe in tactics? *)

  (* Curried such that mach is in the last slot; suitable for ?? op.
     Maybe these should just have the following types in Machine? *)
  val forget_reg32 : Machine.reg -> Machine.mach -> Machine.mach
  val forget_reg16 : Machine.reg -> Machine.mach -> Machine.mach
  val forget_multireg : X86.multireg -> Machine.mach -> Machine.mach
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

  val debug_string : acc -> string
end
