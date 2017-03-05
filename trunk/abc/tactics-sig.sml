signature TACTICS =
sig

  exception Tactics of string
  type acc = Acc.acc
  type reg = X86.reg

  val printable : Word8.word -> bool
  val printable16 : Word16.word -> bool

  (* New good stuff used in ToX86. *)

  (* initialize (acc, init_ebp, init_ebx) *)
  val initialize : acc * Word16.word * Word16.word -> acc

  (* claim_reg16 acc l f
     Claim one of the registers in l, preferring registers towards the
     front of the list.

     The continuation f is run with the register marked as claimed, and then
     it is unclaimed at the end. *)
  val claim_reg16 : acc -> (reg list) -> (acc * reg -> acc) -> acc

  (* modrm byte for a temporary offset from EBP; should usually be used
     instead of IND_EBP_DISP8. Here, an argument of 0 actually results
     in a displacement of the minimum printable value, 0x20. We keep
     EBP 0x20 bytes shy of the nominal start of the frame, so that we
     can use it this way. *)
  val EBP_TEMPORARY : int -> X86.modrm

  (* mov16ind8 acc (reg <- modrm)   or
     mov16ind8 acc (modrm <~ reg)
     Simulates the MOV instruction for printable modrm.
     Most efficient if AX is not claimed. *)
  val mov16ind8 : acc -> X86.args -> acc

  (* mov acc dst_tmp src_tmp *)
  val mov_tmp16_to_tmp16 : acc -> int -> int -> acc

  (* load16 acc dst_tmp addr_tmp
     sets dst_tmp to contain the 16-bit word pointed to by
     the address in addr_tmp. *)
  val load16 : acc -> int -> int -> acc

  (* store16 acc dst_addr src_tmp
     sets the 16-bit word pointed to by the address in dst_addr
     to the value in src_tmp. *)
  val store16 : acc -> int -> int -> acc

  (* Adjust temporary frame or local frame base pointers.
     Give the size of the temporary frame in case it needs
     to use a temporary.

     add_bp acc tmp_frame_size offset
     add_bx acc tmp_frame_size offset *)
  val add_bp : acc -> int -> Word16.word -> acc
  val sub_bp : acc -> int -> Word16.word -> acc
  val add_bx : acc -> int -> Word16.word -> acc
  val sub_bx : acc -> int -> Word16.word -> acc

  (* xor_tmp16 acc dst_tmp src_tmp
     sets dst_tmp = dst_tmp XOR src_tmp. *)
  val xor_tmp16 : acc -> int -> int -> acc

  (* sub_tmp16 acc dst_tmp src_tmp
     sets dst_tmp = dst_tmp - src_tmp. *)
  val sub_tmp16 : acc -> int -> int -> acc

  (* add_tmp16 acc dst_tmp src_tmp *)
  val add_tmp16 : acc -> int -> int -> acc

  (* sub_tmp16_lit acc tmp lit
     Subtract the literal from the temporary at the given offset. *)
  val sub_tmp16_lit : acc -> int -> Word16.word -> acc


  (* push or pop the 16-bit temporary.

     push_tmp16 acc tmp
     pop_tmp16 acc tmp *)
  val push_tmp16 : acc -> int -> acc
  val pop_tmp16 : acc -> int -> acc

  (* Set AX (must be claimed) to the given word. *)
  val imm_ax16 : acc -> Word16.word -> acc

  (* imm_tmp16 acc tmp value *)
  val imm_tmp16 : acc -> int -> Word16.word -> acc

  (* imm_reg16 acc reg value.
     AX and reg must both be claimed. Trashes AX. *)
  val imm_reg16 : acc -> X86.reg -> Word16.word -> acc

  (* Exit the program. Depends on the initialization code above
     already having been executed. *)
  val exit : acc -> acc

  (* Deprecated testing stuff. *)

  (* putc16 acc tmp
     writes the character in the 16-bit temporary to stdout.
     NOTE: This can't be done with printable opcodes, so it's
     just for debugging! *)
  val putc16 : acc -> int -> acc

  val old_initialize : unit -> acc
  val printstring : acc -> string -> acc

end
