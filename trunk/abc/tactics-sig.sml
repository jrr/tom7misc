signature TACTICS =
sig

  exception Tactics of string
  type acc = Acc.acc
  type reg = X86.reg

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

  (* Adjust temporary frame or local frame base pointers.
     Give the size of the temporary frame in case it needs
     to use a temporary.

     add_bp acc tmp_frame_size offset
     add_bx acc tmp_frame_size offset *)
  val add_bp : acc -> int -> Word16.word -> acc
  val sub_bp : acc -> int -> Word16.word -> acc
  val add_bx : acc -> int -> Word16.word -> acc
  val sub_bx : acc -> int -> Word16.word -> acc

  (* push or pop the 16-bit temporary.

     push_tmp16 acc tmp
     pop_tmp16 acc tmp *)
  val push_tmp16 : acc -> int -> acc
  val pop_tmp16 : acc -> int -> acc

  (* Load AX (must be claimed) with the given word. *)
  val load_ax16 : acc -> Word16.word -> acc

  (* imm_tmp16 acc tmp value *)
  val imm_tmp16 : acc -> int -> Word16.word -> acc

  (* Exit the program. Depends on the initialization code above
     already having been executed. *)
  val exit : acc -> acc

  (* Deprecated testing stuff. *)

  val old_initialize : unit -> acc
  val printstring : acc -> string -> acc

end