signature TACTICS =
sig

  exception Tactics of string
  type acc = Acc.acc

  (* New good stuff used in ToX86. *)

  (* initialize (acc, init_ebp, init_ebx) *)
  val initialize : acc * Word16.word * Word16.word -> acc

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