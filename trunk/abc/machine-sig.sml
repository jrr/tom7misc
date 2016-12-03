signature MACHINE =
sig

  (* Gives the machine state, which is partial knowledge about
     the values of registers, temporaries (according to ABC's
     particular execution strategy) and some flags.
     Immutable. *)
  type mach

  val all_unknown : mach

  (* Though only some registers have independent names for the
     individual bytes, we track knowledge of each byte of each
     register. ___@ is the least significant byte, etc. *)
  datatype slot = @--- | -@-- | --@- | ---@
  datatype reg = EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI

  val forget_reg32 : mach -> reg -> mach
  val forget_reg16 : mach -> reg -> mach    
  val forget_slot : mach -> reg -> slot -> mach
  val learn_reg32 : mach -> reg -> Word32.word -> mach
  val learn_reg16 : mach -> reg -> Word16.word -> mach
  val learn_slot : mach -> reg -> slot -> Word8.word -> mach

  val slot : mach -> reg -> slot -> Word8.word option
  (* If either slot is unknown, this will be NONE. Query the individual
     slots if they would be useful on their own. *)
  val reg16 : mach -> reg -> Word16.word option
  (* All slots must be known. *)
  val reg32 : mach -> reg -> Word32.word option

  (* TODO: processor flags *)
  (* TODO: temporary values *)

  val debugstring : mach -> string
    
end