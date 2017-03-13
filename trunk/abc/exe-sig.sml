signature EXE =
sig

  exception EXE of string

  (* write_exe contents filename *)
  val write_exe :
    { (* Many header values are delicately set up, so they can't
         be controlled by the caller. *)
      (* However, these can be set to any (printable) value. *)
      init_ip : Word16.word,
      init_sp : Word16.word,
      (* Already-assembled code segment. Must be 65536 printable bytes.
         Execution starts at the offset given in init_ip. *)
      cs : Word8Vector.vector,
      (* Data segment. Must be 65536 printable bytes. Recall that the
         first 256 bytes do not make their way to the program image
         because of the PSP. *)
      ds : Segment.segment,
      (* If true, write the first 256 bytes of the data segment into
         the end of the header region; this is just for illustration
         purposes, because that part isn't even loaded by DOS (that's
         where the PSP goes). This only works if the header size is
         large enough to accommodate these 256 bytes at the end
         without overwriting the relocation table or header struct. *)
      include_psp : bool

      (* TODO: Filler... *)
      } -> string -> unit

end