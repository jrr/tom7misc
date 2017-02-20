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
         first 256 bytes are overwritten by the PSP. *)
      ds : Word8Vector.vector

      (* TODO: Filler... *)
      } -> string -> unit

end