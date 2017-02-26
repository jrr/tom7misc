signature TOX86 =
sig

  exception ToX86 of string

  val INIT_SP : Word16.word
  val INIT_IP : Word16.word

  (* Convert to a fully-encoded program, which consists of the
     two 63356-byte segments. *)
  val tox86 : ASM.explicit_program ->
    { cs : Word8Vector.vector,
      ds : Word8Vector.vector }

end