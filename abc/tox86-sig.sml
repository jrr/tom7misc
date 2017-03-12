signature TOX86 =
sig

  exception ToX86 of string

  val INIT_SP : Word16.word

  (* Convert to a fully-encoded program, which consists of the
     two 63356-byte segments. *)
  val tox86 : ASM.explicit_program ->
    { init_ip : Word16.word,
      debug : (int * (string * Acc.acc)) list,
      (* XXX can compute from debug *)
      codebytes : int,
      cs : Word8Vector.vector,
      ds : Segment.segment }

end