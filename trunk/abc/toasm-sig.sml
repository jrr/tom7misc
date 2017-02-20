signature TOASM =
sig

  exception ToASM of string

  val toasm : CIL.program -> ASM.named_tmp ASM.program

end