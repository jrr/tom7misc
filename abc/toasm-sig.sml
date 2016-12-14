signature TOASM =
sig

  exception ToASM of string

  val toasm : CIL.program -> ASM.program

end