signature OPTIMIZEASM =
sig
  exception OptimizeAsm of string

  val optimize : ASM.explicit_program -> ASM.explicit_program
end