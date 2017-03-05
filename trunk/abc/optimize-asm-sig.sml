signature OPTIMIZEASM =
sig
  exception OptimizeAsm of string

  (* XXX Some optimizations can run on named and explicit version? *)
  val optimize : ASM.explicit_program -> ASM.explicit_program
end