signature OPTIMIZECIL =
sig
  exception OptimizeCIL of string
  (* Transform the program into a semantically equivalent one, but that
     is smaller/faster. Because of the 64k code limit, we're mostly
     interested in code- and data-size optimizations. *)
  val optimize : CIL.program -> CIL.program
end
