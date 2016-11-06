signature TOCIL =
sig
  exception ToCIL of string
  (* Convert from the ckit AST to CIL. *)
  val tocil : Ast.ast -> CIL.program
end
