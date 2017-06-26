
signature SUBST =
sig

  (* renames bound variables to be unique *)
  val rename : AST.exp -> AST.exp
    
  val subst : string * AST.exp -> AST.exp -> AST.exp

end