
signature TYPE =
sig

    exception TypeError of string

    val typecheck : AST.exp -> AST.typ

end