
signature EVAL =
sig

    exception Wrong

    type process

    val begin : AST.exp list -> process list

    (* runs until all processes are blocked or have terminated *)
    val run : process list -> process list

end