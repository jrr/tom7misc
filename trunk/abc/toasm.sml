structure ToASM :> TOASM =
struct

  structure C = CIL
  structure A = ASM

  exception ToASM of string

  fun doproc (name, C.Func
              { args : (string * C.typ) list,
                ret : C.typ,
                body : C.stmt,
                blocks : (string * C.stmt) list }) : A.proc =
    let
    in
      raise ToASM "unimplemented"
    end

  (* XXX globals ! *)
  fun toasm (C.Program { functions, globals = _ }) =
     A.Program { main = "main",
                 procs = map doproc functions }
end