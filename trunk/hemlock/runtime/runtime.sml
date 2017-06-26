
(* describes the runtime library *)

structure Runtime =
struct

    datatype basetype =
        BINT
      | BSTRING
      | BSTRINGVEC
      (* ...? *)

    (* primop implemented by
         hemlock symbol, domain, codomain, name in runtime lib, name for type *)
    val fns =
        map (fn (po, base, dom, cod) =>
             (po, ("h_" ^ base, dom, cod, "hemrunt_" ^ base, base ^ "t")))
        [
         (* write a string to the given file descriptor *)
         (Primop.PWrite, "write", [BINT, BSTRING], SOME BINT),

         (* submit some cord code with the supplied arg and no deps.
            return the cord_id. *)
         (Primop.PSubmit, "submit", [BSTRING], SOME BSTRING),

         (* wait for the results of all cord ids (in vector), then
            return the vector of those results *)
         (* XXX should be BMARSHVEC?? *)
         (Primop.PWaitall, "waitall", [BSTRINGVEC], SOME BSTRINGVEC)
         ]

end