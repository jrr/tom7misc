
(* Convert to RTL. *)

signature TORTL = 
sig

    exception ToRTL of string

    (* covert a closure-converted CPS term into RTL. The program cexp must
       meet certain invariants that are described in tortl.sml, along
       with the way the translation is preformed. *)
    val convert : CPS.cexp -> RTL.program

end
