signature ALLOCATETMPS =
sig

  exception AllocateTmps of string

  (* Rewrite the program to use explicit offsets for temporaries.
     This can also coalesce temporaries whose lifetimes do not
     overlap.

     Also fills in Named offsets (and frame size) with Explicit ones.

     Conceivably this could also allocate some temporaries to
     registers, though that is not yet supported. *)
  val allocate : ASM.named_program -> ASM.explicit_program

end
