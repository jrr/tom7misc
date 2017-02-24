signature ALLOCATETMPS =
sig

  exception AllocateTmps of string

  (* Rewrite the program to use explicit offsets for temporaries.
     This can also coalesce temporaries whose lifetimes do not
     overlap.

     Also Replaces (Save/Restore)TempsNamed with
     (Save/Restore)TempsExplicit.

     Conceivably this could also allocate some temporaries to
     registers, though that is not yet supported. *)
  val allocate : ASM.named_tmp ASM.program -> ASM.explicit_tmp ASM.program

end
