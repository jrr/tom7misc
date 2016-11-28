
(* val () = WriteTest.writeexe() *)

val () = Params.main1
  "Provide the name of a single C file on the command line."
  (fn cfile =>
   ignore (ABC.go cfile))
