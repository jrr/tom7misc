
(* val s = NCBI.parsefile "NC_005061.seq" *)
(* big *)
(* val s = NCBI.parsefile "NC_002928.seq" *)
(* val _ = Stream.app (fn _ => ()) s *)


(* val _ = Parse.parsefile "NC_002928.seq" *)
(* val _ = Parse.parsefile "NC_005061.seq" *)

(*
val _ = case CommandLine.arguments () of
  [s] => ignore (Parse.parsefile (fn _ => true) s)
 | _ => print ("give file on line\n")
val _ = print "Done parsing.\n"

*)