
local
  fun eprint (e, s) =
    print ("Exception: " ^ s ^ "\n" ^
           StringUtil.delimit "\n  " (Port.exnhistory e) ^ "\n")
in
(* val () = WriteTest.writeexe() *)

(*
val () = Params.main1
  "Provide the name of a single C file on the command line."
  (fn cfile =>
   ignore (ABC.go cfile))
*)

val () = TacticsTest.all_combinations ()
handle e as (Tactics.Tactics s) => eprint (e, "Tactics: " ^ s ^ "\n")
             | e as (Acc.Acc s) => eprint (e, "Acc: " ^ s ^ "\n")
end
