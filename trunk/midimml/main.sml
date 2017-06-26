
(* for mlton or other batch compilers. *)

val _ =
  case Params.docommandline () of
    [input] => Inst.convert input
  | _ =>
      let in
        print ("usage: midimml file.mid\n\n");
        print (Params.usage ());
        print "\n"
      end
