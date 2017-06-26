
val outf = Params.param ""
    (SOME ("-o",
           "Set name of executable.")) "outf"

val _ =
    case Params.docommandline () of
        [input] =>
            let val ff = 
                (case !outf of
                     "" => (#1 (FSUtil.splitext input)) ^ ".exe"
                   | s => s)
            in
                Compile.compile input ff;
                ()
            end
      | _ =>
            let in
                print "Usage: hemlock file.hml\n\n";
                print (Params.usage ())
            end