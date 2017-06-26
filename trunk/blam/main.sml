
val _ =
    (case Params.docommandline () of
         [a, b] => ignore (Blam.blam (a, b))
       | _ =>
             let in
                 TextIO.output(TextIO.stdErr, Params.usage ());
                 TextIO.output(TextIO.stdErr, "blam [options] file1.seq file2.seq\n")
             end) handle Blam.Blam s =>
         TextIO.output(TextIO.stdErr, "fatal error: " ^ s)