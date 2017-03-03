
local
  fun eprint (e, s) =
    print ("Exception: " ^ s ^ "\n" ^
           exnMessage e ^ "\n" ^
           StringUtil.delimit "\n  " (Port.exnhistory e) ^ "\n")

  val outfile_param =
    Params.param "a.exe" (SOME ("-o",
                                "Set the output file.")) "outfile"

in

  val () = Params.main1
    "Provide the name of a single C file on the command line."
    (fn cfile =>
     ignore (ABC.go { input = cfile, output = !outfile_param }))

end
