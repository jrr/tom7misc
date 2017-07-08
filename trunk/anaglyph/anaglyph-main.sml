structure AnaglyphMain =
struct

  val dump = Params.flag false
    (SOME ("-dump", "Dump the canonized dictionary.")) "dump"

  val treedot = Params.flag false
    (SOME ("-treedot", "Dump the tree as a .dot file.")) "treedot"

  val tree = Params.flag false
    (SOME ("-tree", "Dump the tree as a text file.")) "tree"

  val plan = Params.flag false
    (SOME ("-plan", "Plan an animation between phrase1 and phrase2")) "plan"

  val best = Params.flag false
    (SOME ("-best", "Output only the longest words that can be made " ^
           "from the phrase.")) "best"

  val require = Params.param ""
    (SOME ("-require", "When anagramming or generating best words, first " ^
           "subtract this phrase (which must be achievable). It's not " ^
           "included in the output."))
    "require"

  val maxwords = Params.param "1000"
    (SOME ("-maxwords", "Maximum number of words when generating anagrams."))
    "maxwords"

  fun main args =
    let
      val argstring = String.concat args
      val require = !require
      val maxwords = Params.asint 1000 maxwords
    in
      if !dump
      then StringUtil.writefile "canonized.txt" (Anaglyph.canonized_file ())
      else
        if !treedot
        then StringUtil.writefile "tree.dot" (Anaglyph.tree_dotfile ())
        else
        if !tree
        then StringUtil.writefile "tree.txt" (Anaglyph.tree_textfile ())
        else
        if !best
        then Anaglyph.best_requiring require argstring
        else
        if !plan
         then
           (case args of
              [phrase1, phrase2] => Anaglyph.makeplan (phrase1, phrase2)
            | _ => raise Anaglyph.Anaglyph "-plan needs exactly two args.")
         else if argstring <> ""
              then Anaglyph.anaglyph_requiring maxwords require argstring
              else print "Give a phrase on the command line, or -dump, or -plan.\n"
    end

  fun go () =
    (Params.main
     "Give a phrase to anagram, or a pair of phrases to plan, etc."
     main)
    handle Anaglyph.Anaglyph s => print ("Anaglyph: " ^ s ^ "\n")
end

val () = AnaglyphMain.go ()