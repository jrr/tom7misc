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

  fun main args =
    let
      val argstring = String.concat args
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
          if !plan
           then
             (case args of
                [phrase1, phrase2] => Anaglyph.makeplan (phrase1, phrase2)
              | _ => raise Anaglyph.Anaglyph "-plan needs exactly two args.")
           else if argstring <> ""
                then Anaglyph.anaglyph argstring
                else print "Give a phrase on the command line, or -dump, or -plan.\n"
    end

  fun go () =
    (Params.main
     "Give a phrase to anagram, or a pair of phrases to plan, etc."
     main)
    handle Anaglyph.Anaglyph s => print ("Anaglyph: " ^ s ^ "\n")
end

val () = AnaglyphMain.go ()