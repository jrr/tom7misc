structure AnagraphMain =
struct

  val dump = Params.flag false
    (SOME ("-dump", "Dump the canonized dictionary.")) "dump"

  val specialty = Params.flag false
    (SOME ("-specialty", "Run some specialty word finding.")) "specialty"

  val treedot = Params.flag false
    (SOME ("-treedot", "Dump the tree as a .dot file.")) "treedot"

  val tree = Params.flag false
    (SOME ("-tree", "Dump the tree as a text file.")) "tree"

  val js = Params.flag false
    (SOME ("-js", "Dump the tree and atom data as javascript. " ^
           "Used for the anagraph explorer.")) "js"

  val plan = Params.flag false
    (SOME ("-plan", "Plan an animation between phrase1 and phrase2")) "plan"

  val best = Params.flag false
    (SOME ("-best", "Output only the longest words that can be made " ^
           "from the phrase.")) "best"

  val banned = Params.param ""
    (SOME ("-banned", "Comma-separated list of words that cannot be used " ^
           "in the output.")) "banned"

  val nontrivial = Params.flag false
    (* XXX make this work for anagraphing as well *)
    (SOME ("-nontrivial", "When dumping the canonized dictionary, " ^
           "only output clusters that contain non-trivial uses of " ^
           "the ruleset, i.e., are not plain anagrams.")) "nontrivial"

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
      val banned = String.fields (StringUtil.ischar #",") (!banned)
      val maxwords = Params.asint 1000 maxwords
      val nontrivial = !nontrivial
    in
      if !dump
      then StringUtil.writefile "canonized.txt"
           (Anagraph.canonized_file nontrivial)
      else
        if !specialty
        then Anagraph.specialty ()
        else
        if !treedot
        then StringUtil.writefile "tree.dot" (Anagraph.tree_dotfile ())
        else
        if !tree
        then StringUtil.writefile "tree.txt" (Anagraph.tree_textfile ())
        else
        if !js
        then StringUtil.writefile "tree.js" (Anagraph.tree_js ())
        else
        if !best
        then Anagraph.best_requiring require banned argstring
        else
        if !plan
         then
           (case args of
              [phrase1, phrase2] => Anagraph.makeplan (phrase1, phrase2)
            | _ => raise Anagraph.Anagraph "-plan needs exactly two args.")
         else if argstring <> ""
              then Anagraph.anagraph_requiring maxwords require banned argstring
              else print ("Give a phrase to anagraph, or use some " ^
                          "other mode:\n" ^ Params.usage ())
    end

  fun go () =
    (Params.main
     "Give a phrase to anagraph, or a pair of phrases to plan, etc."
     main)
    handle Anagraph.Anagraph s => print ("Anagraph: " ^ s ^ "\n")
end

val () = AnagraphMain.go ()
