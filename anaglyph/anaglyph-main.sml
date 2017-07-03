structure AnaglyphMain =
struct

  val dump = Params.flag false
    (SOME ("-dump", "Dump the canonized dictionary.")) "dump"

  val plan = Params.flag false
    (SOME ("-plan", "Plan an animation between phrase1 and phrase2")) "plan"

  fun main args =
    let in
      if !dump
      then StringUtil.writefile "canonized.txt" (Anaglyph.canonized_file ())
      else ();

      if !plan
      then
        (case args of
           [phrase1, phrase2] => Anaglyph.makeplan (phrase1, phrase2)
         | _ => raise Anaglyph.Anaglyph "-plan needs exactly two args.")
      else ();

      ()
    end

  fun go () =
    (Params.main
     "Give a phrase to anagram, or a pair of phrases to plan, etc."
     main)
    handle Anaglyph.Anaglyph s => print ("Anaglyph: " ^ s ^ "\n")
end

val () = AnaglyphMain.go ()