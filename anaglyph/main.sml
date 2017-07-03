structure AnaglyphMain =
struct

  val dump = Params.flag false
    (SOME ("-dump", "Dump the canonized dictionary.")) "dump"

  fun main args =
    let in
      if !dump
      then StringUtil.writefile "canonized.txt" (Anaglyph.canonized_file ())
      else ();

      ()
    end

  fun go () = Params.main
    "Give a phrase to anagram, or a pair of phrases to plan, etc."
     main
end

val () = AnaglyphMain.go ()