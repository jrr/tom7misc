
(* read files from the command line.
   print out a filename if it has any line
   longer than a certain width. *)

structure Linelen = 
struct

    val lenparam = Params.param "80"
	(SOME("-w", "The width at which to warn.")) "len"

    val tabparam = Params.param "8"
	(SOME("-t", "How long tabs should be considered to be.")) "tab"

    val showlen = Params.flag true
	(SOME("-l", "Show the length of wide files.")) "showlen"


    fun main nil = ()
      | main (f::rest) =
	let
	    val ff = TextIO.openIn f

	    fun max (a, b) = if a < b then b else a

	    fun maxlen mx cur =
		(case TextIO.input1 ff of
		     NONE => mx
		   | SOME #"\n" => maxlen (max(mx, cur)) 0
		   | SOME #"\r" => maxlen (max(mx, cur)) 0
		   | SOME #"\t" => maxlen mx (cur + 
					      Params.asint 8 tabparam)
		   | SOME c => maxlen mx (cur + 1))

	    val l = maxlen 0 0
	in
	    if l > (Params.asint 80 lenparam)
	    then print (f ^ 
			(if !showlen 
			 then (" : " ^ Int.toString l)
			 else "") ^
			"\n")
	    else ();

	    TextIO.closeIn ff;
	    main rest
	end handle _ =>
	    let in
		(* Warn: couldn't open f *)
		main rest
	    end

end

val _ = 
    case Params.docommandline () of
	nil =>
	    let in
		print "linelen: lists files with lines too long\n\n";
		print (Params.usage ())
	    end
      | l => Linelen.main l
