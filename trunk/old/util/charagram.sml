
(* reads lines from stdin.
   Counts the number of times each line appears.
   Doesn't actually draw a histogram yet. Maybe some day.

   With mlton, compile as:

   mlton charagram.sml

   Pipe to "sort -n" to sort the output by frequency.

*)

fun pad n s =
    if (size s >= n) then s
    else (s ^ CharVector.tabulate (n - size s, fn _ => #" "))

fun go entries =
    let
        val tab = Array.array (256, 0)

        fun report () =
            let in
	      Array.appi
	            (fn (x, n) =>
		     if n > 0
		     then 
			 print (pad 10 (Int.toString n ^ ": ") ^
				pad 3  (Int.toString x) ^
				(if Char.isPrint (chr x)
				 then implode[#" ", #"(", chr x, #")"]
				 else "") ^
				"\n")
		     else ())
		    (tab, 0, NONE)
            end

        fun loop () =
            case TextIO.input1 (TextIO.stdIn) of
                NONE => report ()
              | SOME c => 
                let in
		  Array.update(tab, ord c, Array.sub(tab, ord c) + 1);
		  loop ()
                end
    in
        loop ()
    end

fun usage () =
    let in
        print "charagram: prints a histogram of characters on stdin.\n";
        print "\n"
    end

val _ = 
    case CommandLine.arguments () of
        nil => go ()
      | _ => usage ()

