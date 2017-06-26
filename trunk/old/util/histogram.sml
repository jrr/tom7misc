
(* reads lines from stdin.
   Counts the number of times each line appears.
   Doesn't actually draw a histogram yet. Maybe some day.

   With mlton, compile as:

   mlton histogram.sml

   Pipe to "sort -n" to sort the output by frequency.

*)

fun pad n s =
    if (size s >= n) then s
    else (s ^ CharVector.tabulate (n - size s, fn _ => #" "))

fun truncate l s = 
    if size s > l then String.substring(s, 0, l)
    else s

fun go entries =
    let
        val tab = Array.array (entries, nil)

        fun hash s =
            let 
                fun hh w (~1) = w
                  | hh w n = hh 
                    (let open Word32 infix xorb
                     in 0wxFACE4017 xorb 
                         ((w * 0w1331 * (fromInt n)) xorb 
                          (fromInt (ord (CharVector.sub(s, n)))))
                     end) (n - 1)
            in
                hh 0w0 (size s - 1)
            end

        fun upgrade nil s = [(s, 1)]
          | upgrade ((head as (h,n))::t) s = if h = s 
                                             then (h, n + 1) :: t
                                             else head :: upgrade t s

        fun incr s =
            let
                val idx = Word32.toIntX(Word32.andb 
                                        (hash s, 0wx07FFFFFF)) 
                          mod entries
                val l = Array.sub(tab, idx)
            in
                Array.update(tab, idx, upgrade l s)
            end

        fun report () =
            let val l = Array.foldl op@ nil tab
            in
                app (fn (str, n) =>
                     print (pad 10 (Int.toString n ^ ": ") ^ 
                            truncate 60 str ^ "\n")) l
            end

        fun loop () =
            case TextIO.inputLine (TextIO.stdIn) of
                "" => report ()
              | line => 
                let in
                    incr (String.substring(line, 0, size line - 1));
                    loop ()
                end
    in
        loop ()
    end

fun usage () =
    let in
        print "histogram: prints a histogram of lines on stdin.\n";
        print "\n";
        print ("Optionally provide an integer estimate of the number " ^
               "of possible input lines.\n");
        print "\n"
    end

val _ = 
    case CommandLine.arguments () of
        nil => go 10000
      | [n] => 
            (case Int.fromString n of
                 SOME n => go n
               | NONE => usage ())
      | _ => usage ()

