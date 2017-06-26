
structure Blam =
struct

  exception Blam of string

  val normalize = Params.flag false (SOME("-normalize",
                                          "normalize similarity based on on the gene length"))
                                    "normalize"
    
  val parity = Params.flag false (SOME("-parity",
                                       "report scores as negative if direction is mismatched"))
                                 "parity"

  val minimum = Params.param "0" (SOME ("-minimum",
                                        "minimum similarity to report")) "minimum"

  val dos = Params.flag false (SOME("-dos",
                                    "use DOS line endings (cr/lf) in output")) "dos"

  (* currently forced -- there is no ML engine that uses the PAM matrix *)
(*
  val gpsac = Params.flag false (SOME("-c",
                                      "use C gpsa engine")) "gpsac"
*)

  val amino = Params.flag false (SOME("-amino",
                                      "compare amino acids rather than nucleotides")) "amino"

  (* this is just a performance tweak
     which doesn't make much difference in my tests *)
  val longfirst = Params.flag false (SOME ("-longfirst",
                                           "put the long sequence on the X axis for alignment")) 
                    "longfirst"

  fun itos n =
      if n < 0 then "-" ^ Int.toString (~ n) else Int.toString n

  (* 20x20 (symmetric) array of amino acid similarity scores.

     XXX check typos: is this symmetric?

     Extended to 23x23 to account for degenerate acids. 
     b = d or n  (here I use the d column unless d and n differ
                  by more than 1, in which case I average their 
                  scores)
     z = e or q  (same idea, favoring column e)
     x = any     (no bonus or penalty except when matching another x)

     XXX how to handle u = selenocysteine ("like cysteine") ??

     see http://helix.biology.mcmaster.ca/721/distance/node9.html *)
  val matrix_PAM250 =
    ((* radix *) 
     23,
     (* data *)
    Array.fromList
        (* a   c   d   e   f   g   h   i   k   l   m   n   p   q   r   s   t   v   w   y     b   z   x *)
(* a *) [  2, ~2,  0,  0, ~4,  1, ~1, ~1, ~1, ~2, ~1,  0,  1,  0, ~2,  1,  1,  0, ~6, ~3,    0,  0,  0,
(* c *)   ~2, 12, ~5, ~5, ~4, ~3, ~3, ~2, ~5, ~6, ~5, ~4, ~3, ~5, ~4,  0, ~2, ~2, ~8,  0,   ~5, ~5,  0,
(* d *)    0, ~5,  4,  3, ~6,  1,  1, ~2,  0, ~4, ~3,  2, ~1,  2, ~1,  0,  0, ~2, ~7, ~4,    3,  3,  0,
(* e *)    0, ~5,  3,  4, ~5,  0,  1, ~2,  0, ~3, ~2,  1, ~1,  2, ~1,  0,  0, ~2, ~7, ~4,    2,  3,  0,
(* f *)   ~4, ~4, ~6, ~5,  9, ~5, ~2,  1, ~5,  2,  0, ~4, ~5, ~5, ~4, ~3, ~3, ~1,  0,  7,   ~5, ~5,  0,
(* g *)    1, ~3,  1,  0, ~5,  5, ~2, ~3, ~2, ~4, ~3,  0, ~1, ~1, ~2,  1,  0, ~1, ~7, ~5,    1,  0,  0,
(* h *)   ~1, ~3,  1,  1, ~2, ~2,  6, ~2,  0, ~2, ~2,  2,  0,  3,  1, ~1, ~1, ~2, ~3,  0,    1,  2,  0,
(* i *)   ~1, ~2, ~2, ~2,  1, ~3, ~2,  5, ~2,  2,  2, ~2, ~2, ~2, ~2, ~1,  0,  4, ~5, ~1,   ~2, ~2,  0,
(* k *)   ~1, ~5,  0,  0, ~5, ~2,  0, ~2,  5, ~3,  0,  1, ~1,  1,  3,  0,  0, ~2, ~3, ~4,    0,  0,  0,
(* l *)   ~2, ~6, ~4, ~3,  2, ~4, ~2,  2, ~3,  6,  4, ~3, ~3, ~2, ~3, ~3, ~2,  2, ~2, ~1,   ~4, ~3,  0,
(* m *)   ~1, ~5, ~3, ~2,  0, ~3, ~2,  2,  0,  4,  6, ~2, ~2, ~1,  0, ~2, ~1,  1, ~4, ~2,   ~3, ~2,  0,
(* n *)    0, ~4,  2,  1, ~4,  0,  2, ~2,  1, ~3, ~2,  2, ~1,  1,  0,  1,  0, ~2, ~4, ~2,    2,  1,  0,
(* p *)    1, ~3, ~1, ~1, ~5, ~1,  0, ~2, ~1, ~3, ~2, ~1,  6,  0,  0,  1,  0, ~1, ~6, ~5,   ~1, ~1,  0,
(* q *)    0, ~5,  2,  2, ~5, ~1,  3, ~2,  1, ~2, ~1,  1,  0,  4,  1, ~1, ~1, ~2, ~5, ~4,    2,  3,  0,
(* r *)   ~2, ~4, ~1, ~1, ~4, ~3,  2, ~2,  3, ~3,  0,  0,  0,  1,  6,  0, ~1, ~2,  2, ~4,   ~1,  0,  0,
(* s *)    1,  0,  0,  0, ~3,  1, ~1, ~1,  0, ~3, ~2,  1,  1, ~1,  0,  2,  1, ~1, ~2, ~3,    0,  0,  0,
(* t *)    1, ~2,  0,  0, ~3,  0, ~1,  0,  0, ~2, ~1,  0,  0, ~1, ~1,  1,  3,  0, ~5, ~3,    0,  0,  0,
(* v *)    0, ~2, ~2, ~2, ~1, ~1, ~2,  4, ~2,  2,  2, ~2, ~1, ~2, ~2, ~1,  0,  5, ~6, ~2,   ~2, ~2,  0,
(* w *)   ~6, ~8, ~7, ~7,  0, ~7, ~3, ~5, ~3, ~2, ~4, ~4, ~6, ~5,  2, ~2, ~5, ~6, 17,  0,   ~5, ~6,  0,
(* y *)   ~3,  0, ~4, ~4,  7, ~5,  0, ~1, ~4, ~1, ~2, ~2, ~5, ~4, ~4, ~3, ~3, ~2,  0, 10,   ~3, ~4,  0,

(* b *)    0, ~5,  3,  2, ~5,  1,  1, ~2,  0, ~4, ~3,  2, ~1,  2, ~1,  0,  0, ~2, ~5, ~3,    3,  2,  0,
(* z *)    0, ~5,  3,  3, ~5,  0,  2, ~2,  0, ~3, ~2,  1, ~1,  3,  0,  0,  0, ~2, ~6, ~4,    2,  3,  0,
(* x *)    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  3 ],
    (* gap score *) ~8)

  (* right now just print *)
  fun output s = print s
  fun error s = TextIO.output (TextIO.stdErr, s)

  (* status reporting *)
  local 

    val pruned = ref 0
    val total = ref 0
    val done = ref 0
    val hits = ref 0
    val lastfrac = ref 0
    val lasttime = ref (Time.now ())
    val begin = ref (Time.now ())

    fun twodig n =
        if n < 10 then "0" ^ Int.toString n
        else Int.toString n

    fun seconds n =
       ((case n div 60 of
             0 => ""
           | mins => 
         ((case mins div 60 of
               0 => ""
             | hours => 
            ((case hours div 24 of
                 0 => ""
               | days => Int.toString days)
                  ^ ":" ^ twodig (hours mod 24))
               ) ^ ":" ^ twodig (mins mod 60))
               ) ^ ":" ^ twodig (n mod 60))
  in
    fun pruneone () = pruned := !pruned + 1

    fun settotal n = 
      let in
        total := n;
        begin := Time.now ()
      end

    fun hit () =
      let in
        hits := !hits + 1;
        done := !done + 1
      end

    fun miss () =
      let in
        done := !done + 1
      end

    fun report force =
      let 
        val ffrac = real (!done) / real (!total)
        val frac = Real.trunc (100.0 * ffrac)
        val n = Time.now ()
      in
        if force orelse frac > !lastfrac 
           orelse (Time.> (Time.- (n, !lasttime),
                           Time.fromSeconds 60))
        then
          let 
          in
            lastfrac := frac;

            (* but there must be a significant amount of
               time, otherwise short results are reported
               too often *)
            if (force orelse 
                Time.> (Time.- (n, !lasttime),
                        Time.fromSeconds 3))
            then 
              let 
                val exptotal =
                  (1.0 / ffrac) *
                  (Real.fromLargeInt (Time.toSeconds (Time.- (n, !begin))))
              in
                error
                (Int.toString frac ^ "% (" ^ Int.toString (!done) ^ " / " ^
                 Int.toString (!total) ^ ") " ^ Int.toString (!hits) ^ " hits, " ^
                 Int.toString (!pruned) ^ " pruned    " ^

                 "ETA: " ^ 
                 seconds (Real.trunc ((1.0 - ffrac) * exptotal)) ^
                 "\n");
                lasttime := n
              end
            else ()
          end
        else ()
      end

  end

  datatype range =
    Complement of range
  | Range of int * int

  (* put amino acids in the character range 0 .. 20. *)
  val acidval =
     (fn #"a" => 0
       | #"c" => 1
       | #"d" => 2
       | #"e" => 3
       | #"f" => 4
       | #"g" => 5
       | #"h" => 6
       | #"i" => 7
       | #"k" => 8
       | #"l" => 9
       | #"m" => 10
       | #"n" => 11
       | #"p" => 12
       | #"q" => 13
       | #"r" => 14
       | #"s" => 15
       | #"t" => 16
       | #"v" => 17
       | #"w" => 18
       | #"y" => 19
      (* degenerate amino acids *)
       | #"b" => 20
       | #"z" => 21
       | #"x" => 22
       | c => 
      let in
        error ("Bad amino acid " ^ implode [c] ^ "!\n");
        raise Blam "Bad amino acid"
      end)

  fun root_acids s =
      String.map (chr o acidval o Char.toLower) s

  fun read f =
    let
      val (secs, ntides) = 
        Parse.parsefile (fn "cds" => true
      | "version" => true
      | "organism" => true
      | _ => false) f

      fun find s = 
        Array.foldl (fn ((ss, lines), NONE) =>
                     if s = ss 
                     then SOME (StringUtil.delimit " " lines)
                     else NONE
                       | (_, a) => a) NONE secs

      val cds = GrowArray.empty ()
        
      fun parserange s =
        let
           open Parsing
            infixr 4 << >>
            infixr 3 &&
            infix  2 -- ##
            infix  2 wth suchthat return guard when
            infixr 1 ||

            val digit = satisfy (StringUtil.charspec "0-9")
            val number = repeat1 digit when (Int.fromString o implode)
            fun range () =
              alt [string (explode "complement(")
                     >> $range << literal #")"
                     wth Complement,
                   number && (literal #"." && literal #".") >> number
                     wth Range]
        in
          Parsing.parse ($range) (Pos.markstream (StreamUtil.stostream s))
        end

      fun findc s lines =
        let

            open Parsing
            infixr 4 << >>
            infixr 3 &&
            infix  2 -- ##
            infix  2 wth suchthat return guard when
            infixr 1 ||


            (* match a string, case insensitive *)
            fun ? s =
              let
                val ss = size s
                fun next n =
                  if n >= ss 
                  then succeed ()
                  else satisfy (fn c => 
                                Char.toLower c = 
                                Char.toLower (CharVector.sub(s, n))) -- 
                    (fn _ => next (n + 1))
              in
                next 0
              end

            (* double quote escapes itself *)
            val quoted =
              literal #"\"" >> 
                 (repeat (alt [satisfy (fn c => c <> #"\""),
                              literal #"\"" >> literal #"\"" return #"\""])
                        wth implode)
              << literal #"\""

            fun record () =
              literal #"/" >> ?s >> literal #"="
              >> quoted 
        in
          ListUtil.findpartial 
          (fn l =>
           Parsing.parse ($record) (Pos.markstream (StreamUtil.stostream l))) lines
        end

      fun addcds ("cds", range :: lines) =
        (case parserange range of
           NONE =>
             let in
               error ("(" ^ f ^ ") Unparseable range '" ^ range ^ "' in CDS\n");
               ()
             end
         | SOME r => 
             let

               (* quoted items can annoyingly span lines. 
                  put these all on one *)
               fun mergelines nil = nil
                 | mergelines (s::t) =
                 let
                   (* return a list of strings that should be
                      concatenated, along with the remaining lines *)
                   fun look_quote n =
                     if n = size s 
                     then ([s], t) (* no quote found *)
                     else if CharVector.sub(s, n) = #"\"" (* " *)
                          then consume_quoted s t (n + 1)
                          else look_quote (n + 1)

                   and consume_quoted s ll n =
                     if n = size s
                     then (* end of line *)
                       (case ll of
                          nil =>
                            let in
                              error "Unterminated quote in CDS!\n";
                              raise Blam "can't parse"
                            end
                        | (next::rest) =>
                            let
                              val (sr, tt) =
                                consume_quoted next rest 0
                            in
                              (s::sr, tt)
                            end)
                     else
                       (if CharVector.sub(s, n) = #"\""
                        then 
                          (* might be end of string, or escape *)
                          (if n <> (size s - 1) andalso
                              CharVector.sub(s, n + 1) = #"\"" (* " *)
                           then (* escape *)
                             let 
                               val (sr, tt) =
                                 consume_quoted
                                 (String.substring(s, n + 2, size s - (n + 2)))
                                 ll 0
                             in
                               (String.substring(s, 0, n) :: "\"" :: sr, tt) (* " *)
                             end
                           else (* end! *)
                             ([s], ll))
                        else consume_quoted s ll (n + 1))

                    val (thisline, remains) = look_quote 0
                 in
                   String.concat(thisline) :: mergelines remains
                 end

               val lines = mergelines lines

               val aa = 
                   (Option.map root_acids (findc "translation" lines))
                   handle _ => NONE
               val gg = 
                 getOpt (findc "gene" lines, 
                         getOpt (findc "locus_tag" lines,
                                 "?? " ^ range))
             in
               if isSome aa orelse not (!amino)
               then 
                 (* rep invt.: if amino mode is on,
                    the acids field must be filled! *)
                 GrowArray.append cds 
                 { gene = gg,
                   acids = aa,
                   range = r }
               else error ("(" ^ f ^ ") No translation for gene '" ^ gg ^ "'\n")
             end)
        | addcds _ = ()

    in
      Array.app addcds secs;

      (* XXX 
         should normalize ntide case at some point, if it is
         not guaranteed *)
      { organism = getOpt (find "organism", "?? " ^ f),
        version = getOpt (find "version", "?? " ^ f),
        cds = GrowArray.finalize cds,
        ntides = ntides }
    end

  (* XXX check; http://en.wikipedia.org/wiki/Nucleotide *)
  (* XXX u? *)
  fun completide t =
    case t of
      #"a" => #"t"
    | #"c" => #"g"
    | #"g" => #"c"
    | #"t" => #"a"
      (* these are 'meta-nucleotides'
         standing for several possibilities *)
    | #"m" => #"k"
    | #"r" => #"y"
    | #"w" => #"w"
    | #"s" => #"s"
    | #"y" => #"r"
    | #"k" => #"m"
    | #"v" => #"b"
    | #"h" => #"d"
    | #"d" => #"h"
    | #"b" => #"v"
    | #"x" => #"x"
    | z => z (* XXX ??? bad 'tide *)

  fun blam (a, b) =
    let
      val newline = (if !dos
                      then "\r\n"
                      else "\n")


      val s1 as { cds = c1, ntides = nt1, organism=o1, ... } = read a
      val s2 as { cds = c2, ntides = nt2, organism=o2, ... } = read b

      val n1 = Array.length c1
      val n2 = Array.length c2

      fun getn s (Complement (Range(lo, hi))) =
        CharVector.tabulate (hi - lo + 1,
                             (fn x =>
                              completide 
                              (CharArray.sub
                               (s, (hi - 1) - x))))
                                                        
        | getn s (Range (lo, hi)) = 
        CharVector.tabulate (hi - lo + 1,
                             (fn x => CharArray.sub(s, lo - 1 + x)))
        | getn s _ = raise Blam "bug: expected range or complement(range)"

      fun get s a r = if !amino then valOf a else getn s r

      datatype dir = Forward | Backward | Unknown

      fun orientation (Complement (Range _)) = Backward
        | orientation (Range _) = Forward
        | orientation _ = Unknown

      fun otos Backward = "<-"
        | otos Forward  = "->"
        | otos Unknown  = "?!"

      val minimum = Params.asint 0 minimum

      (* normalize a score with respect to the length of the sequences *)
      fun donorm s1 s2 r =
          if !normalize
          then Real.trunc ((real r * 100.0) / ((real (size s1) + real (size s2)) / 2.0))
          else r
                  
      val (alignfn, prunefn) =
          let

              val (cmp, maxscore, gapscore) =
                  if !amino
                  then (GPSA_C.bestalignment_mtx matrix_PAM250,
                        17, ~8)
                  else (GPSA_C.bestalignment, 4, ~4)

(*
                (if !gpsac
                 then GPSA_C.bestalignment (if !longfirst then (s2, s1) else (s1, s2))
                 else GPSA.bestalignment   (if !longfirst then (s2, s1) else (s1, s2)))
*)

             (* pruning is an important optimization: we should avoid computing
                the alignment score of sequences that can't possibly meet our
                threshold.

                we know the score can't be positive if gapscore * (large - small) 
                (which is the penalty we incur for making the sequences the same
                length) is larger than min(large, small) * maxscore (which is the
                best possible similarity we can achieve from the rest). *)

              (* PERF:

                 the entries in the PAM250 matrix that score 17 and 12 (by far the
                 highest) are probably rare amino acids. We can only score 17 as
                 many times as those characters appear. If they are rare, counting
                 chars here would help us prune more often. *)

              (* w is the rarest *)
              val wval = chr(acidval #"w")

              (* assumes |s1| < |s2| *)
              fun pf_normal s1 s2 = 
                 (* best possible score *)
                 let
                   val sn1 = size s1
                   val sn2 = size s2
               
                   val absolute = (sn2 - sn1) * gapscore + sn1 * maxscore 
                 in
                   donorm s1 s2 absolute
                 end
                 < minimum


              (* assumes |s1| < |s2| *)
              fun pf_pam s1 s2 = 
                 (* best possible score *)
                 let
                   val sn1 = size s1
                   val sn2 = size s2

                   fun countz s n acc =
                       if n >= size s
                       then acc
                       else countz s (n + 1)
                           (if CharVector.sub(s, n) = wval
                            then acc + 1
                            else acc)

                   val z = Int.min(countz s1 0 0, countz s2 0 0)

                   (* best possible score from matching zs *)
                   val bonus = z * 17
                       
                   (* now the second highest is only 12 *)
                   val maxscore = 12

                   (* can't double count *)
                   val sn1 = sn1 - z
                   val sn2 = sn2 - z
                       
                   (* this is what's left *)
                   val absolute = (sn2 - sn1) * gapscore + sn1 * maxscore 
                 in
                     donorm s1 s2 (absolute + bonus)
                 end
                 < minimum
          in
              (cmp, if !amino then pf_pam else pf_normal)
          end


      fun nocomma s = String.map (fn #"," => #"." | c => c) s
          
      (* print a gene as genename,startpos,endpos
         startpos will come after endpos if the region is complemented! *)

      fun gtos { range = Complement (Range(lo, hi)), gene, ... } =
          nocomma gene ^ "," ^ itos hi ^ "," ^ itos lo
        | gtos { range = Range(lo, hi), gene, ... } =
          nocomma gene ^ "," ^ itos lo ^ "," ^ itos hi
        | gtos _ = raise Blam "gtos expected range() or complement(range())"

      fun output_org (nn, cc) =
          let in
              print ("!genes " ^ Int.toString nn ^ "\n");
              Array.app (fn g => print (gtos g ^ ",\n")) cc
          end


      val opts =
          (if !normalize then " normalize" else "")
              ^ (if minimum > 0 
                 then (" minimum " ^ Int.toString minimum) else "")
              ^ (if !amino then " amino" else "")
              ^ (if !parity then " parity" else "")
              ^ (if !longfirst then " longfirst" else "")
    in
      output (o2 ^ newline);
      output (o1 ^ newline);

      error  ("  options: " ^ opts ^ "\n");
      output ("!options " ^ opts ^ newline);

      (* output genes first *)
      output_org (n1, c1);
      output_org (n2, c2);

      print "!comparison\n";
      

      (* set up status messages *)
      settotal (n1 * n2);
      
      Util.for 0 (n2 - 1)
      (fn x2 =>
       let 
           val (gene2 as { range=r2, gene=_, acids=a2 }) = Array.sub(c2, x2)
           val matched = ref false

           fun maybestart () =
               if !matched then ()
               else
                   let in
                       matched := true;
                       output (gtos gene2)
                   end
       in
         (* start by printing vertical gene and its position *)

         Util.for 0 (n1 - 1)
         (fn x1 =>
          let
            val (gene1 as { range=r1, gene=_, acids=a1 }) = Array.sub(c1, x1)

            (* arrange that the larger sequence is always s2;
               the alignment score is symmetric *)
            val (s1, s2) = 
              let 
                val a = get nt1 a1 r1
                val b = get nt2 a2 r2
              in
                if size a > size b 
                then (b, a)
                else (a, b)
              end

            val r = 
              if prunefn s1 s2
              then
                  let in
                      pruneone ();
                      ~1000
                  end
              else alignfn (if !longfirst then (s2, s1) else (s1, s2))

            val r = donorm s1 s2 r 

            val st = if r >= minimum
                     then 
                         SOME (hit ();
                               if !parity
                               then (if orientation r1 = orientation r2
                                     then Int.toString r
                                     else "-" ^ Int.toString r)
                               else Int.toString r)
                     else 
                       (miss (); NONE)
          in
              case st of
                  NONE => () (* do nothing *)
                | SOME sss => (maybestart ();
                               output ("," ^ gtos gene1 ^ "," ^ sss))
          end);

         report false;


         (* and always end with a comma if we printed anything *)
         if !matched then output ("," ^ newline)
         else ()
       end);

      report true
    end

end