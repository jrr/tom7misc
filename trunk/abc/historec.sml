structure Histo =
struct
  infixr 9 `
  fun a ` b = a b

  structure MT = MersenneTwister

  (* First character in last page. *)
  val LASTPAGE = 19 * 128 * 160
  val ROWSTART = 9
  val COLSTART = 21

  (* Place to insert convergence help. *)
  val CONVERGEPOS = LASTPAGE + 108 * 160 + 20

  (*  val MAX_CONV = 102*)
  val MAX_CONV = 10

  exception Histo of string

  fun for_converge f =
    Util.for CONVERGEPOS (CONVERGEPOS + MAX_CONV) f

  fun for_column f =
    Util.for 0 (0x7e - 0x20)
    (fn row =>
     Util.for 0 5
     (fn col =>
      f (LASTPAGE + (row + ROWSTART) * 160 + (col + COLSTART))))

  datatype digit = DIGIT of int | SPACE | NO
  fun getdigit 0x20 = SPACE
    | getdigit idx = if idx >= ord #"0" andalso idx <= ord #"9"
                     then DIGIT (idx - ord #"0") else NO

  fun go filename =
    let
      val mt = MersenneTwister.initstring "historec"

      (* Read the whole file *)
      val contents = StringUtil.readfile filename
      val contents = Word8Array.tabulate
        (128 * 160 * 20,
         fn i =>
         Word8.fromInt ` ord `String.sub (contents, i))

      fun writestring pos s =
        Util.for 0 (size s - 1)
        (fn ri =>
         let val i = pos + ri
         in Word8Array.update (contents, i,
                               Word8.fromInt ` ord ` String.sub(s, ri))
         end)


      (* Blank out the region that we overwrite, so that we can assume
         it's all spaces. *)
      fun blank i = Word8Array.update (contents, i, 0wx20)
      val () = for_column blank
      val () = for_converge blank

      (* Now, compute the base histogram (for the whole file). *)
      val start_histo =
        let
          val arr = Array.array (256, 0)
          fun onec c =
            let val idx = Word8.toInt c
            in Array.update (arr, idx, Array.sub (arr, idx) + 1)
            end
        in
          Word8Array.app onec contents;
          arr
        end

      (* What we need to do is compute a delta on the start_histo
         such that the new values for start_histo, when inserted
         into the document, will be accurate. These deltas should
         be nonnegative for digits and nonpositive for spaces.
         Since the total number of characters in the document always
         stays the same, the vector should sum to 0.

         For a given delta (proposed modification to the histogram
         _values_) and conv (arbitrary sequence to insert, replacing
         spaces), this function computes the actual effect of
         the change (ground truth histogram). So if the delta =
         change, then we've solved it. *)
      fun getchange (delta, conv) =
        let
          (* Compute the error that we would cause in the
             histogram from inserting it as text. This only
             affects digits and space. *)
          val err = Array.array (11, 0)
          fun digit c =
            let
              val d = ord c - ord #"0"
            in
              Array.update (err, d, Array.sub (err, d) + 1)
            end
        in
          Util.for 0x20 0x7e
          (fn idx =>
           let
             val base_count = Array.sub (start_histo, idx)
             (* Modify the count with the proposed delta. *)
             val count =
               case getdigit idx of
                 NO => base_count
               | DIGIT d => base_count + Array.sub (delta, d)
               | SPACE => base_count + Array.sub (delta, 10)

             val () = if count < 0
                      then raise Histo ("Count for " ^ Int.toString idx ^
                                        " (" ^ implode [chr idx] ^
                                        ") cannot be negative? " ^
                                        Int.toString count)
                      else ()
             (* String version of the count. *)
             val s = Int.toString count
           in
             CharVector.app digit s;
             (* Any digit displaces a space. *)
             Array.update (err, 10, Array.sub (err, 10) - size s)
           end);
          (* Same, but this is just one big number. *)
          CharVector.app digit conv;
          Array.update (err, 10, Array.sub (err, 10) - size conv);
          err
        end

      fun randomdigits () =
        let val len = MT.random_nat mt 9
        in
          CharVector.tabulate (len, fn _ =>
                               chr (MT.random_nat mt 9 + ord #"0"))
        end

      fun loop (delta, conv) =
        if size conv > MAX_CONV
        then
          let in
            print "Conv got too big.\n";
            loop (delta, randomdigits ())
          end
        else
        let
          val () = print "Delta:  "
          val () = Array.app (fn i => print (Int.toString i ^ " ")) delta
          val () = print ("\t conv: " ^ conv ^ "\n")

          val dtot = Array.foldl op+ 0 delta
          val () =
            if dtot <> 0
            then raise Histo ("Delta total should be 0, but got " ^
                              Int.toString dtot)
            else ();

          val change = getchange (delta, conv)
          (* val changetot = Array.foldl op+ 0 change *)

          val () = print "Change: "
          val () = Array.app (fn i => print (Int.toString i ^ " ")) change
          val () = print "\n"

          val total_error = ref 0
          val error = Array.tabulate (11,
                                     fn i =>
                                     let val diff =
                                       Array.sub (delta, i) -
                                       Array.sub (change, i)
                                     in
                                       total_error := !total_error +
                                           Int.abs diff;
                                       diff
                                     end)
          val () =
            let in
              print "Error:  ";
              Array.app (fn i => print (Int.toString i ^ " ")) error;
              print "\n"
            end

        in
          if !total_error = 0
          then
            let in
              print "This shall work!\n";
              Array.app (fn i => print (Int.toString i ^ " ")) change;
              print "\n";

              (* Actually write the histogram. *)
              Util.for 0x20 0x7e
              (fn i =>
               let
                 val count = Array.sub (start_histo, i)
                 val count =
                   (case getdigit i of
                      NO => count
                    | SPACE => count + Array.sub (change, 10)
                    | DIGIT d => count + Array.sub (change, d))
                 val s = Int.toString count

                 val row = ROWSTART + (i - 0x20)
               in
                 writestring (LASTPAGE + row * 160 + COLSTART) s
               end);

              (* Update convergence string *)
              writestring CONVERGEPOS conv;

              StringUtil.writefilev8 filename (Word8Array.vector contents)
            end
          else
            let
              fun fixup () =
                let
                  (* Temp copies. *)
                  val error = Array.tabulate (Array.length error,
                                              fn i => Array.sub (error, i))
                  val change = Array.tabulate (Array.length change,
                                               fn i => Array.sub (change, i))
                  (* If we can, fix up the error in place by modifying conv. *)
                  val newdigits = Array.array (10, 0)
                  val () = CharVector.app
                    (fn d =>
                     case getdigit ` ord d of
                       NO => ()
                     | SPACE => () (* should be impossible? *)
                     | DIGIT d => Array.update (newdigits, d,
                                                Array.sub (newdigits, d) + 1))
                    conv

                  (* Now we can adjust newdigits:
                     If we have negative error for any slot, add digits.
                     If we have positive error and at least that many
                     digits, remove them. *)

                  val () =
                    Util.for 0 9
                    (fn d =>
                     let
                       val conv_count = Array.sub(newdigits, d)
                       val err = Array.sub (error, d)
                     in
                       if err = 0
                       then ()
                       else if err < 0
                            then
                              let in
                                (* Too few digits. Just add them. *)
                                Array.update (newdigits, d, conv_count - err);
                                Array.update (change, d,
                                              Array.sub (change, d) + err);
                                (* Reduce the number of spaces. *)
                                Array.update (change, 10,
                                              Array.sub (change, 10) - err)
                              end
                            else
                              let
                                (* Too many digits. Remove as many as
                                   we can. *)
                                val num = Int.min (conv_count, err)
                              in
                                if num < 0 then raise Histo "??"
                                else ();
                                Array.update (newdigits, d, conv_count - num);
                                Array.update (change, d,
                                              Array.sub (change, d) - num);
                                Array.update (change, 10,
                                              Array.sub (change, 10) + num)
                              end
                     end)

                  val conv = ref ""
                  val () =
                    Util.for 0 9
                    (fn d =>
                     let val ct = Array.sub (newdigits, d)
                     in
                       if ct > 0
                       then conv := !conv ^
                         CharVector.tabulate (ct, fn _ => chr (ord #"0" + d))
                       else ()
                     end)
                  val conv = !conv
                  val () = print ("New conv of " ^ conv ^ "\n")
                in
                  (change, conv)
                end

              val (new_change, new_conv) = fixup ()
            in
              if size new_conv < MAX_CONV
              then
                loop (new_change, new_conv)
              else
                loop (change, if MT.random_nat mt 6 = 0
                              then randomdigits ()
                              else conv)
            end
        end
    in
      loop (Array.array (11, 0), randomdigits ())
    end

end

val () = Params.main1 "histo.exe input_file" Histo.go
       handle e as Histo.Histo s =>
         let in
           TextIO.output(TextIO.stdErr, s ^ "\n");
           raise e
         end
