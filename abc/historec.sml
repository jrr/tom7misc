structure Histo =
struct
  infixr 9 `
  fun a ` b = a b

  (* First character in last page. *)
  val LASTPAGE = 19 * 128 * 160
  val ROWSTART = 8
  val COLSTART = 21

  (* Place to insert convergence help. *)
  val CONVERGEPOS = LASTPAGE + 108 * 160 + 20

  exception Histo of string

  fun for_converge f =
    Util.for CONVERGEPOS (CONVERGEPOS + 102) f

  fun for_column f =
    Util.for 0 (0x7e - 0x20 + 1)
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
      (* Read the whole file *)
      val contents = StringUtil.readfile filename
      val contents = Word8Array.tabulate (128 * 160 * 20,
                                          fn i =>
                                          Word8.fromInt ` ord `String.sub (contents, i))


      (* Blank out the region that we overwrite, so that we can assume it's all spaces. *)
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
         be positive for digits and negative for spaces. *)
      fun error (delta, conv)  =
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


      (* What we're doing here is solving for a histogram that
         we can place into the paper, satisfying its own contents.
         We pair that with a string that we can insert in the
         "convergence" section, since there may be no solution
         otherwise.

         What we do is take a histogram (maps printable chars
         to counts); this doesn't have to correspond to anything,
         but it should be correct for characters other than space
         and digits, because we don't have any opportunity to
         fix that. We then compute its error, which is how it
         would affect the histogram itself to insert it into
         the paper in text. *)

      (* Takes a histogram, which is a count for each character
         (across the whole file). *)
(*
      fun delta (histo, conv)  =
        let
          (* Compute the delta that we would cause in the
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
             val count = Array.sub (histo, idx)
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
*)
      (* OK, now we have a vector of counts representing the file
         without the column of numbers for the histogram counts,
         and without the convergence string. What we want to do
         is compute a column and a convergence string that
         "solves the equation"; where the sum of character counts
         within the rectangles (minus the spaces displaced)
         is consistent with the contents of the first rectangle.
         (the second rectangle can be anything). *)

      fun loop () =
        let
          val dt = delta (start_histo, "")
          val dtot = Array.foldl op+ 0 dt
        in
          if dtot <> 0
          then raise Histo ("Delta total should be 0, but got " ^ Int.toString dtot)
          else ();

          print "Start histo:\n";
          Util.for 0x20 0x7e
          (fn i =>
           let
             val count = Array.sub (start_histo, i)
           in
             print ("        " ^ implode [chr i] ^ "   0x" ^
                    Word8.toString (Word8.fromInt i) ^
                    "    " ^ Int.toString count ^ "\n")
           end);

          print "Delta: ";
          Array.app (fn i => print (Int.toString i ^ " ")) dt;
          print "\n";

          (* Apply delta to start histo. *)
          Array.update (start_histo, 0x20,
                        Array.sub (start_histo, 0x20) + Array.sub (dt, 10));
          Util.for 0 9
          (fn d =>
           let
             val idx = ord #"0" + d
             val was = Array.sub (start_histo, idx)
           in
             print ("For digit " ^ Int.toString d ^ " was " ^
                    Int.toString was ^
                    " + delta " ^ Int.toString (Array.sub (dt, d)) ^ "\n");
             Array.update (start_histo, idx,
                           was + Array.sub (dt, d))
           end);
          loop ()
        end
    in
      (*
      print "Start histo:\n";
      Util.for 0x20 0x7e
      (fn i =>
       let
         val count = Array.sub (start_histo, i)
       in
         print ("        " ^ implode [chr i] ^ "   0x" ^
                Word8.toString (Word8.fromInt i) ^
                "    " ^ Int.toString count ^ "\n")
       end);
*)
      loop ()
    end

end

val () = Params.main1 "histo.exe input_file" Histo.go
       handle e as Histo.Histo s =>
         let in
           TextIO.output(TextIO.stdErr, s ^ "\n");
           raise e
         end
