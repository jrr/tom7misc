
(* second version of parser,
   hard-core tail recursion *)

structure Parse =
struct

  exception Parse of string

  datatype section =
    Any of string * string list
  | Origin of CharVector.vector

  (* for progress reporting *)
  local
    val size = ref 1
    val last = ref 0
    val lasttime = ref (Time.now ())
  in
    fun setsize n = 
      let in
        lasttime := Time.now ();
        last := 0;
        size := n
      end

    fun progress c =
      let 
      in
        (* have we made more than 1% progress? *)
        if real (c - !last) / real (!size)
           > 0.01
        then
          let 
            val n = Time.now ()
          in
            last := c;
            (* has a reasonable time elapsed? *)
            if Time.> (Time.- (n, !lasttime),
                       Time.fromSeconds 1)
            then
              let in
                lasttime := n;
                TextIO.output
                (TextIO.stdErr,
                 Int.toString 
                 (Real.trunc (100.0 * (real (!last) /  
                                       real (!size))))
                 ^ "%...\n")
              end
            else ()
          end
        else ()
      end
  end

  fun gctos a =
    CharVector.tabulate (GCharArray.length a,
                         (fn z =>
                          GCharArray.sub a z))

  fun parse keep_section orig_s =
    let
      (* be imperative *)
      val s = ref orig_s
      val pos = ref 0

      fun read_char () =
        if Stream.is_empty (!s)
        then raise Parse ("unexpected EOF at " ^ Int.toString (!pos))
        else
          let 
            val (a, ss) = Stream.uncons (!s)
          in
            s := ss;
            pos := (!pos + 1);

            (* report progress *)
            if (0w0 = Word32.andb(Word32.fromInt (!pos),
                                  0w65535))
            then progress (!pos)
            else ();

            a
          end

      fun look_char () =
        if Stream.is_empty (!s)
        then raise Parse ("unexpected EOF at " ^ Int.toString (!pos))
        else
          let 
            val (a, _) = Stream.uncons (!s)
          in
            a
          end

      fun save () = (!s, !pos)
      fun restore (ss, pp) =
        let in
          s := ss;
          pos := pp
        end

      (* array of sections *)
      val sections = GrowArray.empty ()

      (* origin array, pick 4mb *)
      val ntides = GCharArray.init 4000000

      (* returns the number of character seen *)
      fun whitespace () =
        let
          fun ws n = 
            if look_char () = #" "
            then (read_char (); ws (n + 1))
            else n
        in
          ws 0
        end

      val letter = StringUtil.charspec "-A-Za-z0-9'_*/"

      fun word () =
        let
          fun w cs =
            if Stream.is_empty (!s)
               orelse not (letter (look_char ()))
            then implode (rev cs)
            else w (read_char () :: cs)
        in
          w nil
        end

      fun restofline () =
        let
          val a = GCharArray.init 81
          fun rol () =
            case read_char () of
              #"\n" => ()
            | c => (GCharArray.append a c;
                    rol ())
        in
          rol ();
          gctos a
        end

      fun indentlines x =
        let
          val old = save ()
          fun chew 0 = restofline () :: indentlines x
            | chew n =
            (if (not (Stream.is_empty (!s)))
                andalso read_char () = #" "
             then chew (n - 1)
             else
               let in
                 restore old;
                 nil
               end) 
        in
          chew x
        end

      (* read data from the 'origin' (nucleotide) section.
         ignore whitespace, and ignore the leading numbers 
         on each line. *)
      fun originlines () =
        let
          val old = save ()
        in
          case whitespace () of
            0 => restore old
          | _ =>
              let
                fun tides () =
                  case read_char () of
                    #" " => tides ()
                  | #"\n" => originlines ()
                  | c => (GCharArray.append ntides c;
                          tides ())

                fun num any =
                  let val c = read_char ()
                  in if Char.isDigit c
                     then num true
                     else if any
                          then tides ()
                          else restore old
                  end
              in
                num false
              end
        end

      fun consume_sections () =
        let
          val lead = whitespace ()
        in 
          case String.map Char.toLower (word ()) of
            "//" => ()
          | "origin" => 
              let in
                ignore (restofline ()); 
                originlines ()
              end
          | w =>
              let
                val trail = whitespace ()
                val rol = restofline ()
                  
                val lines = 
                  rol :: indentlines (lead + size w + trail)
              in
                if keep_section w
                then GrowArray.append sections (w, lines)
                else ();
                consume_sections ()
              end
        end
    in
      consume_sections();
      (GrowArray.finalize sections,
       GCharArray.finalize ntides)
    end


  fun parsefile filter file =
    let 
      val n = Position.toInt (Posix.FileSys.ST.size 
                              (Posix.FileSys.stat file))
      val _ = TextIO.output (TextIO.stdErr,
                             file ^ " is " ^ Int.toString n ^ " bytes\n")
      val _ = setsize n

      val s = StreamUtil.ftostream file
    in
      parse filter s
    end handle Parse s =>
      let in
        TextIO.output (TextIO.stdErr, "Parse error: " ^ s ^ "\n");
        raise Parse s
      end


end