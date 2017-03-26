structure MIDI2ABC =
struct
  infixr 9 `
  fun a ` b = a b

  (* command line option? length of base note. *)
  val MIN = 30

  exception MIDI2ABC of string

  fun normalize (MIDI.NOTEON (c, n, 0)) = SOME ` MIDI.NOTEOFF (c, n, 0)
    | normalize (e as MIDI.NOTEON _) = SOME e
    | normalize (e as MIDI.NOTEOFF _) = SOME e
    | normalize _ = NONE

  fun getlen dt =
    let val x = dt div MIN
    in
      case dt mod MIN of
        0 => x
      | _ => raise MIDI2ABC ("can't render length " ^ Int.toString dt ^ " with MIN=" ^
                             Int.toString MIN)
    end

  (* XXX this will render numbers greater than 8, which won't work *)
  fun lenstring 1 = ""
    | lenstring n = Int.toString n
  fun notename n =
    let
      val octave = n div 12
      val note = n mod 12
      fun rep 0 s = ""
        | rep x s = rep (x - 1) s ^ s
      val basenotes =
        Vector.fromList ["C", "^C", "D", "^D", "E", "F", "^F", "G", "^G", "A", "^A", "B"]
      val upnotes = Vector.map StringUtil.lcase basenotes
      val (suffix, str) =
        if octave <= 4 then (rep (4 - octave) ",", basenotes)
        else (rep (octave - 5) "'", upnotes)
    in
      Vector.sub (str, note) ^ suffix
    end

  fun go file =
    let
      val (typ, divi, tracks) = MIDI.readmidi (Reader.fromfile file)

      fun onetrack nil = ""
        | onetrack ((dt, MIDI.NOTEON (_, n, vel)) :: (nextdt, MIDI.NOTEOFF (_, nn, _)) :: rest) =
        let in
          if n <> nn then raise MIDI2ABC "overlapping notes" else ();
(*
          (if dt > 0
           then "  REST for " ^ Int.toString dt ^ "\n"
           else "") ^
          "  ON " ^ Int.toString n ^ " for " ^ Int.toString nextdt ^ "\n" ^ *)
          (if dt > 0
           then "z" ^ lenstring (getlen dt)
           else "") ^
          notename n ^ lenstring (getlen nextdt) ^
          onetrack rest
        end
        | onetrack _ = "midi not in normal form.\n"

      fun dotrack t =
        let
          val t = MIDI.map_partial normalize t
        in
          print "Track:\n";
          print (onetrack t);
          print "\n"
        end
    in
      app dotrack tracks;
      print "OK\n"
    end

end


val () = Params.main1 "histo.exe input.midi" MIDI2ABC.go
       handle e as MIDI2ABC.MIDI2ABC s =>
         let in
           TextIO.output(TextIO.stdErr, s ^ "\n");
           raise e
         end
