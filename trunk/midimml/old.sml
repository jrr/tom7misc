
(* This is an old simpler version, which might not even work any more.
   poly.mml includes enhanced functionality. *)

structure Old =
struct

  exception MML of string

  val itos = Int.toString

  (* make note-on 0 be note-off *)
  fun clean (MIDI.NOTEON (a, b, 0)) = MIDI.NOTEOFF (a, b, 0)
    | clean x = x

  fun write (_, tracks) name =
    let
      val f = TextIO.openOut name
      fun wr s = TextIO.output (f, s)
      fun wrl s = (TextIO.output (f, s);
                   TextIO.output (f, "\n"))

      (* XXX calculate this table based on tempo *)
                 (* midi frames, nth note *)
      val times = [(512, 1),
                   (256, 2),
                   (128, 4),
                   (64, 8),
                   (32, 16),
                   (16, 32),
                   (8, 64),
                   (4, 128),
                   (2, 256),
                   (1, 512)]

      (* current octave *)
      val co = ref 999
      (* current velocity *)
      val cv = ref 999
      (* current length *)
      val cl = ref 0

      fun mktime n =
        let 
          fun findy _ nil = raise MML "can't make 0 time" 
            | findy m ((fr,th)::rest) =
            if m = fr
            then itos th
            else if m > fr
                 then itos th ^ "^" ^ findy (m - fr) ((fr,th)::rest)
                 else findy m rest
          val tt = findy n times
        in
          (*
          if !cl <> n
          then (cl := n;
                SOME (findy n times))
          else NONE
          *)
          findy n times
        end

(*
      fun mknote n = "n" ^ itos (n - 24) ^ ","
*)

      val notes = Vector.fromList
        ["c", "c+", "d", "d+", "e", "f", "f+", "g", "g+", "a", "a+", "b"]

      fun mknote n =
        let val t = n - 24
          val octave = 1 + t div 12
        in 
          if t >= 0
          then
            (if !co <> octave then (co := octave; "o" ^ itos (1 + t div 12) ^ " ")
             else "") ^ Vector.sub(notes, t mod 12)
          else raise MML "note too low"
        end

      fun mktimed s n =
        s ^ mktime n ^ " "
        (*
        (case mktime n of
           SOME l => "l" ^ l ^ " " ^ s ^ " "
         | NONE => s ^ " ")
        *)

      fun dotrack trk =
        let
          val _ = co := 999
          val _ = cv := 999

          (* look for note-on.
             last is the time that the last note
             ended. we'll emit a rest of that length
             when we see the note-on. *)
          fun on last nil = ()
            | on last (evt::rest) =
            (case evt of
               (delta, MIDI.NOTEON(ch, note, vel)) =>
                 let 
                   val len = delta + last
                 in
                   (* emit rest *)
                   if len > 0 
                   then wr(mktimed "r" len)
                   else ();
                   off 0 note vel rest
                end
             | (delta, MIDI.NOTEOFF _) => 
                let in
                  print "expected ON, not OFF...\n";
                  on (last + delta) rest
                end
             | (delta, MIDI.META MIDI.END) =>
                let 
                  val len = delta + last
                in
                  if len > 0
                  then wr(mktimed "r" len)
                  else ()
                (* done *)
                end
             | (delta, MIDI.META (MIDI.NAME s)) =>
                (wr ("/* " ^ s ^ " */ ");
                 on (last + delta) rest)
             | (delta, _) => on (last + delta) rest)

          (* look for note-off *)
          and off last note vel nil = raise MML "got eof looking for OFF"
            | off last note vel (evt::rest) =
            (case evt of
               (delta, MIDI.NOTEON _) =>
                 let in
                   print "expected OFF, got ON\n";
                   off (last + delta) note vel rest
                 end
             | (delta, MIDI.NOTEOFF (ch, newnote, _)) =>
                 if note = newnote then 
                    let 
                      val veloc = vel div 8
                    in
                      (* emit note *)
                      if !cv <> veloc
                      then (wr ("v"  ^ itos veloc ^ " ");
                            cv := veloc)
                      else ();
                      wr (mktimed (mknote note) (delta + last));
                      on 0 rest
                    end
                   else (print "note-off for wrong note\n";
                         off (last + delta) note vel rest)
             | (delta, _) => off (last + delta) note vel rest)

        in
          on 0 trk
        end

      fun dotracks (NONE::rest) (_::trest) = dotracks rest trest
        | dotracks (SOME trk::rest) (t::trest) =
        let in
          wrl "";
          print (trk ^ "...\n");
          wr (trk ^ " ");
          dotrack (ListUtil.mapsecond clean t);
          dotracks rest trest
        end
        | dotracks nil nil = ()
        | dotracks _ nil = print "Out of tracks.\n"
        | dotracks nil _ = print "Out of NES channels.\n"

    in
      wrl "#TITLE Test";
      wrl "#COMPOSER Tom 7 Entertainment System";
      wrl "#PROGRAMER Tom 7";
      wrl "";
      wrl "/* generated by midimml */";
      wrl "";
      wrl "ABCD @2 @v0 q8 o4 v8 t90";
      wrl "ABC L"; (* XXX *)
      wrl "";
      
      dotracks [NONE, SOME "A", SOME "B", SOME "C", SOME "D"] tracks;

      wrl "";
      wrl "/* end */";
      wrl "";
      TextIO.closeOut f
    end


  fun convert f =
    let 
      val r = Reader.fromfile f
      val m = MIDI.readmidi r
      val out = 
        "c:\\nesmusic\\mckwatch\\" ^ 
        (case StringUtil.find "." f of
           NONE => f
         | SOME n => String.substring(f, 0, n)) ^ ".mml"
    in 
      #close r ();
      write m out
    end handle (e as (MML s)) => (print ("Error: " ^ s ^ "\n"); raise e)
       | (e as (MIDI.MIDI s)) => (print ("Bad MIDI: " ^ s ^ "\n"); raise e)

end
