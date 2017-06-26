(* This is the "Instrument" version of MIDIMML. ("Midimml 2.2")

   Like the Polyphonic version in poly.sml, If it sees overlapping
   notes in the input MIDI, then it can generate a rapid arpeggio
   between them, to simulate polyphony. This generally sounds bad. 

   It also interprets MIDI instruments to generate user-configured
   settings for the NES music channels. This allows composition with
   volume and pitch envelopes, etc. 

   This program is licensed under the GNU GPL. See the file COPYING
   for details. *)

structure Inst =
struct

  val quant =
    Params.param "1" (SOME ("-quantize",
                            "Round off notes to this many MIDI ticks."))
       "quant"

  val config =
    Params.param "default.mmc"
       (SOME ("-config",
              "Set the config file. Look at default.mmc for an example."))
       "config"

  val outpath =
    Params.param ""
       (SOME ("-out",
              "Set the directory in which to write the mml output."))
       "outpath"

  val debug =
    Params.flag false
       (SOME ("-debug", "Turn on debugging info (not recommended)."))
       "debug"

  fun dprint f =
      if !debug
      then print (f () ^ "\n")
      else ()

  fun getbase f =
    (case StringUtil.rfind "\\" f of
       NONE => f
     | SOME n => String.substring(f, n + 1, size f - (n + 1)))

  exception MML of string

  val itos = Int.toString

  (* could have 'quiet' flag to disable this *)
  fun warn s =
      let in
          print ("Warning: " ^ s ^ "\n")
      end

  fun dirplus "" p = p
    | dirplus d p =
      case CharVector.sub(d, size d - 1) of
          #"/" => d ^ p
        | _    => d ^ "/" ^ p

  (* get the name of a track *)
  fun findname nil = NONE
    | findname ((_, MIDI.META (MIDI.NAME s)) :: _) = SOME s
    | findname (_ :: rest) = findname rest


  (* make note-on 0 be note-off *)
  fun clean (MIDI.NOTEON (a, b, 0)) = MIDI.NOTEOFF (a, b, 0)
    | clean x = x

  fun doconfig wrl f =
    let 

      val title = ref ""
      val composer = ref ""
      val programmer = ref ""
      val drumpath = ref ""
      val drums = ref nil
      val quote = ref nil
      val instrums = Array.array(128, NONE)

      fun oneline s =
        case StringUtil.token StringUtil.whitespec s of
          ("TITLE", t) => title := t
        | ("COMPOSER", c) => 
            let in
              composer := c;
              if !programmer = ""
              then programmer := c
              else ()
            end
        | ("PROGRAMMER", p) => programmer := p
        | ("PROGRAMER", p) => programmer := p
        | ("DRUMPATH", dp) => drumpath := dp
        | ("DRUM", d) =>
            (case String.tokens StringUtil.whitespec d of
               [sample, midinote] =>
                 (case Int.fromString midinote of
                    SOME i => drums := (i, OS.Path.concat(!drumpath, sample))
                                        :: !drums
                  | NONE => 
                      raise MML "config: midi note must be a number")
             | _ => raise MML
                    ("config: DRUM must be followed by a " ^
                     "sample name and midi note number"))
        | ("QUOTE", q) => quote := q :: !quote
        | ("INSTRUMENT", rest) =>
                 let
                     val (n, command) = 
                         StringUtil.token StringUtil.whitespec rest
                 in
                     case Int.fromString n of
                         NONE => raise MML 
                             ("config: bad instrument number " ^ n)
                       | SOME n => 
                     case Array.sub(instrums, n) of
                         NONE => Array.update(instrums, n, SOME command)
                       | SOME _ => raise MML 
                             ("config error: instrument " ^ 
                              itos n ^ " defined multiple times")
                 end
        | ("", "") => ()
        | ("#", _) => ()
        | (what, _) => raise MML ("config: unrecognized command '" ^
                                  what ^ "'")


      val fd = TextIO.openIn f

      fun rconf () =
        case TextIO.inputLine fd of
          NONE => TextIO.closeIn fd
        | SOME line =>
            (* lose trailing whitespace *)
            (oneline (StringUtil.losespecr StringUtil.whitespec line);
             rconf ())

      val _ = rconf ()

      val drums = ListUtil.mapi (fn ((s, path), i) => (s, (i, path))) (!drums)

      (* returned from config generator *)
      fun mapdrums x =
        case ListUtil.Alist.find op= drums x of
          NONE => raise MML ("drum for MIDI note #" ^ itos x ^ 
                             " never defined in config")
        | SOME (idx, _) => idx

      fun emitdrum (s, (i, path)) =
        wrl ("@DPCM" ^ Int.toString i ^ 
             " = { \"" ^ path ^ "\" 15 }")

      (* if the instrument has not been set, then output nothing.

         XXX maybe it makes sense to "reset" the instrument states? *)
      fun mapinstrums x =
          (case Array.sub(instrums, x) of
               NONE =>
                   let in
                       (* warn, but just once *)
                       warn ("Instrument " ^ itos x ^ " undefined!");
                       Array.update(instrums, x, SOME "");
                       ""
                   end
             | SOME cmd => cmd)

    in
      wrl ("#TITLE " ^ !title);
      wrl ("#COMPOSER " ^ !composer);
      (* nb. spelling mistake in format *)
      wrl ("#PROGRAMER " ^ !programmer);
      wrl "";
      wrl "/* generated by midimml */";
      wrl "";

      app wrl (rev (!quote));

      (* calibrate drum track to octave 0 *)
      wrl "E o0";
      wrl "";

      app emitdrum drums;
      
      (mapdrums, mapinstrums)
    end

  fun write (_, _, tracks) name =
    let
      val notes = Vector.fromList
        ["c", "c+", "d", "d+", "e", "f", "f+", "g", "g+", "a", "a+", "b"]

      val warn = fn msg => warn (getbase name ^ ": " ^ msg)

      (* current octave *)
      val co = ref 999

      fun mknote (n, false) =
        let val t = n - 24
          val octave = 1 + t div 12
        in 
          if t >= 0
          then
            (if !co <> octave then (co := octave; 
                                    "o" ^ itos (1 + t div 12) ^ " ")
             else "") ^ Vector.sub(notes, t mod 12)
          else raise MML ("note (" ^ itos n ^ ") too low")
        end
      (* direct, like for drums *)
        | mknote (n, _) = Vector.sub(notes, n)

      val f = TextIO.openOut name
      fun wr s = TextIO.output (f, s)
      fun wrl s = (TextIO.output (f, s);
                   TextIO.output (f, "\n"))

      (* XXX calculate this table based on tempo *)
                 (* midi frames, nth note *)
      val times = [(512, 1),
                   (256, 2),
                   (128, 4),
                   (64,  8),
                   (32, 16),
                   (16, 32),
                   (8,  64),
                   (4, 128),
                   (2, 256),
                   (1, 512)]

      (* these track the state of what we're outputting,
         so that we don't repeat ourselves *)

      (* current octave *)
      val co = ref 999
      (* current velocity *)
      val cv = ref 999
      (* current length *)
      val cl = ref 0
      (* current instrument *)
      val ci = ref 999

      fun mktime n =
        let 
            val () = dprint (fn () => "mktime " ^ Int.toString n)
            fun findy _ nil = raise MML "can't make 0 time" 
              | findy m ((fr,th)::rest) =
                if m = fr
                then itos th
                else if m > fr
                     then itos th ^ "^" ^ findy (m - fr) ((fr,th)::rest)
                     else findy m rest
            val tt = findy n times
        in
          findy n times
        end

      fun wrvel vel =
        let val veloc = vel div 8
        in
          if !cv <> veloc
          then (wr ("v"  ^ itos veloc ^ " ");
                cv := veloc)
          else ()
        end

      fun onenote nmap imap (n, v, inst) =
        let in
          dprint (fn () => "onenote n:" ^ Int.toString n ^ " v:" ^ Int.toString v ^ "\n");
          (* single note *)
          wrvel v;
          if !ci = inst
          then ()
          else 
              let in
                  (* write instrument init and save
                     state *)
                  ci := inst;
                  wr (" " ^ imap inst ^ " ")
              end;
          wr (mknote (nmap n))
        end
      
      fun makesound nmap imap notes 0 =
        (case notes of
           nil => ()
         | _ => warn "0-length notes??")
        | makesound nmap imap notes ticks =
        (dprint (fn () => "makesound [" ^ StringUtil.delimit ", "
                                      (map (fn (n, v, _) => itos n ^ "/" ^ itos v) notes) ^ 
                                      "] ticks: " ^ itos ticks ^ "\n");
         case notes of
           nil =>
             let in
               (* rest ... *)
               wr ("r" ^ mktime ticks ^ " ")
             end
         | [one] =>
             let in
               onenote nmap imap one;
               wr (mktime ticks);
               wr " "
             end
         | many =>
             let 
               (* warn with -nopoly switch or something *)
               val num = length many
               val reps = ticks div num
               val left = ticks mod num
             in
               (* what about 1? *)
               if reps > 0
               then
                 let in
                   wr ("[l" ^ mktime 1 ^ " ");
                   app (onenote nmap imap) many;
                   wr "]";
                   wr (itos reps);
                   wr " "
                 end
               else ();
               (* write leftovers *)
               if left > 0 then
                  let in
                    wr ("l" ^ mktime 1 ^ " ");
                    app (onenote nmap imap) (List.take (many, left));
                    wr " "
                  end
               else ()
             end)

      (* called with track name ("A") and list of
         (name option, MIDI track)s that match it. *)
      fun dowrite nmap imap s (ts : (int * MIDI.event) list list) =
        let
          (* reset current states *)
          val _ = cv := 999
          val _ = cl := 999
          val _ = co := 999

          (* initialize current instruments (this tracks what
             we've actually output, so that we don't keep outputting
             instrument change commands that are the same) *)
          val _ = ci := 999

          (* and this tracks the instrument that will be used
             for a midi event seen on any particular track. *)
          val trackinst = Array.array(length ts, 0)

          (* instrument tracks with indices. These are used
             to keep track of the current instrument on each
             track. *)

          val ts = ListUtil.mapi (fn (a, n) => (n, a)) ts

          val warn = fn msg => warn ("(track " ^ s ^ ") " ^ msg)

          (* get all the events at the next tick, ignoring
             anything that doesn't pass "filter" *)
          (* n = smallest delta on any track. 
             subtract n from the delta for each
             track, then report all events with
             delta 0, and return n. *)
          fun nextevents (trl : (int * (int * MIDI.event) list) list) =
            let
              val ttl = ListUtil.sort (ListUtil.bysecond
                (fn (nil, nil) => EQUAL
                  | (_, nil) => LESS
                  | (nil, _) => GREATER
                  | ((d,_)::_, (c,_)::_) => Int.compare (d, c))) trl
            in
              case ttl of
                (id, (n, _)::_)::_ =>
                  let
                    (* subtract delta from everything else.
                       this will never make it negative, since
                       we got the smallest delta. *)
                    val ntl = map (fn (x, nil) => (x, nil)
                                    | (x, (d, e)::t) => 
                                      (x, (d - n, e)::t)) ttl

                    (* get all 0-time events from the head of list,
                       return rest too *)
                    fun gete (x, l) =
                      let 
                          val (zero, rest) =
                                   ListUtil.partitionaslongas
                                   (fn (0, _) => true | _ => false) l
                      in
                          (map (fn (_, e) => (x, e)) zero, (x, rest))
                      end

                    (* get any events with delta 0 *)                    
                    val (zeros, rests) = ListPair.unzip (map gete ntl)

                  in
                    (* return: n (defined above)
                       zeros : int * MIDI.event list
                          (list of events happening now,
                          paired with track number)
                       rests : (int * (int * MIDI.event) list) list
                          (rest of tracks) *)
                    (n, List.concat zeros, rests)
                  end
               | _ =>
                  let in
                    print "No more events in any track.\n";
                    print "Perhaps missing \"end\" marker?\n";
                    raise MML "bad midi"
                  end
            end

          fun etos (MIDI.NOTEON (_, nn, vel)) = "NOTEON " ^ itos nn ^ "/" ^ itos vel
            | etos (MIDI.NOTEOFF (_, nn, _)) = "NOTEOFF " ^ itos nn
            | etos (MIDI.PCHANGE (_, inst)) = "PCHANGE " ^ itos inst
            | etos (MIDI.META _) = "META?"
            | etos _ = "???"

          (* process a state (list of on notes)
             by getting the next chunk of events,
             and then dumping sound for the interval. *)
          fun process (state as (notes, time)) ts =
            let
              val (n, now, ts) = nextevents ts

              (* turn noteon 0 -> noteoff *)
              val now = ListUtil.mapsecond clean now

              val () = dprint (fn () =>
                               "events: \n" ^
                               StringUtil.delimit "\n" (map (etos o #2) now) ^ "...\n")

              (* change the events list. ignore channels. *)
              fun proce ((tr, MIDI.NOTEON (_, nn, vel)), notes) =
                (case List.partition (fn (num, _, _) =>
                                      num = nn) notes of
                   (nil, notes) => (nn, vel, Array.sub(trackinst, tr))
                                         :: notes
                 | (_, notes) =>
                     let in
                       warn "note-on for note already on!";
                       notes
                     end)
                | proce ((_, MIDI.NOTEOFF (_, nn, _)), notes) =
                (case List.partition (fn (num, _, _) =>
                                      num = nn) notes of
                   (nil, notes) =>
                     let in
                       warn "note-off for note that was off!";
                       notes
                     end
                 | (_, notes) => notes)
                | proce ((tr, MIDI.PCHANGE (_, inst)), notes) =
                     let in
                         Array.update(trackinst, tr, inst);
                         notes
                     end
                | proce (_, notes) = notes

              (* get new notes *)
              val newnotes = foldl proce notes now

              fun hasmark s nil = false
                | hasmark s ((_, MIDI.META (MIDI.MARK m)) :: rest) =
                s = m orelse hasmark s rest
                | hasmark s (_ :: t) = hasmark s t

              fun getmark s nil = NONE
                | getmark s ((_, MIDI.META (MIDI.MARK m)) :: rest) =
                if StringUtil.matchhead s m
                then SOME m
                else getmark s rest
                | getmark s (_ :: t) = getmark s t

            in
              (* generate sound for n + time steps (previous
                 frames) *)
              makesound nmap imap notes (n + time);

              (* start loop if "loop" marker *)
              if hasmark "loop" now
              then wr "L "
              else ();

              case getmark "t" now of
                NONE => ()
              | SOME t => wr (t ^ " ");

              (* if no "end" marker, continue ... *)
              if hasmark "end" now
              then ()
              else process (newnotes, 0) ts
            end

          (* since events in the middle of notes can cause
             disturbances, only get the events that we're interested
             in. This is sort of a stop-gap measure, since something
             like 'loop' in the middle of a note still breaks the
             note into two parts (or any other mark we don't care
             about) *)
          fun goodevt (MIDI.NOTEON _) = true
            | goodevt (MIDI.NOTEOFF _) = true
            | goodevt (MIDI.PCHANGE _) = true
            | goodevt (MIDI.META (MIDI.MARK _)) = true
            | goodevt _ = false


          fun evtfilter f n ((delta, h) :: t) =
              if f h
              then (* keep it. *)
                  (delta + n, h) :: evtfilter f 0 t
              else (* delete it. *)
                  evtfilter f (delta + n) t
            | evtfilter _ _ nil = nil

          fun trkfilter f (id, l) = (id, evtfilter f 0 l)

          val ts = map (trkfilter goodevt) ts
              
        in
          wr "\n\n";
          wr (s ^ " ");
          process (nil, 0) ts
        end

      (* tracks that should always be included. We add anything 
         that doesn't have any note-on or note-off events. *)
      fun keeper nil = true
        | keeper ((_, MIDI.META _) :: rest) = keeper rest
          (* any non-meta event makes it a non-keeper *)
        | keeper (_ :: _) = false

      (* get tracks starting with s,
         or with no name (control track(s)) *)
      (* XXX this is bogus, since a midi sequencer is
         free to put control data on ANY track. what we
         should be doing is filtering out all of the
         META events we're concerned with from ALL other
         tracks, and including them here. (right?) *)
      fun tracksfor s nil = nil
        | tracksfor s (h::t) =
          if keeper h then h :: tracksfor s t
          else
              case findname h of
                  NONE => h :: tracksfor s t
                | SOME tn => if StringUtil.matchhead s tn
                      orelse tn = "untitled" (* sonar XL *)
                             then h :: tracksfor s t
                             else tracksfor s t

      fun id x = (x, false)
      fun add n x = (n + x, false)

      val (mapdrums, mapinstrums) = doconfig wrl (!config)

      val mapdrums = fn x => (mapdrums x, true)

    in

      app (fn (s,mapper) =>
           dowrite mapper mapinstrums s (tracksfor s tracks))
          [("A",id), ("B",id), ("C",add 12), ("D",id), ("E", mapdrums)];

      wrl "";
      wrl "/* end */";
      wrl "";
      TextIO.closeOut f
    end


  (* round each delta as if the resolution of midi
     ticks were 1/ticks what it was *)
  fun quantize 1 midi = midi   (* common case *)
    | quantize ticks (mtype, mdivision, tracks) =
      let
          fun onetrack t =
              map (fn (delta, evt) =>
                   ((delta div ticks) * ticks, evt)) t
      in
          (mtype, mdivision, map onetrack tracks)
      end

  fun convert f =
    let 
      val r = (Reader.fromfile f) handle _ => 
                  raise MML ("couldn't read " ^ f)
      val m as (ty, _, _) = MIDI.readmidi r

      val _ = ty = 1
              orelse raise MML ("MIDI file must be type 1 (got type " ^ itos ty ^ ")")

      val m = quantize (Params.asint 1 quant) m

      val base = getbase f

      val out = 
        dirplus 
        (!outpath)
        ((case StringUtil.find "." base of
              NONE => base
            | SOME n => String.substring(base, 0, n)) ^ ".mml")
    in 
      #close r ();
      write m out
    end handle (e as (MML s)) => (print ("Error: " ^ s ^ "\n"); raise e)
       | (e as (MIDI.MIDI s)) => (print ("Bad MIDI: " ^ s ^ "\n"); raise e)
end

