structure MakeFreq =
struct

  exception MakeFreq of string

  val ftos = Real.fmt (StringCvt.FIX (SOME 2))

  (* For each MIDI note, find the closest adlib block/frequency pair. *)
  fun go () =
    let
      val adlib =
        Vector.tabulate
        (8192, (fn bits =>
                let
                  (* 3 block bits followed by 10 freq bits. *)
                  val block = real (bits div 1024)
                  val fnum = real (bits mod 1024)
                (* Invert this to solve for freq:
                   fnum = freq * 2^(20 - block) / 49716
                   fnum * 49716 = freq * 2^(20 - block)
                   (fnum * 49716) / 2^(20 - block) = freq *)
                  val freq = (fnum * 49716.0) / Math.pow (2.0, 20.0 - block)
                in
                  freq
                end))
      fun getclosest target =
        let
          val closest_idx = ref 0
          val closest_dist = ref (Real.abs (Vector.sub (adlib, 0) - target))
        in
          Vector.appi (fn (idx, freq) =>
                       let
                         val dist = Real.abs (freq - target)
                       in
                         if dist < !closest_dist
                         then
                           let in
                             closest_idx := idx;
                             closest_dist := dist
                           end
                         else ()
                       end) adlib;
          print ("Closest block/fnum to " ^ ftos target ^ " is " ^
                 Int.toString (!closest_idx) ^ " (" ^
                 ftos (!closest_dist) ^ " away)\n");
          !closest_idx
        end

      val freqs = Vector.tabulate (127, MIDI.pitchof)
      val best = Vector.map getclosest freqs
      fun out c =
        if c > 0x20 andalso c < 0x7e andalso c <> ord #"\"" andalso c <> ord #"\\"
        then implode [chr c]
        else implode [#"\\", #"x",
                      StringUtil.nybbletohex (c div 16),
                      StringUtil.nybbletohex (c mod 16)]
      fun vtol v = Vector.foldr op:: nil v
      val upper = String.concat (map (fn idx =>
                                      out (32 + idx div 256)) (vtol best))
      val lower = String.concat (map (fn idx =>
                                      out (idx mod 256)) (vtol best))
    in
      print ("char upper = \"" ^ upper ^ "\";\n");
      print ("char lower = \"" ^ lower ^ "\";\n")
    end

end

val () = Params.main0 "makefreq.exe" MakeFreq.go
