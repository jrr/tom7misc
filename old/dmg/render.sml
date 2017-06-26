
structure Render =
struct

  (* render a chord list as a midi file *)
  fun render f cl =
    let
      open ChordDB MIDI
      val base = [40, 45, 50, 55, 59, 64]

      fun playnote (X,b) = NONE
        | playnote (O,b) = SOME b
        | playnote (F x, b) = SOME (b + x)

      fun playchord c =
        let val notes =
          List.mapPartial (fn x => x)
            (ListPair.map playnote (c, base))

            val nn = length notes

            (* causes drift ... *)
            val strumlen = 0

            fun strum l =
              let
                fun st n nil = nil
                  | st n ((delta, evt)::rest) = 
                  (delta + n * strumlen, evt) :: st (n + 1) rest
              in
                st 0 l
              end
        in
          strum (map (fn n => (0, NOTEON (1, n, 100))) notes) @
          (100 - (if nn > 0 
                  then (nn - 1) * strumlen
                  else 0), META (PROP "")) ::
          map (fn n => (0, NOTEOFF (1, n, 0))) notes
        end
      
      val track = 
        (* use steel string guitar *)
        (0,PCHANGE (1,25)) ::
        List.concat (map playchord cl) @ [(0, META END)]
    in
      MIDI.writemidi f (1, 128, [track])
    end



end

