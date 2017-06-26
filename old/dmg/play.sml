
structure Play = 
struct

  exception Play of string

  fun play cdb s =
    (case ListUtil.Alist.find op= cdb s of
      SOME song =>
        let
          open ChordDB

          fun chord (base, tune, capo) ch =
            ListUtil.map3
              (fn (X, p, c) => X
                | (t, p, X) => X
                | (F n, O, O) => F n
                | (F n, O, F m) => F (n + m + base)
                | (F n, F x, O) => F (n + x)
                | (F n, F x, F m) => F (n + 
                                        (if x > (m + base)
                                         then x else (m + base)))
                | _ => raise Play "bad chord/context")
              tune capo ch

          fun tune old new =
            ListPair.map
            (fn (X, _) => X
              | (_, X) => X
              | (F n, F m) => F (n + m)
              | (f, O) => f
              | (O, f) => f) (old, new)

          fun capo old new =
            ListPair.map
            (fn (X, _) => raise Play "capo X??"
              | (_, X) => raise Play "capo X??"
              | (F n, F m) => F (if n > m then n else m)
              | (O, f) => f
              | (f, O) => f) (old, new)

          fun doexp G (Chord c) : chord list =
            [chord G c]

            | doexp G (Rep el) =
            List.concat (map (doexp G) (el @ el))

            | doexp (b,t,p) (Tune (c, el)) =
            List.concat (map (doexp (b, tune t c, p)) el)

            | doexp (b,t,p) (PC (i, c, el)) =
            List.concat (map (doexp (i, t, capo p c)) el)
        in
          List.concat (map (doexp (0, [F 0, F 0, F 0,
                                       F 0, F 0, F 0], 
                                   [O,O,O,O,O,O])) song)
        end
    | NONE => raise Play ("song " ^ s ^ " not found in db"))
       handle (e as Play s) =>
         let in
           print s;
           print "\n";
           raise e
         end
     

end