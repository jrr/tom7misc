
structure Marker =
struct

  structure MT = MersenneTwister
  val mt = MT.init32 0wx12345

  exception BadMarker of string
  fun marker (a, b) =
    let
      fun sv s : bool Vector.vector =
        if size s <> 8
        then raise BadMarker "wrong size"
        else Vector.tabulate (8, fn i =>
                              case CharVector.sub (s, i) of
                                #" " => false
                              | #"*" => true
                              | _ => raise BadMarker "bad char")
    in
      Vector.concat [sv a, sv b]
    end

  type marker = bool Vector.vector

  fun parity v = Vector.foldl (fn (a, b) => a <> b) false v
  (* Parity for top and bottom row. Doesn't appear to be relevant. *)
  fun parity1 v = parity (VectorSlice.vector (VectorSlice.slice (v, 0, SOME 8)))
  fun parity2 v = parity (VectorSlice.vector (VectorSlice.slice (v, 8, SOME 8)))

  (* This is the same as !parity. *)
  fun rowparity v = parity1 v = parity2 v

  fun popcount v = Vector.foldl (fn (a, b) => if a then 1 + b else b) 0 v

  fun rotate180 v = Vector.tabulate(16, fn i => Vector.sub (v, 15 - i))
  fun toword v =
    Vector.foldl (fn (bit, w) => Word32.orb(Word32.<<(w, 0w1),
                                            if bit then 0w1 else 0w0)) 0w0 v
  fun fromword w =
    Vector.tabulate
    (16,
     (fn b =>
      0w0 <> Word32.andb (w, Word32.<<(0w1, Word.fromInt (15 - b)))))

  fun wparity 0w0 = 0w0
    | wparity w = Word32.xorb (Word32.andb (w, 0w1),
                               wparity (Word32.>> (w, 0w1)))

  (* XXX filter out symmetric ones. But also "normalize" so that we don't
     have two versions of the same marker? *)
  fun is_symmetric v =
    let
      fun f 16 = true
        | f n = if Vector.sub (v, n) = Vector.sub (v, 15 - n)
                then f (n + 1)
                else false
    in
      f 0
    end

  (* All the legal ones *)
  structure WS = SplaySetFn(type ord_key = Word32.word
                            val compare = Word32.compare)
  fun all_legal () =
    let
      val all = ref WS.empty
      fun gen x =
        let
          val () = if x < 0 orelse x > 2047
                   then raise BadMarker "out of range"
                   else ()
          val w = Word32.fromInt x
          (* 00000bbbbbbbbbbb *)
          (* fedcba9876543210 *)
          val p = wparity w
          val w = Word32.orb(w, Word32.<< (p, 0w11))
          (* 0000pbbbbbbbbbbb *)
          (* fedcba9876543210 *)
          val w = Word32.orb
            (Word32.andb (w, 0w63),
             Word32.<<
             (Word32.andb (w, Word32.notb (0w63)),
              0w2))
          (* 00pbbbbb00bbbbbb *)
          val w = Word32.<< (w, 0w1)
          (* 0pbbbbb00bbbbbb0 *)
          val w = Word32.orb (w, 0wx8181)
          (* 1pbbbbb11bbbbbb1 *)

          val v = fromword w
          val rv = rotate180 v
          val rw = toword rv
        in
          if popcount v = 10
          then
            (* Insert the marker in its canonical orientation. *)
            (case Word32.compare (w, rw) of
               (* ... but exclude symmetric markers. *)
               EQUAL => ()
             | LESS => all := WS.add (!all, w)
             | GREATER => all := WS.add (!all, rw))
          else ()
        end
    in
      Util.for 0 2047 gen;
      map fromword (WS.listItems (!all))
    end

  (*
  local
    fun num r = TextSVG.rtos (r / 72.0)
  in
    val borderpts =
      String.concat
      [
       "M",
       num 122.4003906,
       ",",
       num 7.2001953,
       "V",
       num 28.7998047,
       "c",
       num 0.0,
       ",",
       num 3.9765625,
       num ~3.2246094,
       ",",
       num 7.2001953,
       num ~7.2001953,
       ",",
       num 7.2001953,
       "h",
       num ~108.0,
       "C",
       num 3.2236328,
       ",",
       num 36.0,
       ",",
       num 0.0,
       ",",
       num 32.7763672,
       ",",
       num 0.0,
       ",",
       num 28.7998047,
       "V",
       num 7.2001953,
       "C",
       num 0.0,
       ",",
       num 3.2236328,
       ",",
       num 3.2236328,
       ",",
       num 0.0,
       ",",
       num 7.2001953,
       ",",
       num 0.0,
       "h",
       num 108.0,
       "C",
       num 119.1757813,
       ",",
       num 0.0,
       ",",
       num 122.4003906,
       ",",
       num 3.2236328,
       ",",
       num 122.4003906,
       ",",
       num 7.2001953,
       "z"]
  end
*)

  fun markersvg (x, y) v =
    let
      fun xy (xx, yy) = TextSVG.rtos (x + xx) ^ "," ^ TextSVG.rtos (y + yy)
      val bg =
        "<path fill=\"#000000\" d=\"" ^
        "M" ^ xy (1.7, 0.10000271) ^
        "V" ^ TextSVG.rtos(y + 0.10000271 + 0.3) ^
        let
          val xx = 1.7
          val yy = 0.10000271 + 0.3
        in
          "C" ^ xy (xx, yy + 0.05523003) ^ "," ^
          xy (xx - 0.04478624, yy + 0.1) ^ "," ^
          xy (xx - 0.1, yy + 0.1)
        end ^
        "h-1.5" ^
        "C" ^ xy (0.04477268,0.5) ^ "," ^ xy (0.0,0.45522732) ^ "," ^ xy (0.0,0.4) ^
        "V" ^ TextSVG.rtos (y + 0.1) ^
        "C" ^ xy (0.0,0.04477268) ^ "," ^ xy (0.04477268,0.0) ^ "," ^ xy (0.1,0.0) ^
        "h1.5" ^
        "C" ^ xy (1.65521918,0.0) ^ "," ^ xy (1.7,0.04477268) ^ "," ^ xy (1.7,0.1) ^
        "z\"/>\n"

      val dots = ref nil
    in
      Util.for 0 1
      (fn r =>
       Util.for 0 7
       (fn c =>
        let val idx = r * 8 + c
        in
          if Vector.sub (v, idx)
          then
            dots :=
            ("<circle fill=\"#fff\" cx=\"" ^ TextSVG.rtos (x + real c * 0.2 + 0.1 + 0.05) ^
             "\" cy=\"" ^ TextSVG.rtos (y + real r * 0.2 + 0.1 + 0.05) ^
             "\" r=\"0.05\"/>\n") :: !dots
          else ()
        end));

       String.concat (bg :: rev (!dots))
    end

  val markers = ref (MT.shuffle_list mt (all_legal()))
  (* val markers = ref [hd (!markers)] *)
  val () = print ("Valid markers: " ^ Int.toString (length (!markers)) ^ "\n")

  fun printmarker m =
    print ("      (\"" ^
           CharVector.tabulate
           (8,
            (fn x => if Vector.sub (m, x) then #"*" else #" ")) ^ "\",\n" ^
           "       \"" ^
           CharVector.tabulate
           (8,
            (fn x => if Vector.sub (m, x + 8) then #"*" else #" ")) ^ "\"),\n")

  (*

  One-off to add these to the database, but might be useful again.

  structure IS = SplaySetFn(type ord_key = int val compare = Int.compare)
  val firstpage_ok = IS.fromList [0, 2, 4, 6, 7, 8, 9, 10, 11,
                                  14, 16, 18, 19, 20, 21, 23,
                                  24, 25, 36, 42, 43]

  local val mv = Vector.fromList (!markers)
  in
    val () =
      let in
        print "\nthese scanned\n";
        Util.for 0 43
        (fn i =>
         if IS.member (firstpage_ok, i)
         then printmarker (Vector.sub (mv, i))
         else ());
        print "\nthese didn't scan\n";
        Util.for 0 43
        (fn i =>
         if not (IS.member (firstpage_ok, i))
         then printmarker (Vector.sub (mv, i))
         else ())
      end
  end
  *)

  fun makepage n =
    let val s = ref (nil : string list)
    in
      Util.for 0 10
      (fn r =>
       Util.for 0 3
       (fn c =>
        let
          val y = 0.25 + (real r)
          val x = 0.5 + (2.0 * real c)
        in
          case !markers of
            m :: rest =>
              let in
                markers := rest;
                s := markersvg (x, y) m :: !s
              end
          | nil => ()
        end));

      TextSVG.svgheaderex { x = 0.0, y = 0.0,
                            width = 8.5, height = 11.0,
                            units = "in", generator = "marker.sml" } ^
      String.concat (!s) ^
      TextSVG.svgtext { x = 0.15, y = 10.65, face = "Helvetica,sans-serif",
                        size = 0.20, text = [("#000", Int.toString n)] } ^
      TextSVG.svgfooter ()
    end

  fun writepages () =
    let fun loop n =
      case !markers of
        nil => ()
      | _ =>
          let
            val f = ("page" ^ Int.toString n ^ ".svg")
          in
            StringUtil.writefile f (makepage n);
            print ("Wrote " ^ f ^ "\n");
            loop (n + 1)
          end
    in
      loop 1
    end

end