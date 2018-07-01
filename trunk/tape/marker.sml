
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

  (* Get some official bitstrings *)
  val official = []

  (* Unofficial strings that did scan *)
  val scanned = map marker
    [("** **  *",
      "* * * **"),
     ("*** * **",
      "* *   **"),
     ("**** * *",
      "*  *  **"),
     ("***  * *",
      "* **  **"),
     ("*** *  *",
      "* **  **"),
     ("* * * **",
      "**  ** *"),
     ("*** * **",
      "* * *  *")]

  (* These didn't scan, suggesting they are invalid --
     but it could also just be dirt, etc. *)
  val unscanned = map marker
     [("** **  *",
       "* * ****"),
      ("***   **",
       "*  *****"),
      ("** *  **",
       "*  **  *"),
      ("** *** *",
       "**** * *"),
      ("*   * **",
       "*** *  *")]

  fun parity v = Vector.foldl (fn (a, b) => a <> b) false v
  fun wparity 0w0 = 0w0
    | wparity w = Word32.xorb (Word32.andb (w, 0w1),
                               wparity (Word32.>> (w, 0w1)))

  (* All the legal ones *)
  fun all_legal () =
    let
      val all = ref (nil : marker list)
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
        in
          all :=
          Vector.tabulate
          (16,
           (fn b =>
            0w0 <> Word32.andb (w, Word32.<<(0w1, Word.fromInt (15 - b))))) :: !all
        end
    in
      Util.for 0 2047 gen;
      !all
    end

  fun num r = TextSVG.rtos (r / 72.0)

  val borderpts =
    String.concat
    [
     "M",
     num 122.4003906,
     ",",
     num 7.2001953,
     "v",
     num 21.5996094,
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

  fun markersvg (x, y) v =
    let
      fun xy (xx, yy) = TextSVG.rtos (x + xx) ^ "," ^ TextSVG.rtos (y + yy)
      val bg =
        "<path fill=\"#000000\" d=\"" ^
        "M" ^ xy (1.7,0.10000271) ^
        "v.3" ^
        "c0,.05523003-.04478624,.1-.1,.1" ^
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

  (* XXX shuffle them *)
  val markers = ref (MT.shuffle_list mt (all_legal()))

  fun makepage () =
    let val s = ref (nil : string list)
    in
      Util.for 0 10
      (fn r =>
       Util.for 0 3
       (fn c =>
        let
          val y = 0.25 + (real r)
          val x = 0.5 + (2.0 * real c)
          val m = hd (!markers)
        in
          markers := tl (!markers);
          s := markersvg (x, y) m :: !s
        end
        ));

      TextSVG.svgheaderex { x = 0.0, y = 0.0,
                            width = 8.5, height = 11.0,
                            units = "in", generator = "pir8tape" } ^                               String.concat (!s) ^
      TextSVG.svgfooter ()
    end

  fun writepage () =
    StringUtil.writefile "page.svg" (makepage ())

end