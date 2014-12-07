structure SVGToMap = 
struct 

  datatype tree = datatype XML.tree

  fun getrects xml =
    let
      fun getcoord attrs what : real option =
        Option.mapPartial Real.fromString (XML.getattr attrs what)
      val rects = ref nil
      fun look (Text _) = ()
        | look (Elem (("rect", attrs), _)) =
        (case (getcoord attrs "x",
               getcoord attrs "y",
               getcoord attrs "width",
               getcoord attrs "height") of
           (SOME x, SOME y, SOME w, SOME h) =>
             rects := (x, y, w, h) :: !rects
         | _ => print "Bad rect\n")
        | look (Elem (_, subtrees)) = app look subtrees
    in
      look xml;
      !rects
    end

  fun rtos r = Int.toString (Real.round r)

  val () =
      case CommandLine.arguments () of
          [f] =>
            let val xml = 
              (XML.parsefile f
               handle (e as (XML.XML s)) => 
                 (print ("Error: " ^ s); raise e))
              val rects = getrects xml
              val s =
                "/* Generated by svgtomap.sml */\n\n" ^
                "var mapsvg = [\n" ^
                StringUtil.delimit ",\n"
                  (map (fn (x, y, w, h) =>
                        ("  { x: " ^
                         rtos x ^ ", y: " ^
                         rtos y ^ ", w: " ^
                         rtos w ^ ", h: " ^
                         rtos h ^ "}")) rects) ^
                "];\n"
            in
              StringUtil.writefile "map.js" s
            end
        | _ => print "svgtomap file.xml\n"

end