structure Segments :> SEGMENTS =
struct

  exception Segments of string

  datatype segment =
    Segment of { name: string,
                 gates: (LatLon.pos * LatLon.pos) Vector.vector }

  fun parse_xml xml =
    let
      datatype tree = datatype XML.tree

      (* Do this in two passes, first getting rid of extraneous XML
         stuff. *)
      datatype stree =
        Folder of string * stree list
      | Placemark of string * (real * real) list

      fun getchildname children =
        case List.mapPartial (fn Elem (("name", _), body) => SOME body
                              | _ => NONE) children of
          [[Text name]] => name
        | [nil] => ""
        | _ => raise Segments ("Expected a single <name> tag as an " ^
                               "immediate child.")

      fun process (Text _) = nil
        | process (Elem (("Folder", _), children)) =
        [Folder (getchildname children, List.concat (map process children))]
        | process (elt as Elem (("Placemark", _), children)) =
        let
          val name = getchildname children
          val coordstring = case XML.firstleaf "coordinates" elt of
            NONE => raise Segments "Expected <coordinates> in <Placemark>"
          | SOME c => c
          val coords = String.tokens StringUtil.whitespec coordstring
          val coords = map (map Real.fromString o
                            String.tokens (StringUtil.ischar #",")) coords
          val coords = map (fn [SOME lat, SOME lon, SOME elev] => (lat, lon)
                            | _ => raise Segments
                            ("Bad lat,lon,elev in <coordinates>: " ^
                             coordstring)) coords
        in
          [Placemark (name, coords)]
        end
        | process (Elem (_, children)) = List.concat (map process children)

      val stree : stree list = process xml

      (* Now interpret the segments. *)

    in
      raise Segments "unimplemented"
    end

  fun parse_string contents =
    let val xml = XML.parsestring contents
      handle XML.XML s => raise Segments ("Malformed xml: " ^ s)
    in
      parse_xml xml
    end

  fun parse_file f = parse_string (StringUtil.readfile f)


end