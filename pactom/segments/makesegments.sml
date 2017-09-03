structure MakeSegments =
struct

  exception MakeSegments of string

  fun fail s =
    let in
      TextIO.output (TextIO.stdErr, s);
      OS.Process.exit OS.Process.failure
    end

  (* Some place to center the gnomonic projection. *)
  val home = LatLon.fromdegs { lat = 40.455441, lon = ~79.928058 }

  fun genpic filename (segments, activities) =
    let
      val f = TextIO.openOut filename

      val bounds = Bounds.nobounds ()
      val rtos = TextSVG.rtos
      fun xtos x = rtos (Bounds.offsetx bounds x)
      fun ytos y = rtos (Bounds.offsety bounds y)
      fun ptos (x, y) = (xtos x ^ "," ^ ytos y ^ " ")


      val osm = OSM.loadosms ["../pittsburgh-center.osm"]

        (*
        ["../pittsburgh-north.osm",
         "../pittsburgh-northeast.osm",
         "../pittsburgh-center.osm",
         "../pittsburgh-south.osm",
         "../pittsburgh-west.osm",
         "../pittsburgh-southwest.osm"]
        *)

      val gnomonic = LatLon.gnomonic home
      fun proj p =
        let
          val (x, y) = gnomonic p
        in
          (* Scale up massively, and invert Y axis. *)
          (2400000.0 * x, ~2400000.0 * y)
        end

      fun observepoint (pos : LatLon.pos) =
        Bounds.boundpoint bounds (proj pos)
      fun observesegment (Segments.Segment { gates, ... }) =
        Vector.app (fn (a, b) =>
                    let in
                      observepoint a;
                      observepoint b
                    end) gates
      fun observeactivity (GPX.Activity { points, ... }) =
        Vector.app (fn GPX.Pt { pos, ... } => observepoint pos) points

      val () = app observesegment segments
      val () = app observeactivity activities
      val () = Bounds.addmarginfrac bounds 0.05

      fun Q s = "\"" ^ s ^ "\""

      fun printpolyline c coords =
        let in
          TextIO.output(f,
                        "<polyline stroke-linejoin=" ^ Q"round" ^
                        " fill=" ^ Q"none" ^ " stroke=\"" ^ c ^
                        "\" stroke-width=" ^ Q"0.7" ^ " points=\"");
          Vector.app (fn (x, y) => TextIO.output (f, ptos (x, y))) coords;
          TextIO.output (f, "\"/>\n") (* " *)
        end

      val width = Real.trunc (Bounds.width bounds)
      val height = Real.trunc (Bounds.height bounds)

      fun output_osm (OSM.O { points, streets }) =
        let
          val missing_points = ref 0
          fun onestreet { pts, ... } =
            let
              fun onepoint i =
                case OSM.IntMap.find (points, i) of
                  NONE => missing_points := !missing_points + 1
                | SOME pos => TextIO.output (f, ptos (proj pos))
          in
            TextIO.output(f,
                          "<polyline fill=\"none\" stroke=\"#000000\" " ^
                          "opacity=\".5\" " ^
                          "stroke-width=\".3\" points=\""); (* " *)
            Vector.app onepoint pts;
            TextIO.output (f, "\" />\n") (* " *)
          end
       in
         Vector.app onestreet streets;
         if !missing_points > 0
         then TextIO.output(TextIO.stdErr, "Missing points: " ^
                            Int.toString (!missing_points) ^ "\n")
         else ()
       end

      fun output_activity (GPX.Activity { name, points }) =
        let val vxy = Vector.map (fn GPX.Pt { pos, ... } =>
                                  proj pos) points
        in
          printpolyline "#3333CC" vxy
        end

      fun output_segment (Segments.Segment { name, gates }) =
        let
          fun output_gate (a, b) =
            let
              val (ax, ay) = proj a
              val (bx, by) = proj b
            in
              printpolyline "#000000" (Vector.fromList [(ax, ay),
                                                        (bx, by)])
            end
        in
          Vector.app output_gate gates
        end
    in
      TextIO.output (f,
                     TextSVG.svgheader { x = 0, y = 0,
                                         width = width,
                                         height = height,
                                         generator = "makesegments.sml" });
      output_osm osm;
      app output_activity activities;
      app output_segment segments;

      TextIO.output (f, TextSVG.svgfooter ());
      TextIO.closeOut f;
      print ("Wrote " ^ filename ^ "\n")
    end

  fun main dirname =
    let
      val glob = FSUtil.dirplus dirname "*.gpx"
      (* Maybe should be in subdir? *)
      val segments = Segments.parse_file "segments.kml"
      val () = print ("Loaded " ^ Int.toString (length segments) ^
                      " segments.\n")
      val activities = map GPX.parse_file (FSUtil.globfiles glob)
      val () = print ("Loaded " ^ Int.toString (length activities) ^
                      " activities.\n")
    in
      genpic "debug.svg" (segments, activities);

      print "(unimplemented)\n"
    end
  handle GPX.GPX s => fail ("GPX error: " ^ s ^ "\n")
       | Segments.Segments s => fail ("Segments error: " ^ s ^ "\n")
       | MakeSegments s => fail ("MakeSegments error: " ^ s ^ "\n")

end

val () = Params.main1 ("Pass the name of a subdirectory with .gpx files on " ^
                       "the command line.") MakeSegments.main