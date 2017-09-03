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

  (* Find crossings of segments in the activities.

     A crossing is a minimal subsequence of an activity that passes
     through the gates in order. *)
  fun find_crossings (segments, activities) =
    let
      (* XXX do something with them! *)
      fun emit (Segments.Segment { name, ... },
                GPX.Activity { name = activity_name, ... },
                start_idx, end_idx) =
        print ("Segment " ^ name ^ " crossed in activity " ^
               activity_name ^ ": " ^
               Int.toString start_idx ^ " to " ^
               Int.toString end_idx ^ "\n")

      (* Because we have to cross gates in order, gate matching is
         done with a state machine. As we run through an activity
         we can have a number of active gatestates. Each one tells
         us the last time we ran through the start gate, and how
         many of the gates we've correctly passed through since that.

         If we've been through gates 0 (the start gate), 1, and 2,
         then the gatestate records the time we went through gate 0,
         and the index 2. When we pass through gate 3 we can advance
         that index to 3. If instead we see gate 4, we ignore it (we
         may pass through it again later). If we see gate 1 again,
         there's no point in doing anything, since it just makes us
         ineligible to pass through gate 3 next and doesn't affect
         our time (which is determined by the passage through gate 0).
         If we see gate 0, we start a new gatestate (since this DOES
         affect our time). However, it supersedes any gatestate
         currently at index 0.

         As a result of all that, there is at one most gatestate per
         gate index. So we just store a flat array, parallel with
         the gates (XXX minus one?). *)

      (* Index of the first element of the pair of adjacent points
         where we passed the START gate. *)
      type gatestate = int

      val segments = Vector.fromList segments
      val activities = Vector.fromList activities

      (* Does the running vector pos1 -> pos2 cross the gate (a, b)? *)
      fun doescross ((a, b), (pos1, pos2)) =
        let
          (* Though the intersection point is not going to be exactly
             correct, I'm pretty sure that the fact of intersection
             is preserved when treating these as planar (poles and
             meridian notwithstanding). *)
          fun topt pos = let val { lon, lat } = LatLon.todegs pos
                         in (lon, lat)
                         end
        in
          (* XXXXXX also check the direction of crossing is correct! *)
          case Geom.intersection { seg1 = (topt a, topt b),
                                   seg2 = (topt pos1, topt pos2) } of
            NONE => false
          | SOME _ => true
        end

      fun doactivity activity =
        let
          fun makestates i : gatestate option Array.array =
            let val Segments.Segment { gates, ... } = Vector.sub (segments, i)
            in Array.array (Vector.length gates, NONE)
            end
          val gatestates = Vector.tabulate (Vector.length segments, makestates)

          (* Get the gates crossed (in the accepting direction) as the
             runner passes from pos1 to pos2.
             PERF: This can be much more efficient using Quadtree; almost
             no pairs of waypoints cross any gates. *)
          fun advancegates (act_idx,
                            GPX.Pt { pos = pos1, ... },
                            GPX.Pt { pos = pos2, ... }) =
            Util.for 0 (Vector.length segments - 1)
            (fn segidx =>
             let
               val segment as Segments.Segment { gates, name } =
                 Vector.sub (segments, segidx)
               val num_gates = Vector.length gates
             in
               Util.for 0 (num_gates - 1)
               (fn gateidx =>
                let
                  val gate = Vector.sub (gates, gateidx)
                in
                  if doescross (gate, (pos1, pos2))
                  then
                    let val states = Vector.sub (gatestates, segidx)
                    in
                      (* First gate is special. It always overwrites
                         the first slot (see discussion above). *)
                      if gateidx = 0
                      then
                        let in
                          print ("[" ^ name ^ "] Gate 0 crossed @" ^
                                 Int.toString act_idx ^ "\n");
                          Array.update (states, 0, SOME act_idx)
                        end
                      else
                        (case Array.sub (states, gateidx - 1) of
                           NONE => ()
                         | SOME entry =>
                             let in
                               print ("[" ^ name ^ "] Gate " ^
                                      Int.toString (gateidx - 1) ^ " -> " ^
                                      Int.toString gateidx ^ " @" ^
                                      Int.toString act_idx ^ "\n");
                               (* Advance this cursor to the next gate *)
                               Array.update (states, gateidx - 1, NONE);
                               (* If it was actually the last gate,
                                  we can emit it. *)
                               if gateidx = num_gates - 1
                               then emit (segment, activity, entry, act_idx)
                               else Array.update (states, gateidx, SOME entry)
                             end)
                    end
                  else ()
                end)
             end)

          val GPX.Activity { points, ... } = activity
        in
          if Vector.length points >= 2
          then
            Util.for 0 (Vector.length points - 2)
            (fn act_idx =>
             advancegates (act_idx,
                           Vector.sub (points, act_idx),
                           Vector.sub (points, act_idx + 1)))
          else ()
        (* TODO: Debug message about outstanding gatestates *)
        end
    in
      Vector.app doactivity activities
    end

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

      find_crossings (segments, activities);

      print "(unimplemented)\n"
    end
  handle GPX.GPX s => fail ("GPX error: " ^ s ^ "\n")
       | Segments.Segments s => fail ("Segments error: " ^ s ^ "\n")
       | MakeSegments s => fail ("MakeSegments error: " ^ s ^ "\n")

end

val () = Params.main1 ("Pass the name of a subdirectory with .gpx files on " ^
                       "the command line.") MakeSegments.main
