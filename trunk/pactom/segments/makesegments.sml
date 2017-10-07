structure MakeSegments =
struct

  exception MakeSegments of string

  fun fail s =
    let in
      TextIO.output (TextIO.stdErr, s);
      OS.Process.exit OS.Process.failure
    end

  structure Config :>
  sig
    type config

    val home : config -> LatLon.pos
    val gnomonic : config -> LatLon.projection
    val inv_gnomonic : config -> LatLon.inverse_projection

    val fromfile : string -> config
  end =
  struct
    datatype config = C of { home : LatLon.pos,
                             gnomonic : LatLon.projection,
                             inv_gnomonic : LatLon.inverse_projection }

    fun home (C { home, ... }) = home
    fun gnomonic (C { gnomonic, ... }) = gnomonic
    fun inv_gnomonic (C { inv_gnomonic, ... }) = inv_gnomonic

    fun fromfile s =
      let val { lookup, alist = _ } = Script.alistfromfile s
      in
        case Option.mapPartial LatLon.fromstring (lookup "home") of
          NONE => raise MakeSegments "Need in config.txt:\nhome lat,lon"
        | SOME pos =>
            C { home = pos,
                gnomonic = LatLon.gnomonic pos,
                inv_gnomonic = LatLon.inverse_gnomonic pos }
      end
  end

  datatype segment = datatype Segments.segment
  datatype activity = datatype GPX.activity
  datatype pt = datatype GPX.pt

  fun postos p =
    let val { lon, lat } = LatLon.todegs p
    in Real.toString lon ^ "," ^ Real.toString lat
    end

  (* Interpolated index (int and fraction) to string. *)
  fun idxtos (idx, f) =
    if Real.isNan f orelse f < 0.0 orelse f > 1.0
    then Int.toString idx ^ ".BAD:" ^ Real.toString f
    else Real.fmt (StringCvt.FIX (SOME 4)) (real idx + f)

  (* Find crossings of segments in the activities.

     A crossing is a minimal subsequence of an activity that passes
     through the gates in order. The same activity can have many
     crossings of a segment, even overlapping ones.

     Returns a list of segments, each with a non-empty list of
     crossings (activity, waypoint start index, waypoint end index).
     TODO: include weight for interpolating start and end indices *)
  fun find_crossings conf (segments, activities) =
    let
      (* PERF: Make the debug printing efficiently optional *)
      val gnomonic = Config.gnomonic conf
      val inv_gnomonic = Config.inv_gnomonic conf

      val segments = Vector.fromList segments
      val activities = Vector.fromList activities

      (* Accumulated crossings for each segment. *)
      type crossing = GPX.activity * (int * real) * (int * real)
      val crossings = Array.array (Vector.length segments,
                                   nil : crossing list)

      fun emit (seg_idx,
                act as (Activity { name = activity_name, ... }),
                starti : int * real, endi : int * real) =
        let
          val Segment { name, ... } =
            Vector.sub (segments, seg_idx)
          val prev = Array.sub (crossings, seg_idx)
        in
          Array.update (crossings, seg_idx, (act, starti, endi) :: prev);
          print ("Segment " ^ name ^ " crossed in activity " ^
                 activity_name ^ ": " ^
                 idxtos starti ^ " to " ^
                 idxtos endi  ^ "\n")
        end

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
         currently at index 0. So we only need to keep track of the
         latest one.

         As a result of all that, there is at one most gatestate per
         gate index. So we just store a flat array, parallel with
         the gates, except for the unnecessary final gate. *)

      (* Index of the first element of the pair of adjacent points where we
         passed the START gate, along with the interpolation fraction
         between that event and the next one (see intersection_frac). *)
      type gatestate = int * real

      (* XXXXXX also check the direction of crossing is correct! *)
      (* Does the running vector pos1->pos2 cross the gate (a, b)?
         When we cross a gate, the two points may be pretty far
         apart (especially when the GPS device merges collinear
         points). Also compute the fraction (in 0, 1) of the
         segment pos1->pos2 that yields the approximate intersection
         point. This allows us to compute more precise times (etc.)
         by interpolating between pos1 and pos2. *)
      fun intersection_frac ((a, b), (pos1, pos2)) =
        let
          (* PERF: We could do a quick test treating the positions
             as euclidean... *)
          (* Convert to gnomonic projection. All great circles--such
             as the ones we're testing--are straight lines. *)
          fun toxy pos = gnomonic pos
          val (aa, bb) = (toxy a, toxy b)
          val (ppos1, ppos2) = (toxy pos1, toxy pos2)
        in
          (* Now, compute their intersection point, which corresponds
             to the correct point on the geodesic. *)
          case Geom.intersection { seg1 = (aa, bb),
                                   seg2 = (ppos1, ppos2) } of
          NONE => NONE
        | SOME (iix, iiy) =>
            let
              (* Inverse gnomonic projection gives us the actual
                 position where the two great circles intersect. *)
              val isect_pt = inv_gnomonic (iix, iiy)
              val plen = LatLon.dist_meters (pos1, pos2)
              val ilen = LatLon.dist_meters (pos1, isect_pt)

              val f = ilen / plen
            in
              if f < 0.0 orelse f > 1.0
              then raise MakeSegments ("bad intersection for\n" ^
                                       postos pos1 ^ " -> " ^ postos pos2 ^
                                       "\n through gate\n" ^
                                       postos a ^ " -- " ^ postos b ^
                                       "\n yielding intersection at\n" ^
                                       postos isect_pt ^
                                       "\n and f=" ^ Real.toString f)
              else SOME f
            end
        end

      fun doactivity activity =
        let
          val Activity { name = activity_name,
                         start = activity_start, ... } = activity
          val () = print
            (" --- " ^ activity_name ^ " (" ^
             Date.toString (Date.fromTimeLocal activity_start) ^
             ") ---\n")

          fun makestates i : gatestate option Array.array =
            let val Segment { gates, ... } = Vector.sub (segments, i)
            in Array.array (Vector.length gates - 1, NONE)
            end
          val gatestates = Vector.tabulate (Vector.length segments, makestates)

          (* Get the gates crossed (in the accepting direction) as the
             runner passes from pos1 to pos2.
             PERF: This can be much more efficient using Quadtree; almost
             no pairs of waypoints cross any gates. *)
          fun advancegates (act_idx,
                            Pt { pos = pos1, ... },
                            Pt { pos = pos2, ... }) =
            Util.for 0 (Vector.length segments - 1)
            (fn segidx =>
             let
               val segment as Segment { gates, name } =
                 Vector.sub (segments, segidx)
               val num_gates = Vector.length gates
             in
               Util.for 0 (num_gates - 1)
               (fn gateidx =>
                let
                  val gate = Vector.sub (gates, gateidx)
                  fun didcross frac =
                    let val states = Vector.sub (gatestates, segidx)
                    in
                      (* First gate is special. It always overwrites
                         the first slot (see discussion above). *)
                      if gateidx = 0
                      then
                        let
                          val re = case Array.sub (states, 0) of
                            SOME _ => "re-"
                          | NONE => ""
                        in
                          print ("[" ^ name ^ "] Gate 0 " ^ re ^ "crossed @" ^
                                 idxtos (act_idx, frac) ^ "\n");
                          Array.update (states, 0, SOME (act_idx, frac))
                        end
                      else
                        (case Array.sub (states, gateidx - 1) of
                           NONE =>
                             let in
                               print ("[" ^ name ^ "] Gate " ^
                                      Int.toString gateidx ^ " @" ^
                                      idxtos (act_idx, frac) ^
                                      " but no predecessor\n")
                             end
                         | SOME entry =>
                             let in
                               print ("[" ^ name ^ "] Gate " ^
                                      Int.toString (gateidx - 1) ^ " -> " ^
                                      Int.toString gateidx ^ " @" ^
                                      idxtos (act_idx, frac) ^ "\n");
                               (* Advance this cursor to the next gate *)
                               Array.update (states, gateidx - 1, NONE);
                               (* If it was actually the last gate,
                                  we can emit it. *)
                               if gateidx = num_gates - 1
                               then emit (segidx, activity, entry,
                                          (act_idx, frac))
                               else Array.update (states, gateidx, SOME entry)
                             end)
                    end
                in
                  case intersection_frac (gate, (pos1, pos2)) of
                    SOME f => didcross f
                  | NONE => ()
                end)
             end)

          val Activity { points, ... } = activity

          (* When we reach the end of the activity, print any outstanding
             gatestates. *)
          fun print_unfinished () =
            Vector.appi
            (fn (seg_idx, Segment { name, ... }) =>
             let
               fun onestate (_, NONE) = ()
                 | onestate (gate_idx, SOME (act_idx, act_f)) =
                 print ("[" ^ name ^ "] Unfinished (last gate " ^
                        Int.toString gate_idx ^ ") @" ^
                        idxtos (act_idx, act_f) ^ "\n")
             in
               Array.appi onestate (Vector.sub (gatestates, seg_idx))
             end) segments
        in
          if Vector.length points >= 2
          then
            Util.for 0 (Vector.length points - 2)
            (fn act_idx =>
             advancegates (act_idx,
                           Vector.sub (points, act_idx),
                           Vector.sub (points, act_idx + 1)))
          else ();
          print_unfinished ()
        end

      val () = Vector.app doactivity activities

      (* Now collate each segment with crossings (if any) and put
         them back in the original order. *)
      val out = ref (nil : (segment * crossing list) list)
    in
      Vector.appi
      (fn (seg_idx, segment) =>
       case Array.sub (crossings, seg_idx) of
         nil => ()
       | c => out := (segment, rev c) :: !out) segments;
      rev (!out)
    end

  fun genpic conf filename (segments, activities) =
    let
      val f = TextIO.openOut filename
      val gnomonic = Config.gnomonic conf

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

      fun proj p =
        let
          val (x, y) = gnomonic p
        in
          (* Scale up massively, and invert Y axis. *)
          (2400000.0 * x, ~2400000.0 * y)
        end

      fun observepoint (pos : LatLon.pos) =
        Bounds.boundpoint bounds (proj pos)
      fun observesegment (Segment { gates, ... }) =
        Vector.app (fn (a, b) =>
                    let in
                      observepoint a;
                      observepoint b
                    end) gates
      fun observeactivity (Activity { points, ... }) =
        Vector.app (fn Pt { pos, ... } => observepoint pos) points

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
          TextIO.output (f, "\"/>\n")
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
                          "stroke-width=\".3\" points=\"");
            Vector.app onepoint pts;
            TextIO.output (f, "\" />\n")
          end
       in
         Vector.app onestreet streets;
         if !missing_points > 0
         then TextIO.output(TextIO.stdErr, "Missing points: " ^
                            Int.toString (!missing_points) ^ "\n")
         else ()
       end

      fun output_activity (Activity { points, ... }) =
        let val vxy = Vector.map (fn Pt { pos, ... } =>
                                  proj pos) points
        in
          printpolyline "#3333CC" vxy
        end

      fun output_segment (Segment { name, gates }) =
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

  (* For some crossing of a segment, compute various data about it. *)
  datatype enriched_crossing =
    E of { seconds_before : real,
           seconds : real,
           miles_before : real,
           miles : real,
           total_beats : real,
           hr_start : real,
           hr_end : real
           (* XXX more! *) }
  fun enrich_crossing (Segment { ... })
                      (Activity { points, ... }, starti, endi) =
    let
      (* Sum some quantity over an interval that ends at arbitrary
         positions within the activity.

         f computes a quantity for two adjacent activity points.

         Points are represented as an index into the activity
         points (integral; can't be the last one) and an intepolant
         (real; between 0 and 1). *)
      fun sum f ((idx1, f1), (idx2, f2)) =
        if idx1 < 0 orelse
           idx1 >= idx2 orelse
           idx2 >= Vector.length points - 1
        then raise MakeSegments "enrich_crossing: bad points"
        else
          let
            fun acc (v, idx) =
              if idx > idx2 then v
              else
                let
                  val p1 = Vector.sub (points, idx)
                  val p2 = Vector.sub (points, idx + 1)
                  val vv = f (p1, p2)
                  (* Attenuate if it's the start or end point. *)
                  val vv = if idx = idx1
                           then (1.0 - f1) * vv
                           else if idx = idx2
                                then f2 * vv
                                else vv
                in
                  acc (v + vv, idx + 1)
                end
          in
            acc (0.0, idx1)
          end
      fun get_sec (Pt { time = time1, ... },
                   Pt { time = time2, ... }) =
        Time.toReal (Time.- (time2, time1))
      fun get_miles (Pt { pos = pos1, ... },
                     Pt { pos = pos2, ... }) =
        LatLon.dist_miles (pos1, pos2)

      fun get_beats (Pt { time = time1, heart_bpm = h1, ... },
                     Pt { time = time2, heart_bpm = h2, ... }) =
        let
          val sec = Time.toReal (Time.- (time2, time1))
          val avg_bps = ((real h1 / 60.0) + (real h2 / 60.0)) * 0.5
        in
          avg_bps * sec
        end

      (* No interpolation here; starting or stopping causes HR to change so
         the endpoints themselves are probably more what we want. *)
      fun pt_hr idx =
        let val Pt { heart_bpm, ... } = Vector.sub (points, idx)
        in real heart_bpm
        end

      val zeroi = (0, 0.0)

    in
      E { seconds = sum get_sec (starti, endi),
          seconds_before = sum get_sec (zeroi, starti),
          miles = sum get_miles (starti, endi),
          miles_before = sum get_miles (zeroi, starti),
          total_beats = sum get_beats (starti, endi),
          hr_start = pt_hr (#1 starti),
          hr_end = pt_hr (#1 endi + 1)
          }
    end

  (* Sort activities in chronological order. *)
  fun comparebydate (Activity { start = a, ... },
                     Activity { start = b, ... }) =
    Time.compare (a, b)

  fun comparecrossing ((acta, starta, enda),
                       (actb, startb, endb)) =
    let
      fun compare_pt ((idxa, fa), (idxb, fb)) =
        case Int.compare (idxa, idxb) of
          EQUAL => Real.compare (fa, fb)
        | order => order
    in
      case comparebydate (acta, actb) of
        EQUAL => (case compare_pt (starta, startb) of
                    EQUAL => compare_pt (enda, endb)
                  | order => order)
      | order => order
    end

  fun enrich_crossings crossings =
    let fun one (seg, crlist) =
      let val crlist = ListUtil.sort comparecrossing crlist
      in (seg, ListUtil.mapto (enrich_crossing seg) crlist)
      end
    in
      map one crossings
    end

  (* Print a summary of the crossings. *)
  fun dosegment_crossings (seg as Segment { name, ... }, crlist) =
    let
      val rtos = Real.fmt (StringCvt.FIX (SOME 4))
      fun dtos d = Date.fmt "%d %b %y" d
    in
      print ("Crossings of " ^ name ^ ":\n");
      app (fn ((Activity { name = activity_name, start, ... },
                starti, endi), E { seconds, total_beats, ... }) =>
           let val date = Date.fromTimeLocal start
           in
             print ("  " ^ dtos date ^ " @" ^
                    idxtos starti ^ "-" ^
                    idxtos endi ^ "  " ^
                    rtos seconds ^ "s " ^
                    rtos total_beats ^ "b" ^
                    "\n")
           end) crlist
    end

  local val okspec = StringUtil.charspec "A-Za-z0-9_"
  in
    fun filebase name =
      CharVector.map (fn c =>
                      if okspec c
                      then c
                      else #"_") name
  end

  (* Graph elevation vs time and mark the end point.
     TODO: Mark start and end points
     TODO: Start all of them at the same x coordinate
     TODO: Absolute elevation seems to change a lot
     from day to day (I guess because of prevailing
     barometric pressure?). So maybe should just use
     the deltas.
     *)
  fun elevgraph (Segment { name, ... }, crlist) =
    let
      val filename = filebase name ^ "-elev.svg"
      (* Bounds for graph data. *)
      val bounds = Bounds.nobounds ()

      val lines = ref nil
      fun onecrossing ((Activity { points, ... }, starti, endi),
                       E { hr_start, hr_end, ...}) =
        (* Emit all the points in starti-endi, including the
           interpolated but excluded endpoints. Accumulate the
           time since start (x axis); y axis is elevation. *)
        let
          (* Start time. In the general case this is AFTER the
             first point we emit. *)
          val tstart =
            let
              (* first two points *)
              val (idx, f) = starti
              val Pt { time = tstart, ... } =
                Vector.sub (points, idx)
              val Pt { time = tstart1, ...} =
                Vector.sub (points, idx + 1)

              (* Time is actually integral, so do the subsecond
                 calculation in microseconds *)
              val subsec = Time.toReal (Time.-(tstart1, tstart)) * f
              val toffset = Time.fromMicroseconds
                (IntInf.fromInt (Real.trunc (subsec * 1000000.0)))
            in
              Time.+ (tstart, toffset)
            end

          val (endidx, endf) = endi
          fun go i =
            if i = endidx + 1 then nil
            else
              let
                val Pt { elev, time, ... } = Vector.sub (points, i)
                val seconds = Time.toReal (Time.-(time, tstart))
              in
                (* Reverse elevation so that increasing Y is up.
                   XXX do this a better way. *)
                (seconds, 0.0 - elev) :: go (i + 1)
              end
        in
          lines := go (#1 starti) :: !lines
        end

      val () = app onecrossing crlist
      val () = app (app (Bounds.boundpoint bounds)) (!lines)
      val f = TextIO.openOut filename

      val () = Bounds.addmarginfrac bounds 0.05;
      (* Screen coordinates *)
      val WIDTH = 1000
      val HEIGHT = 1000
      val scaler = Bounds.stretch bounds (real WIDTH, real HEIGHT)

      fun svgline l =
        let in
          TextIO.output (f, "<polyline fill=\"none\" stroke=\"#000\" " ^
                         "opacity=\".5\" stroke-width=\".3\" points=\"");
          app (fn pt =>
               let val (x, y) = Bounds.scale scaler pt
               in
                 TextIO.output (f, TextSVG.rtos x ^ "," ^
                                TextSVG.rtos y ^ " ")
               end) l;
          TextIO.output (f, "\" />\n")
        end
    in
      TextIO.output (f, TextSVG.svgheader { x = 0,
                                            y = 0,
                                            width = WIDTH,
                                            height = HEIGHT,
                                            generator = "makesegments" });
      app svgline (!lines);
      TextIO.output (f, TextSVG.svgfooter ());
      TextIO.closeOut f;
      print ("Wrote " ^ filename ^ "...\n")
    end

  fun crossings_tsv (Segment { name, ... }, crlist) =
    let
      val rtos = Real.fmt (StringCvt.FIX (SOME 6))
      val filename = filebase name ^ ".tsv"
      val f = TextIO.openOut filename

      (* val () = TextIO.output (f, name ^ "\n") *)
      val () = TextIO.output (f,
                              "date\t" ^
                              "sec\t" ^
                              "total beats\t" ^
                              "miles\t" ^
                              "sec before\t" ^
                              "miles before\t" ^
                              "hr start\t" ^
                              "hr end\t" ^
                              "\n")

      fun onerow ((Activity { name = activity_name, start, ... },
                   starti, endi), E { seconds, total_beats, miles,
                                      seconds_before, miles_before,
                                      hr_start, hr_end, ... }) =
        let
          (* XXX no, get it by interpolating starti
             (this code is in elevgraph; factor it out -- or actually,
             it could just go in the enrichment) *)
          val start = Date.fromTimeLocal start
        in
          TextIO.output (f,
                         Date.toString start ^ "\t" ^
                         rtos seconds ^ "\t" ^
                         rtos total_beats ^ "\t" ^
                         rtos miles ^ "\t" ^
                         rtos seconds_before ^ "\t" ^
                         rtos miles_before ^ "\t" ^
                         rtos hr_start ^ "\t" ^
                         rtos hr_end ^ "\t" ^
                         "\n")
        end
    in
      app onerow crlist;
      print ("Wrote " ^ filename ^ "\n");
      TextIO.closeOut f
    end

  fun main dirname =
    let
      fun maybe_parse_file f =
        SOME (GPX.parse_file f) handle GPX.GPX msg =>
          let in
            print ("Couldn't parse " ^ f ^ ": " ^ msg ^ "\n");
            NONE
          end

      val conf = Config.fromfile (FSUtil.dirplus dirname "config.txt")

      val glob = FSUtil.dirplus dirname "*.gpx"
      (* Maybe should be in subdir? *)
      val segments = Segments.parse_file "segments.kml"
      val () = print ("Loaded " ^ Int.toString (length segments) ^
                      " segments.\n")
      val activities = List.mapPartial maybe_parse_file (FSUtil.globfiles glob)
      val () = print ("Loaded " ^ Int.toString (length activities) ^
                      " activities.\n")
      val activities = ListUtil.sort comparebydate activities
      val () = genpic conf "debug.svg" (segments, activities)
      val crossings = find_crossings conf (segments, activities)
      val crossings = enrich_crossings crossings
    in
      app dosegment_crossings crossings;
      app crossings_tsv crossings;
      app elevgraph crossings
    end
  handle GPX.GPX s => fail ("GPX error: " ^ s ^ "\n")
       | Segments.Segments s => fail ("Segments error: " ^ s ^ "\n")
       | MakeSegments s => fail ("MakeSegments error: " ^ s ^ "\n")

end

val () = Params.main1 ("Pass the name of a subdirectory with .gpx files on " ^
                       "the command line.") MakeSegments.main
