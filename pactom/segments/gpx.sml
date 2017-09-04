structure GPX :> GPX =
struct

  exception GPX of string

  datatype tree = datatype XML.tree

  datatype pt = Pt of { pos : LatLon.pos,
                        (* In meters *)
                        elev : real,
                        (* Since epoch *)
                        time : Time.time,
                        (* Degrees Celsius *)
                        temp : real,
                        heart_bpm : int,
                        (* Strides/min? *)
                        cadence : int }
  datatype activity =
    Activity of { name : string,
                  start : Time.time,
                  points : pt Vector.vector }

  fun parse_xml xml =
    let
      val name = ref "Untitled"

      val points : pt GrowArray.growarray = GrowArray.empty ()

      fun process (trkpt as Elem(("trkpt", attrs), _)) =
        let
          (*
          <trkpt lat="40.45568422414362430572509765625"
                 lon="-79.9284434504806995391845703125">
            <ele>289</ele>
            <time>2017-08-18T16:44:35.000Z</time>
            <extensions>
              <ns3:TrackPointExtension>
                <ns3:atemp>29.0</ns3:atemp>
                <ns3:hr>89</ns3:hr>
                <ns3:cad>80</ns3:cad>
              </ns3:TrackPointExtension>
            </extensions>
          </trkpt>
          *)
          fun realattr a =
            Option.mapPartial Real.fromString (XML.getattr attrs a)
          val pos = case (realattr "lat", realattr "lon") of
            (SOME lat, SOME lon) => LatLon.fromdegs { lat = lat, lon = lon }
          | _ => raise GPX "<trkpt> without valid lat and lon"


          val leaves = XML.getleaves trkpt
          fun findoneopt tag =
            case ListUtil.Alist.find op= leaves tag of
              NONE => NONE
            | SOME [v] => SOME v
            | SOME nil => raise GPX "impossible"
            | SOME _ =>
                raise GPX ("<trkpt> specified more than one " ^ tag)

          fun findone p tag =
            case findoneopt tag of
              NONE => raise GPX ("<trkpt> didn't specify " ^ tag)
            | SOME v =>
                case p v of
                  NONE => raise GPX ("<trkpt> has invalid " ^ tag ^ ": [" ^
                                     v ^ "]")
                | SOME vv => vv

          (* Allegedly ISO 8601, UTC.
             This just parses what I see in Garmin's actual export,
             which looks like this:
             2017-08-18T16:46:07.000Z *)
          fun parse_datetime s =
            let
              fun parse_date d =
                case map Int.fromString (String.fields
                                         (StringUtil.ischar #"-") d) of
                  [SOME y, SOME m, SOME d] => (y, m, d)
                | _ => raise GPX ("Expected ISO 8601 date YYYY-MM-DD: " ^ d)

              fun parse_time t =
                let
                  fun fail () =
                    raise GPX ("Expected ISO 8601 time HH:MM:SS.mmmZ: " ^ t)
                in
                  case StringUtil.removetail "Z" t of
                    SOME hms =>
                      (case String.fields (StringUtil.ischar #":") hms of
                         [h, m, sms] =>
                           (case String.fields (StringUtil.ischar #".") sms of
                              [s, ms] =>
                                (case map Int.fromString [h, m, s, ms] of
                                   [SOME h, SOME m, SOME s, SOME ms] =>
                                     (h, m, s, ms)
                                 | _ => fail ())
                            | _ => fail ())
                       | _ => fail ())
                  | NONE => fail ()
                end

              fun num_month 1 = Date.Jan
                | num_month 2 = Date.Feb
                | num_month 3 = Date.Mar
                | num_month 4 = Date.Apr
                | num_month 5 = Date.May
                | num_month 6 = Date.Jun
                | num_month 7 = Date.Jul
                | num_month 8 = Date.Aug
                | num_month 9 = Date.Sep
                | num_month 10 = Date.Oct
                | num_month 11 = Date.Nov
                | num_month 12 = Date.Dec
                | num_month n = raise GPX ("Bad month num: " ^ Int.toString n)
            in
              case String.fields (StringUtil.ischar #"T") s of
                [date, time] =>
                  let
                    val (year, month, day) = parse_date date
                    val (hour, minute, second, ms) = parse_time time
                    val date = Date.date
                      { year = year, month = num_month month, day = day,
                        hour = hour, minute = minute, second = second,
                        (* Always UTC. *)
                        offset = SOME Time.zeroTime }

                    val time = Date.toTime date
                  in
                    (* Add in milliseconds since Date.date doesn't support
                       that. *)
                    SOME (Time.+ (Time.fromMilliseconds (IntInf.fromInt ms),
                                  time))
                  end
                | _ => raise GPX ("Expected T in ISO 8601 time: " ^ s)
            end

          val elev = findone Real.fromString "ele"
          val time = findone parse_datetime "time"
          val temp = findone Real.fromString "ns3:atemp"
          val heart_bpm = findone Int.fromString "ns3:hr"
          val cadence = findone Int.fromString "ns3:cad"
        in
          GrowArray.append points (Pt { pos = pos,
                                        elev = elev,
                                        time = time,
                                        temp = temp,
                                        heart_bpm = heart_bpm,
                                        cadence = cadence })
        end
      | process (Elem(("name", nil), [Text nametext])) = name := nametext
      | process (Elem(_, children)) = app process children
      | process _ = ()

      (* Only consistency check so far is that the times are
         strictly increasing. *)
      fun check v =
        let
          val last_time = ref Time.zeroTime
        in
          Vector.app
          (fn Pt { time, ... } =>
           if Time.< (!last_time, time)
           then last_time := time
           else raise GPX ("Timestamps must be strictly increasing, " ^
                           "but got\n" ^
                           Time.toString (!last_time) ^ "\nfollowed by\n" ^
                           Time.toString time)) v
        end

      val () = process xml
      val points = GrowArray.vector points

      val start = if Vector.length points = 0
                  then raise GPX ("There were 0 waypoints in the activity " ^
                                  !name ^ "!")
                  else let val Pt { time, ... } = Vector.sub (points, 0)
                       in time
                       end
    in
      check points;
      Activity { name = !name,
                 start = start,
                 points = points }
    end

  fun parse_string contents =
    let val xml = XML.parsestring contents
      handle XML.XML s => raise GPX ("Malformed xml: " ^ s)
    in
      parse_xml xml
    end

  fun parse_file f = parse_string (StringUtil.readfile f)

end
