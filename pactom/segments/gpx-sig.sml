(* Parse GPX (GPS Exchange XML) files, specifically the ones
   exported by Garmin Connect in 2017.

   We keep basically everything good that's in there from a Forerunner
   935 as of 19 Aug 2017. *)
signature GPX =
sig

  exception GPX of string

  datatype pt = Pt of { pos : LatLon.pos,
                        (* In meters *)
                        elev : real,
                        (* Since epoch *)
                        time : Time.time,
                        (* Degrees Celsius *)
                        temp : real,
                        heart_bpm : int,
                        (* Pairs of strides per minute *)
                        cadence : int }
  datatype activity =
    Activity of { name : string,
                  (* Time of the first point. *)
                  start : Time.time,
                  points : pt Vector.vector }

  (* Raises GPX on parse error *)
  val parse_file : string -> activity
  val parse_string : string -> activity

end
