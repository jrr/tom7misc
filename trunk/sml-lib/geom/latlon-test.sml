structure LatLonTest =
struct

  exception LatLonTest of string

  fun ctos (x, y) = Real.toString x ^ "," ^ Real.toString y
  fun latlontos pos =
    let val { lat, lon } = LatLon.todegs pos
    in
      "lon=" ^ Real.toString lon ^ ",lat=" ^ Real.toString lat
    end

  (* Test round trip of gnomonic projection. *)
  fun gnomon home pos =
    let
      val proj = LatLon.gnomonic home
      val inv = LatLon.inverse_gnomonic home

      val (x, y) = proj pos
      val pos2 = inv (x, y)

      val err = LatLon.dist_meters (pos, pos2)
    in
      if err < 0.01
      then ()
      else raise LatLonTest("Gnomonic centered at " ^ latlontos home ^ ":\n" ^
                            "  Input pos: " ^ latlontos pos ^ "\n" ^
                            "  Projected: " ^ ctos (x, y) ^ "\n" ^
                            "  Inverse:   " ^ latlontos pos2 ^ "\n" ^
                            "  Error:     " ^ Real.toString err ^ " meters")
    end

  fun go () =
    let
      val home = LatLon.fromdegs { lat = 40.452911, lon = ~79.936313 }
    in
      gnomon home (LatLon.fromdegs { lat = 40.0, lon = ~80.0 });
      gnomon home (LatLon.fromdegs { lat = 40.4, lon = ~79.5 });
      gnomon home home;
      ()
    end
  handle LatLonTest s =>
    let in
      print "FAILED:\n";
      print s;
      (* OS.Process.exit OS.Process.failure *)
      raise (LatLonTest s)
    end
end

val () = LatLonTest.go ()
