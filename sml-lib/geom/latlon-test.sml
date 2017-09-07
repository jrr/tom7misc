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

      val msg =
        ("Gnomonic centered at " ^ latlontos home ^ ":\n" ^
         "  Input pos: " ^ latlontos pos ^ "\n" ^
         "  Projected: " ^ ctos (x, y) ^ "\n" ^
         "  Inverse:   " ^ latlontos pos2 ^ "\n" ^
         "  Error:     " ^ Real.toString err ^ " meters\n")
    in
      (* XXX *)
      print msg;
      
      if err < 0.01
      then ()
      else raise LatLonTest msg
    end

  fun go () =
    let
      val home = LatLon.fromdegs { lat = 40.452911, lon = ~79.936313 }

      val hpos = LatLon.inverse_gnomonic home (0.0, 0.0)
      val err = LatLon.dist_meters (home, hpos)
      val () = if err < 0.01
               then ()
               else raise LatLonTest ("0,0 is not inverted back onto " ^
                                      "the tangent point? Got " ^
                                      latlontos hpos ^ ", expected " ^
                                      latlontos home)
    in
      gnomon home (LatLon.fromdegs { lat = 40.450, lon = ~79.936 });
      gnomon home (LatLon.fromdegs { lat = 40.0, lon = ~80.0 });
      gnomon home (LatLon.fromdegs { lat = 40.4, lon = ~79.5 });
      gnomon home (LatLon.fromdegs { lat = 40.443438, lon = ~79.790413 });
      gnomon home (LatLon.fromdegs { lat = 41.430454, lon = ~72.890565 });
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
