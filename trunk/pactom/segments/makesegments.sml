structure MakeSegments =
struct

  exception MakeSegments of string

  fun fail s =
    let in
      TextIO.output (TextIO.stdErr, s);
      OS.Process.exit OS.Process.failure
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
      print "(unimplemented)\n"
    end
  handle GPX.GPX s => fail ("GPX error: " ^ s ^ "\n")
       | Segments.Segments s => fail ("Segments error: " ^ s ^ "\n")
       | MakeSegments s => fail ("MakeSegments error: " ^ s ^ "\n")


end

val () = Params.main1 ("Pass the name of a subdirectory with .gpx files on " ^
                       "the command line.") MakeSegments.main