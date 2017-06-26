(* modify (or add) ID3v1.1 "track number" tags according to a playlist
   (m3u) file *)

structure Main =
struct
    
  fun go f =
    let
      val fd = TextIO.openIn f
      fun loop n =
        let
            fun one n ff =
                let 
                    val { title, artist, album, 
                          genre, comment, year, ... } =
                        case ID3v11.readid3 ff of
                            SOME r => r
                          | NONE => { title=ff, artist="", album="",
                                      genre=0, comment="", track=0, 
                                      year="" }
                in
                    ID3v11.writecreateid3 ff
                      { title=title,
                        artist=artist,
                        album=album,
                        genre=genre,
                        comment=comment,
                        year=year,
                        track=n }
                end
        in
            case TextIO.inputLine fd of 
              NONE => ()
            | SOME "" => loop n
            | SOME line =>  
                case CharVector.sub(line, 0) of
                    #"#" => loop n
                  | _ =>
                    (* lose trailing whitespace *)
                    (case StringUtil.losespecr StringUtil.whitespec line of
                         (* ignore blank lines *)
                         "" => loop n
                       | s => (one n s;
                               loop (n + 1)))
        end
    in
        loop 1
    end

end

val _ =
  case CommandLine.arguments () of
    nil =>
      let in
          print "Supply a m3u playlist (or lists) on the command line.\n";
          print "Referenced MP3 files will be modified in place.\n\n"
      end
  | l => app Main.go l
