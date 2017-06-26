
val title = Params.param "@" (SOME ("-title",
                                    "set the title (@ = retain existing)")) "title"

val artist = Params.param "@" (SOME ("-artist",
                                    "set the artist")) "artist"

val album = Params.param "@" (SOME ("-album",
                                    "set the album")) "album"

val year = Params.param "@" (SOME ("-year",
                                    "set the year")) "year"

val comment = Params.param "@" (SOME ("-comment",
                                    "set the comment")) "comment"
                              
val track = Params.param "@" (SOME ("-track",
                                    "set the track")) "track"

val genre = Params.param "@" (SOME ("-genre",
                                    "set the genre (as a number)")) "genre"

val _ =
    case Params.docommandline () of
        nil =>
            let in
                print "usage: id3tool [options] files...\n";
	 	print "(modifies id3v11 tags in MP3 files.)\n";
                print (Params.usage ())
            end
      | files =>
            let 
                fun dofile f =
                    let
                        val _ = print (f ^ "...\n");

                        val old = 
                            case ID3v11.readid3 f of 
                                SOME r => r
                              | NONE => { title = "",
                                          artist = "",
                                          album = "",
                                          year = "",
                                          comment = "",
                                          track = 0,
                                          genre = 0 }

                        (* now map across all fields *)
                        val dat =
                            { title =
                              if !title <> "@" then !title
                              else #title old,
                              artist =
                              if !artist <> "@" then !artist
                              else #artist old,
                              album =
                              if !album <> "@" then !album
                              else #album old,
                              year =
                              if !year <> "@" then !year
                              else #year old,
                              comment =
                              if !comment <> "@" then !comment
                              else #comment old,

                              track =
                              if !track <> "@" then
                                 Params.asint (#track old) track
                              else #track old,

                              genre =
                              if !genre <> "@" then
                                 Params.asint (#genre old) genre
                              else #genre old }


                        val _ =
                            let 
                                val { title, artist, album, year, 
                                      comment, track, genre } = dat
                            in
                                print ("title: " ^ title ^ "\n");
                                print ("artist: " ^ artist ^ "\n");
                                print ("album: " ^ album ^ "\n");
                                print ("year: " ^ year ^ "\n");
                                print ("comment: " ^ comment ^ "\n");
                                print ("track: " ^ Int.toString track ^ "\n");
                                print ("genre: " ^ Int.toString genre ^ "\n")
                            end
                                
                    in
                        ID3v11.writecreateid3 f dat
                    end
            in
                app dofile files
            end
