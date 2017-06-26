
val db = ChordDB.readdb "db.txt"

fun render s =
  Render.render (StringUtil.harden (StringUtil.charspec "A-Za-z0-9")
                 #"_" 12 s ^ ".mid") (Play.play db s)
