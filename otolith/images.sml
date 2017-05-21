structure Images =
struct

  val pxfont = Image.loadimage "pxfont.png"

  val tinymouse = Image.loadimage "tinymouse.png"
  val tiniestmouse = Image.loadimage "tiniestmouse.png"

  val mapcell = Image.loadimage "mapcell.png"
  val mapcellnone = Image.loadimage "mapcellnone.png"

  (* val runleft = loadanim ("run", ".png", 1, 12) *)

  val personr = Image.loadimage "persontest.png"
  val personl = Image.fliph personr

end
