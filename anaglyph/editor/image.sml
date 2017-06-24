structure Image :> IMAGE =
struct

  exception Image of string

  type image = int * int * Word32.word Array.array
  (* XXX should have different animation modes,
     wait on frame, etc. Should maybe be described by
     some metadata file? *)
  type anim = image Vector.vector

  fun loadimage f =
    case SDL.loadimage f of
      NONE => (print ("Couldn't load " ^ f ^ "\n");
               raise Image f)
    | SOME surf =>
        let
          (* XXX This gets the pixel order wrong. *)
          val (width, height, pixels) = SDL.pixels32 surf
        in
          print ("Loaded " ^ f ^ " (" ^
                 Int.toString width ^
                 "x" ^
                 Int.toString height ^
                 ")\n");
          SDL.freesurface surf;
          (width, height, pixels)
        end

  fun loadanim (base, ext, lo, hi) : anim =
     Vector.tabulate (hi - lo + 1,
                      fn i => loadimage (base ^
                                         Int.toString (lo + i) ^
                                         ext))

  fun numframes (v : anim) : int = Vector.length v
  fun getframe (v : anim, i) : image = Vector.sub (v, i)

  fun fliph (width, height, pixels) =
    (width, height,
     Array.tabulate (Array.length pixels,
                     fn i =>
                     let
                       val x = i mod width
                       val y = i div width
                       val oldi = y * width +
                         (width - x) - 1
                     in
                       Array.sub (pixels, oldi)
                     end))

end
