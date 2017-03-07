structure Solve =
struct
  infixr 9 `
  fun a ` b = a b

  fun w16tow8s (w : Word16.word) : Word8.word * Word8.word =
    let
      val wh = Word8.fromInt ` Word16.toInt `
        Word16.andb(Word16.>>(w, 0w8), Word16.fromInt 0xFF)
      val wl = Word8.fromInt ` Word16.toInt `
        Word16.andb(w, Word16.fromInt 0xFF)
    in
      (wh, wl)
    end

  val PRINT_LOW : Word8.word = 0wx20
  val PRINT_HIGH : Word8.word = 0wx7e
  fun printable p = p >= PRINT_LOW andalso p <= PRINT_HIGH
  fun printable16 w =
    let val (a, b) = w16tow8s w
    in printable a andalso printable b
    end

  fun solveseg (addr : int) =
    let
      (* find some SEG:OFF such that SEG + OFF = ADDR,
         and also SEG/OFF are printable. *)
      val start = addr div 16 - 0x10000

      fun next seg =
        let
          val off = addr - seg * 16
        in
          (* print ("Try: " ^ Int.toString seg ^ ":" ^ Int.toString off ^ "\n"); *)
          if seg < 0 orelse seg > 0xFFFF orelse
             off < 0 orelse off > 0xFFFF
          then next (seg + 1)
          else
            let
              val segw = Word16.fromInt seg
              val offw = Word16.fromInt off
            in
              if printable16 segw andalso printable16 offw
              then print ("OK: " ^ Word16.toString segw ^ ":" ^ Word16.toString offw ^ "\n")
              else next (seg + 1)
            end
        end
    in
      next start
    end

end
