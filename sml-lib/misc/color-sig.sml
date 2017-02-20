
signature COLOR =
sig

  (* Convert from HSV color space to RGB *)
  val hsvtorgbf : (real * real * real) -> (real * real * real)
  val hsvtorgb : (Word8.word * Word8.word * Word8.word) ->
                 (Word8.word * Word8.word * Word8.word)

  val rgbf : (real * real * real) -> (Word8.word * Word8.word * Word8.word)

  (* To HTML RRGGBB as a 6-character hexadecimal string, and back. *)
  val tohexstring : Word8.word * Word8.word * Word8.word -> string
  val fromhexstring : string -> (Word8.word * Word8.word * Word8.word) option
  (* eg "fE" to 0w254 *)
  val onefromhexstring : string -> Word8.word option

  (* rgbtolab (r, g, b)

     returns (l, a, b) after converting from the
     SRGB color space (with values nominally in [0, 1])
     to LAB (L is nominally in [0, 100]) *)
  val rgbtolab : (real * real * real) -> (real * real * real)

  (* CIE1994 distance between sample color Lab2 and reference Lab1.
     ** Careful: This may not even be symmetric! **
     Note: This has been superseded by an even more complicated function
     (CIEDE2000) if you are doing something very sensitive. *)
  val delta_e : (real * real * real) * (real * real * real) -> real
end
