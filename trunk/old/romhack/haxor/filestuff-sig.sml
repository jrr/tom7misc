
signature FILESTUFF =
sig

  type bytes = Word8Vector.vector

  (* load and save files *)

  val readfile : string -> bytes
  val writefile : string -> bytes -> unit

  (* modify a particular byte in a file *)
    
  val setbyte : string -> int -> Word8.word -> unit

end