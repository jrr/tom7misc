
(* utilities for stream package *)

signature STREAMUTIL =
sig

    (* converts a string to a char stream *)
    val stostream : string -> char Stream.stream

    (* convert a file to a char stream *)
    val ftostream : string -> char Stream.stream

end