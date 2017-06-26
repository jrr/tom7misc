(* Chris Okasaki / Robert Harper
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
*)

signature POS =
  (* positions within a file (for reporting errors) *)
sig

  type pos

  val markstream : char Stream.stream -> (char * pos) Stream.stream

  val initpos  : pos
  val nextchar : pos -> pos
  val nextline : pos -> pos
	(* to roll your own markstream for non-strings *)

  val rightedge : pos -> pos

  val union  : pos * pos -> pos
  val max    : pos * pos -> pos
  val min    : pos * pos -> pos
        (* if positions are ranges (start,finish) :          *)
        (*   union ((s,f),(s',f')) = (min (s,s'),max (f,f')) *)
        (*   max   ((s,f),(s',f')) = (max (s,s'),max (f,f')) *)
        (*   min   ((s,f),(s',f')) = (min (s,s'),min (f,f')) *)

  val toString : pos -> string

end
