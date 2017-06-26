
(* Copyright (c) 2000-2001 Tom Murphy VII.

   Licensed for your use and distribution under the
   GNU Public License, version 2.
   http://www.gnu.org/copyleft/gpl.html

*)

structure Haxor =
struct

  structure V = Word8Vector
  structure W8 = Word8

  exception Error of string

  fun K a b = a

  open FileStuff

  (* returns a candidate list where everything is in the list *)
  fun all i v =
    List.tabulate ((V.length v), (fn x => (x, i x)))


  fun step p c v =
    List.mapPartial (fn (i, data) =>
                     Option.map (fn d => (i,d)) (p (i, data, V.sub (v, i)))) c

   (* this returns all the candidates that satisfy property p
      for the list of filenames. *)
  fun sats p i l =
    let val vs = map readfile l
    in
      foldl (fn (a, b) => step p b a) (all i (hd vs)) vs
    end
   
  fun decr_prop (_, state, x) =
    if W8.< (x, state) then SOME x else NONE
  
  val decreases = sats decr_prop (fn x => 0wxff)
 
  fun equ_prop (_, nil, x) = NONE
    | equ_prop (_, h::t, x : W8.word) =
    if x = h then SOME t else NONE

  fun equals l = sats equ_prop (K l)

end
