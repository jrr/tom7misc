signature ATOM =
sig
  eqtype atom
  val compare : atom * atom -> order
  val toint : atom -> int
  val fromint : int -> atom option
  val tochar : atom -> char

  val num_atoms : int
  val atomchars : string
  val decompose : char -> atom list option
end
