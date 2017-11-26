signature ATOM =
sig
  val name : string

  eqtype atom
  val compare : atom * atom -> order
  val toint : atom -> int
  val fromint : int -> atom option
  val tochar : atom -> char

  val num_atoms : int
  val decompose : char -> atom list option
end
