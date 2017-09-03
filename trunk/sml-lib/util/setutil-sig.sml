(* Utilities derived from structures implementing njlib/ord-set-sig. *)
signature SETUTIL =
sig

  type item
  type set

  val fromlist : item list -> set
  (* From smallest to largest *)
  val tolist : set -> item list

  val fromvector : item Vector.vector -> set
  (* From smallest to largest *)
  val tovector : set -> item Vector.vector

  val mappartial : (item -> item option) -> set -> set

end
