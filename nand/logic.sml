
structure Logic =
struct

  exception Domain

  fun nand (true, true) = false
    | nand _ = true

  (* Target function to represent. *)
  fun target [a0, a1, a2, b0, b1, b2] =
    not a0 andalso not a2 andalso not b0 andalso not b2 andalso
    a1 andalso b1
    | target _ = raise Domain

  (* All bit strings of length n *)
  fun bits 0 = [[]]
    | bits n =
    let val b = bits (n - 1)
    in
      map (fn l => true :: l) b @
      map (fn l => false :: l) b
    end

  fun x [a0, _, a2,  _, _, _]  = nand(a0, a2)
    | x _ = raise Domain
  fun y [_,  _,  _, b0, _, b2] = nand(b0, b2)
    | y _ = raise Domain
  fun z [_, a1, _, _, b1, _] = nand(a1, b1)
    | z _ = raise Domain

  fun xandy inputs = x inputs andalso y inputs

  fun fns bits = [target bits, x bits, y bits, z bits, xandy bits]

  val inputs = bits 6

  fun mkbits l = map (fn true => "1" | false => "0") l

  val table =
    ["a0", "a1", "a2", "b0", "b1", "b2", "f", "x", "y", "z", "xandy"] ::
    map (fn input =>
         mkbits (input @ [target input, x input, y input, z input,
                          xandy input])) inputs

  val s = StringUtil.table 80 table

end