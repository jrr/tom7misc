structure Find =
struct
  exception Find of string

  val nan = 0.0 / 0.0
  val inf = Real.posInf

  (* Positive or negative infinity *)
  fun is_inf x = not (Real.isNan x) andalso not (Real.isFinite x)

  (* Not in SML basis, but looks really useful.. *)
  fun hypot (x, y) =
    if is_inf x orelse is_inf y
    then inf
    else if Real.isNan x orelse Real.isNan y
         then nan
         else (* For normal values *)
           Math.sqrt (x * x + y * y)

  (* Collection of binary operators real * real -> real,
     with their names. *)
  val bingates =
    [("+", Real.+),
     ("-", Real.-),
     ("*", Real.*),
     ("/", Real./),
     ("hypot", hypot),
     (* is IEEE? *)
     ("rem", Real.rem),
     (* These are good candiates!
        These are called minNum and maxNum in IEE 754-2008 (5.3.1, p19)

        "If exactly one argument is NaN, they return the other.
        If both are NaN they return NaN.

        NAND  0 1     MAX  nan inf
             +---         +-------
           0 |1 1     nan |nan inf
           1 |1 0     inf |inf inf

                    HYPOT  nan inf
                          +-------
                      nan |nan inf
                      inf |inf inf

        So if 0 = nan and 1 = inf, then MAX is OR.

        We also have AND, since only inf*inf (aka 1 AND 1)
        returns inf in that truth table.

        Is AND and OR together complete? (I don't think so?)
        Do we have NOT?
        *)
     ("min", Real.min),
     ("max", Real.max)]

  val ungates =
     [("~", Real.~),
      ("NaN^x", fn x => Math.pow(nan, x)),
      ("1/x", fn x => 1.0 / x),
      ("0/x", fn x => 0.0 / x),
      ("1+x", fn x => 1.0 + x),
      ("x-1", fn x => x - 1.0),
      ("abs(x)", fn x => Real.abs(x)),
      ("x/x", fn x => x / x),
      ("hypot(x,nan)", fn x => hypot(x, nan)),
      ("x^-1", fn x => Math.pow(x, ~1.0))]

  (* ISO: any function that maps:

         - nan -> non-nan
         - (any non-nan value) -> anything other than above
     *)

  fun same_float (a, b) =
    (Real.isNan a andalso Real.isNan b) orelse
    Real.== (a, b)

  (* Search for unary functions that satisfy all the constraints,
     up to max_depth compositions.

     Here we are specialized to two constranints, f(src1) = dst1
     and f(src2) = dst2. But it would be natural to extend it
     to n constraints.

     Invariant: the source values in the constraints are
     all distinct. *)
  fun unsearch (_, _, 0) = NONE
    | unsearch ((src1, dst1), (src2, dst2), max_depth : int) =
    (* This can be done better with dynamic programming or
       even a simple cache, but brute force to start. For
       each input in the constraint set, try every unary function
       on it. If we get the correct answers, great; we're done!
       Otherwise, make sure they are all distinct. If not, we've
       failed since no function could ever distinguish them.
       If they are distinct, then we have new constraints, so
       try that with depth - 1. *)
    let
      fun try (name, f) =
        let
          val d1 = f src1
          val d2 = f src2
        in
          (* Already done? *)
          if same_float (d1, dst1) andalso
             same_float (d2, dst2) then SOME [name]
          else
            (* Are the values still distinguished? *)
            if same_float (d1, d2)
            then NONE
            else (case unsearch ((d1, dst1), (d2, dst2), max_depth - 1) of
                    NONE => NONE
                  | SOME fl => SOME (name :: fl))
        end

      (* here, the depth is the number of binary nodes we
         allow. *)
      fun trygen () =
        let
          fun tg nil = NONE
            | tg (u :: rest) =
            (case try u of
               NONE => tg rest
             | SOME r => SOME r)
        in
          tg ungates
        end
    in
      trygen ()
    end

  (* TODO NEXT!
     I think the right way to do this search is to have a pool of
     pairs of values that can be reached by applying some function
     to the source values. We never insert anything there the
     values are the same. We can grow the pool by applying a unary
     function to any value (subject to the constraint that it produces
     different outputs) or binary functions to any pair of values
     (same constraint). Each value should have its expression,
     as well as a (min) depth, so that we can generate the best
     expression. *)

  fun go n =
     let
     in
       unsearch ((nan, inf), (inf, nan), n)
     (* raise (Find "unimplemented") *)
     end

end
