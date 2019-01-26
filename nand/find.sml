
(*
Control.Print.printDepth := 100;
Control.Print.printLength := 1000;
*)

structure Exp =
struct
  (* functions of type real * real -> real. *)
  datatype bingate =
    Plus
  | Minus
  | Times
  | Div
  | Hypot
  | Rem
  | Min
  | Max
  | Pow

  datatype ungate =
    Neg
  | NanTo
  | Reciprocal
  | ZeroOver
  | OnePlus
  | MinusOne
  | Abs
  | DivSelf
  | HypotNan
  | ToNegOne

  datatype exp =
    Bingate of bingate * exp * exp
  | Ungate of ungate * exp
  | Zero
  | Nan
  | Inf
  | One
  | NegInf
  | Atom of char

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

  val nullgates =
    [(Zero, 0.0),
     (Nan, nan),
     (Inf, inf),
     (One, 1.0),
     (NegInf, ~inf)]

  val bingates =
    [(Plus, Real.+),
     (Minus, Real.-),
     (Times, Real.* ),
     (Div, Real./),
     (Hypot, hypot),
     (Rem, Real.rem),
     (Min, Real.min),
     (Max, Real.max),
     (Pow, Math.pow)]

  val ungates =
    [(Neg, Real.~),
     (* (NanTo, fn x => Math.pow(nan, x)), *)
(*
     (Reciprocal, fn x => 1.0 / x),
     (ZeroOver, fn x => 0.0 / x),
     (OnePlus, fn x => 1.0 + x),
     (MinusOne, fn x => x - 1.0),
*)
     (Abs, fn x => Real.abs(x))
(*     (DivSelf, fn x => x / x), *)
     (* (HypotNan, fn x => hypot(x, nan)), *)
     (* (ToNegOne, fn x => Math.pow(x, ~1.0)) *)]

end

(* A row is a fixed-size sequence of values. *)
signature ROW =
sig
  type row
  (* Must be orderable so that we can test for success,
     and build splay set *)
  val compare : row * row -> order

  val make_nullary : real -> row
  (* Pointwise *)
  val apply_unary : (real -> real) -> row -> row
  val apply_binary : (real * real -> real) -> (row * row) -> row

end

signature DATABASE =
sig
  (* From argument. *)
  type row
  (* Mutable db of all rows that are reachable from some
     unspecified source row. *)
  type db

  (* Create a database initialized with the atoms (and other nullary
     rows). *)
  val init : (char * row) list -> db

  (* Expand one step. *)
  val expand_unary : db -> unit
  val expand_binary : db -> unit

  val contains : db -> row -> Exp.exp option

end

(* TODO: Could allow filter for some problems. Like if we
   are searching for a row where all the values are distinct,
   then any row with duplicates is known to be useless.
   Other functions like NAND however do have duplicates in
   some positions, so we don't want to require that in
   the general case. *)
functor Database(structure R : ROW) : DATABASE =
struct
  exception Database of string
  type row = R.row
  structure RM = SplayMapFn(type ord_key = row
                            val compare = R.compare)

  datatype exp = datatype Exp.exp
  type db = exp RM.map ref

  (* TODO: Mark inserted stuff as 'new' *)
  fun maybe_update_db (db, row, exp) =
    case RM.find (!db, row) of
      NONE => db := RM.insert (!db, row, exp)
      (* XXX do insert if smaller exp (and mark new) *)
    | SOME _ => ()

  (* XXX Init should also insert all nullary? *)
  fun init l =
    let
      val db = ref RM.empty
      fun add_atom (c, row) =
        db := RM.insert (!db, row, Atom c)
      fun add_null (n, r) =
        maybe_update_db (db, R.make_nullary r, n)
    in
      app add_atom l;
      app add_null Exp.nullgates;
      db
    end


  (* Apply unary operators to everything in the database
     and update it. *)
  fun expand_unary db =
    let
      val start_db = !db
      fun one (u : Exp.ungate, f : real -> real) =
        RM.appi (fn (row_src, exp) =>
                 let
                   val row_dst = R.apply_unary f row_src
                   val exp_dst = Exp.Ungate (u, exp)
                 in
                   maybe_update_db (db, row_dst, exp_dst)
                 end) start_db
    in
      app one Exp.ungates
    end

  fun expand_binary db =
    let
      (* Only consider stuff currently in the db, not things
         that are added in this pass. *)
      val start_db = !db
      fun one (b : Exp.bingate, f : real * real -> real) =
        RM.appi
        (fn (row_srca, expa) =>
         RM.appi
         (fn (row_srcb, expb) =>
          let
            val row_dst = R.apply_binary f (row_srca, row_srcb)
            val exp_dst = Exp.Bingate (b, expa, expb)
          in
            maybe_update_db (db, row_dst, exp_dst)
          end) start_db) start_db
    in
      app one Exp.bingates
    end

  fun contains db row = RM.find (!db, row)

end


structure Find =
struct
  exception Find of string

  structure PairRow : ROW =
  struct
    type row = (real * real)

    fun rcomp (a, b) =
      (* NaNs first (all considered the same). *)
      (case (Real.isNan a, Real.isNan b) of
         (true, true) => EQUAL
       | (true, false) => LESS
       | (false, true) => GREATER
       | (false, false) =>
           Real.compare (a, b))

    fun compare ((a, b), (aa, bb)) =
      case rcomp (a, aa) of
        EQUAL => rcomp (b, bb)
      | ord => ord

    fun make_nullary r = (r, r)
    fun apply_unary f (a, b) = (f a, f b)
    fun apply_binary f ((a, b), (aa, bb)) =
      (f (a, aa), f (b, bb))
  end

  structure DB = Database(structure R = PairRow)

  val nan = 0.0 / 0.0
  val inf = Real.posInf

  val start = [(#"x", (nan, inf) : PairRow.row)]
  val want = (inf, nan) : PairRow.row

  val db = DB.init start
  fun expand () =
    let in
      DB.expand_unary db;
      DB.expand_binary db;
      DB.contains db want
    end

  (*

  (* Collection of binary operators real * real -> real,
     with their names. *)
  val bingates =
    [("+", Real.+),
     ("-", Real.-),
     ("*", Real.* ),
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
     to the source values. We never insert anything where the
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

   *)
end
