
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
  | Atan2
  | CopySign
    (* Note: sml/nj does not have nextAfter *)

  fun btos b =
    case b of
      Plus => "Plus"
    | Minus => "Minus"
    | Times => "Times"
    | Div => "Div"
    | Hypot => "Hypot"
    | Rem => "Rem"
    | Min => "Min"
    | Max => "Max"
    | Pow => "Pow"
    | Atan2 => "Atan2"
    | CopySign => "CopySign"

  datatype ungate =
    Neg
  | Abs
  | Atan
  | Tan
  | Tanh
  | Asin
  | Sin
  | Sinh
  | Exp
  | Mod
  | Sqrt
  | Ln

  fun utos u =
    case u of
      Neg => "Neg"
    | Abs => "Abs"
    | Atan => "Atan"
    | Tan => "Tan"
    | Tanh => "Tanh"
    | Asin => "Asin"
    | Sin => "Sin"
    | Sinh => "Sinh"
    | Exp => "Exp"
    | Mod => "Mod"
    | Sqrt => "Sqrt"
    | Ln => "Ln"

  datatype exp =
    Bingate of bingate * exp * exp
  | Ungate of ungate * exp
  | Zero
  | Nan
  | Inf
  | One
  | NegOne
  | NegInf
  | Atom of char

  fun etos e =
    case e of
      Bingate (b, aa, bb) => btos b ^ "(" ^ etos aa ^ ", " ^ etos bb ^ ")"
    | Ungate (u, a) => utos u ^ "(" ^ etos a ^ ")"
    | Zero => "0"
    | Nan => "NaN"
    | Inf => "Inf"
    | One => "1"
    | NegOne => "-1"
    | NegInf => "NegInf"
    | Atom c => implode [c]

  fun esize e =
    case e of
      Bingate (_, a, b) => 1 + esize a + esize b
    | Ungate (_, a) => 1 + esize a
    | _ => 1

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
     (NegOne, ~1.0),
     (NegInf, ~inf)]

  val bingates =
    [(Plus, Real.+),
     (Minus, Real.-),
     (Times, Real.* ),
     (Div, Real./),
     (Hypot, hypot),
     (Rem, Real.rem),
(*
     (Min, Real.min),
     (Max, Real.max)
*)
     (* (CopySign, Real.copySign), *)
     (Pow, Math.pow),
     (Atan2, Math.atan2)

     ]

  val ungates =
    [(Neg, Real.~),
     (Abs, Real.abs),

     (Sqrt, Math.sqrt),
     (Atan, Math.atan),
     (Tan, Math.tan),
     (Tanh, Math.tanh),
     (Asin, Math.asin),
     (Sin, Math.sin),
     (Sinh, Math.sinh),
     (Ln, Math.ln),

     (Exp, Math.exp),
     (Mod, Real.realMod)
     ]

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

  val rtos : row -> string

end

signature DATABASE =
sig
  (* From argument. *)
  type row
  (* Mutable db of all rows that are reachable from some
     unspecified source row. *)
  type db

  (* Create a database initialized with the atoms (and other nullary
     rows). Only rows that pass the filter function are ever inserted. *)
  val init : (row -> bool) -> (char * row) list -> db

  val size : db -> int

  (* Expand one step. *)
  val expand_unary : db -> unit
  val expand_binary : db -> unit

  val contains : db -> row -> Exp.exp option
  val app : (row * Exp.exp -> unit) -> db -> unit

end

functor Database(structure R : ROW) : DATABASE =
struct
  exception Database of string
  type row = R.row
  structure RM = SplayMapFn(type ord_key = row
                            val compare = R.compare)

  datatype exp = datatype Exp.exp
  type db = (row -> bool) * exp RM.map ref

  fun size (_, ref m) = RM.numItems m

  (* TODO: Mark inserted stuff as 'new'? *)
  fun maybe_update_db ((filter, m), row, exp) =
    if filter row
    then
      (case RM.find (!m, row) of
         NONE =>
           let in
             m := RM.insert (!m, row, exp)
           end
       | SOME old =>
           let
             (* Update if it is smaller. *)
             val newsize = Exp.esize exp
             val oldsize = Exp.esize old
           in
             if newsize < oldsize
             then
               let in
                 (*
                 print ("Improved " ^ R.rtos row ^ ":\n  " ^
                 Exp.etos old ^ " ->\n  " ^
                 Exp.etos exp ^ "\n");
                 *)
                 m := RM.insert (!m, row, exp)
               end
             else ()
           end)
    else ()

  fun init filter l =
    let
      val db = (filter, ref RM.empty)
      fun add_atom (c, row) =
        maybe_update_db (db, row, Atom c)
      fun add_null (n, r) =
        maybe_update_db (db, R.make_nullary r, n)
    in
      app add_atom l;
      app add_null Exp.nullgates;
      db
    end


  (* Apply unary operators to everything in the database
     and update it. *)
  fun expand_unary (db as (_, ref start_map)) =
    let
      fun one (u : Exp.ungate, f : real -> real) =
        RM.appi (fn (row_src, exp) =>
                 let
                   val row_dst = R.apply_unary f row_src
                   val exp_dst = Exp.Ungate (u, exp)
                 in
                   maybe_update_db (db, row_dst, exp_dst)
                 end) start_map
    in
      List.app one Exp.ungates
    end

  fun expand_binary (db as (_, ref start_map)) =
    let
      (* Only consider stuff currently in the db, not things
         that are added in this pass. *)
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
          end) start_map) start_map
    in
      List.app one Exp.bingates
    end

  fun contains (_, m) row = RM.find (!m, row)

  fun app f (_, ref m) = RM.appi f m

end


structure Find =
struct
  exception Find of string

  fun rcomp (a, b) =
    (* NaNs first (all considered the same). *)
    (case (Real.isNan a, Real.isNan b) of
       (true, true) => EQUAL
     | (true, false) => LESS
     | (false, true) => GREATER
     | (false, false) =>
         Real.compare (a, b))

  structure PairRow : ROW =
  struct
    type row = (real * real)

    fun compare ((a, b), (aa, bb)) =
      case rcomp (a, aa) of
        EQUAL => rcomp (b, bb)
      | ord => ord

    fun make_nullary r = (r, r)
    fun apply_unary f (a, b) = (f a, f b)
    fun apply_binary f ((a, b), (aa, bb)) =
      (f (a, aa), f (b, bb))

    fun rtos (a, b) =
      "[" ^ Real.toString a ^ ", " ^ Real.toString b ^ "]"
  end

  structure TruthTable : ROW =
  struct
    (* as      false true
         false   a    b
         true    c    d
         *)

    type row = (real * real *
                real * real)

    fun compare ((a, b, c, d), (aa, bb, cc, dd)) =
      case rcomp (a, aa) of
        EQUAL =>
          (case rcomp (b, bb) of
             EQUAL =>
               (case rcomp (c, cc) of
                  EQUAL => rcomp (d, dd)
                | ord => ord)
           | ord => ord)
      | ord => ord

    fun make_nullary r = (r, r, r, r)
    fun apply_unary f (a, b, c, d) = (f a, f b, f c, f d)
    fun apply_binary f ((a, b, c, d), (aa, bb, cc, dd)) =
      (f (a, aa), f (b, bb), f (c, cc), f (d, dd))

    fun rtos (a, b, c, d) =
      "[" ^
      Real.toString a ^ ", " ^
      Real.toString b ^ ", " ^
      Real.toString c ^ ", " ^
      Real.toString d ^ "]"
  end

  val nan = 0.0 / 0.0
  val inf = Real.posInf

    (*
  structure DB = Database(structure R = PairRow)
  val start = [(#"x", (nan, inf) : PairRow.row)]
  val want = (inf, nan) : PairRow.row
  *)

  structure DB = Database(structure R = TruthTable)
  val start = [(#"x", (nan, inf, nan, inf)),
               (#"y", (nan, nan, inf, inf))]
  val table_nand = (inf, inf, inf, nan)
  val table_and = (nan, nan, nan, inf)
  val table_or = (nan, inf, inf, inf)
  val table_xor = (nan, inf, inf, nan)
  val table_nor = (inf, nan, nan, nan)

  val want = table_nand

  fun is_special r =
    Real.isNan r orelse
    not (Real.isFinite r) orelse
    Real.==(r, 0.0) orelse
    Real.==(r, 1.0) orelse
    Real.==(r, ~1.0) orelse
    Real.==(r, 2.0) orelse
    Real.==(r, ~2.0)

  (* To cut down on search space, don't consider values outside
     this small set of distinguished ones. We assume that e.g.
     1.12346 is not going to give us any new behavior that we
     don't already have with 1 or 2. *)
  fun only_special (a, b, c, d) =
    is_special a andalso is_special b andalso is_special c
    andalso is_special d

  val db = DB.init only_special start
  fun expand () =
    let in
      DB.expand_unary db;
      DB.expand_binary db;
      print ("Database size: " ^ Int.toString (DB.size db) ^ "\n");
      case DB.contains db want of
        NONE => ()
      | SOME exp => print ("Success:\n" ^ Exp.etos exp ^ "\n")
    end

  fun printdb () =
    let in
      DB.app (fn (row, exp) =>
              print (TruthTable.rtos row ^ " = " ^ Exp.etos exp ^ "\n")) db
    end

end
