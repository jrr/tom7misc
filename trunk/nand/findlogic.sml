
(*
Control.Print.printDepth := 100;
Control.Print.printLength := 1000;
*)

structure Exp =
struct
  (* Logic gates *)
  datatype bingate =
    Nand | Or | Xor | And

  fun btos b =
    case b of
      Nand => "Nand"
    | Or => "Or"
    | Xor => "Xor"
    | And => "And"

  datatype ungate =
    Not

  fun utos u =
    case u of
      Not => "Not"

  datatype wire =
      Zero
    | One

  fun wtos w =
    case w of
      Zero => "0"
    | One => "1"

  val bingates = [Nand, Or, Xor, And]
  val ungates = [Not]
  val wires = [Zero, One]

  datatype exp =
    Bingate of bingate * exp * exp
  | Ungate of ungate * exp
  | Wire of wire
  | Atom of char

  fun etos e =
    case e of
      Bingate (b, aa, bb) => btos b ^ "(" ^ etos aa ^ ", " ^ etos bb ^ ")"
    | Ungate (u, a) => utos u ^ "(" ^ etos a ^ ")"
    | Wire w => wtos w
    | Atom c => implode [c]

  fun esize e =
    case e of
      Bingate (_, a, b) => 1 + esize a + esize b
    | Ungate (_, a) => 1 + esize a
    | Wire _ => 1
    | Atom _ => 1

end

(* A row is a fixed-size sequence of values. *)
signature ROW =
sig
  type row
  (* Must be orderable so that we can test for success,
     and build splay set *)
  val compare : row * row -> order

  val make_wire : Exp.wire -> row
  (* Pointwise *)
  val apply_unary : Exp.ungate -> row -> row
  val apply_binary : Exp.bingate -> (row * row) -> row

  val rtos : row -> string
end

signature DATABASE =
sig
  (* From argument. *)
  type row
  (* Mutable db of all rows that are reachable from some
     unspecified source row. *)
  type db

  (* init filter rows
     Create a database initialized with the atoms (and other wire
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
  fun maybe_update_db ((filter, m), row : R.row, exp : Exp.exp) =
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

  fun init filter (l : (char * R.row) list) =
    let
      val db = (filter, ref RM.empty)
      fun add_atom (c, row) =
        maybe_update_db (db, row, Atom c)
      fun add_wire (n : Exp.wire) =
        maybe_update_db (db, R.make_wire n, Exp.Wire n)
    in
      app add_atom l;
      app add_wire Exp.wires;
      db
    end


  (* Apply unary operators to everything in the database
     and update it. *)
  fun expand_unary (db as (_, ref start_map)) =
    let
      fun one (u : Exp.ungate) =
        RM.appi (fn (row_src, exp) =>
                 let
                   val row_dst = R.apply_unary u row_src
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
      fun one (b : Exp.bingate) =
        RM.appi
        (fn (row_srca, expa) =>
         RM.appi
         (fn (row_srcb, expb) =>
          let
            val row_dst = R.apply_binary b (row_srca, row_srcb)
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

  structure WordRow =
  struct
    type row = Word64.word
    val compare = Word64.compare
    exception WrongLength

    fun fromlist l =
      let
        fun fl (w, nil, 0) = w
          | fl (w, h :: t, n) =
          fl (Word64.orb(Word64.<<(w, 0w1),
                         if h then 0w1 else 0w0), t, n - 1)
          | fl _ = raise WrongLength
      in
        fl (0w0, l, 64)
      end

    fun make_wire Exp.Zero = fromlist (List.tabulate (64, fn _ => false))
      | make_wire Exp.One = fromlist (List.tabulate (64, fn _ => true))

    fun apply_unary Exp.Not w = Word64.notb w
    fun apply_binary Exp.Nand (a, b) =
      Word64.notb(Word64.andb(a, b))
      | apply_binary Exp.Or (a, b) = Word64.orb(a, b)
      | apply_binary Exp.Xor (a, b) = Word64.xorb(a, b)
      | apply_binary Exp.And (a, b) = Word64.andb(a, b)

    fun rtos w = Word64.toString w
  end

  structure Row = WordRow
  structure DB = Database(structure R = Row)
  (* Full adder is three inputs, two outputs *)

  val a0 = WordRow.fromlist (List.tabulate(64, fn x => (x div 32) mod 2 = 0))
  val a1 = WordRow.fromlist (List.tabulate(64, fn x => (x div 16) mod 2 = 0))
  val a2 = WordRow.fromlist (List.tabulate(64, fn x => (x div 8) mod 2 = 0))
  val b0 = WordRow.fromlist (List.tabulate(64, fn x => (x div 4) mod 2 = 0))
  val b1 = WordRow.fromlist (List.tabulate(64, fn x => (x div 2) mod 2 = 0))
  val b2 = WordRow.fromlist (List.tabulate(64, fn x => x mod 2 = 0))

  val a02 = WordRow.apply_binary Exp.Nand (WordRow.apply_unary Exp.Not a0,
                                           WordRow.apply_unary Exp.Not a2)
  val b02 = WordRow.apply_binary Exp.Nand (WordRow.apply_unary Exp.Not b0,
                                           WordRow.apply_unary Exp.Not b2)
  val z = WordRow.apply_binary Exp.Nand (a1, b1)

  val f = WordRow.fromlist (List.tabulate(64, fn x => x = 45))

  val start = [(* (#"a", a0), *)
               (* (#"b", a1), *)
               (* (#"c", a2), *)
               (* (#"d", b0), *)
               (* (#"e", b1), *)
               (* (#"f", b2) *)

               (#"x", a02),
               (#"y", b02),
               (#"z", z)
               ]

  (* We know that the last step has to take the form Nand(g, h)
     where g and h are 1 in every position except for the 45th
     bit. So first find 11111111...0...111111.
     If we have that, then we are done, because NAND(g, g) will
     work. If g were actually all ones, then h would have to
     be of that form (because nand of both would yield zero). *)
  val g = WordRow.fromlist (List.tabulate(64, fn x => x = 45))

  val want = f

  (* Anything good we can do here? *)
  fun only_special l = true

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
              print (Row.rtos row ^ " = " ^ Exp.etos exp ^ "\n")) db
    end

end
