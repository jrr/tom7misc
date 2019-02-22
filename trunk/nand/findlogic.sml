
(*
Control.Print.printDepth := 100;
Control.Print.printLength := 1000;
*)

structure Exp =
struct
  (* Logic gates *)
  datatype bingate = Nand

  fun btos b =
    case b of
      Nand => "Nand"

  datatype ungate =
    Not

  fun utos u =
    case u of
      Not => "Not"

  datatype exp =
    Bingate of bingate * exp * exp
  | Ungate of ungate * exp
  | Zero
  | One
  | Atom of char

  fun etos e =
    case e of
      Bingate (b, aa, bb) => btos b ^ "(" ^ etos aa ^ ", " ^ etos bb ^ ")"
    | Ungate (u, a) => utos u ^ "(" ^ etos a ^ ")"
    | Zero => "0"
    | One => "1"
    | Atom c => implode [c]

  fun esize e =
    case e of
      Bingate (_, a, b) => 1 + esize a + esize b
    | Ungate (_, a) => 1 + esize a
    | _ => 1

  val nullgates =
    [(Zero, false),
     (One, true)]

  val bingates =
    [(Nand, (fn (a, b) => not (a andalso b)))]

  val ungates =
    [(Not, not)]

end

(* A row is a fixed-size sequence of values. *)
signature ROW =
sig
  type row
  (* Must be orderable so that we can test for success,
     and build splay set *)
  val compare : row * row -> order

  val make_nullary : bool -> row
  (* Pointwise *)
  val apply_unary : (bool -> bool) -> row -> row
  val apply_binary : (bool * bool -> bool) -> (row * row) -> row

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
      fun one (u : Exp.ungate, f : bool -> bool) =
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
      fun one (b : Exp.bingate, f : bool * bool -> bool) =
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

  fun rcomp (false, true) = LESS
    | rcomp (true, false) = GREATER
    | rcomp _ = EQUAL

  (* Once it gets long enough, just treat as n-ary.

     Here for example we can have a 64-element list
     corresponding to each of 2^8 inputs. *)
  functor ValueList(val n : int) : ROW =
  struct

    exception ValueList of string
    exception WrongLength

    (* Of length exactly n. *)
    type row = bool list

    fun compare (a :: ar, b :: br) =
      (case rcomp (a, b) of
        EQUAL => compare (ar, br)
      | ord => ord)
      | compare (nil, nil) = EQUAL
      | compare _ = raise WrongLength

    fun make_nullary r = List.tabulate(n, fn _ => r)
    fun apply_unary f l = List.map f l
    fun apply_binary f (a :: al, b :: bl) =
      f (a, b) :: apply_binary f (al, bl)
      | apply_binary _ (nil, nil) = nil
      | apply_binary _ _ = raise WrongLength

    fun rtos l =
      "[" ^
      StringUtil.delimit ", " (map (fn true => "1" | false => "0") l) ^
      "]"
  end

  (* structure Truth3Table = ValueList(val n = 8) *)
  structure Truth8Table = ValueList(val n = 64)


  structure Row = Truth8Table
  structure DB = Database(structure R = Row)
  (* Full adder is three inputs, two outputs *)

  val a0 = List.tabulate(64, fn x => (x div 32) mod 2 = 0)
  val a1 = List.tabulate(64, fn x => (x div 16) mod 2 = 0)
  val a2 = List.tabulate(64, fn x => (x div 8) mod 2 = 0)
  val b0 = List.tabulate(64, fn x => (x div 4) mod 2 = 0)
  val b1 = List.tabulate(64, fn x => (x div 2) mod 2 = 0)
  val b2 = List.tabulate(64, fn x => x mod 2 = 0)

  val f = List.tabulate(64, fn x => x = 45)

  val start = [(#"a", a0),
               (#"b", a1),
               (#"c", a2),
               (#"d", b0),
               (#"e", b1),
               (#"f", b2)]

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
