structure Canonical :> ATOM =
struct
  val name = "ceors"

  type atom = int
  val compare = Int.compare
  val atomchars = "ceors'.?"
  val num_atoms = size atomchars
  fun tochar a = CharVector.sub(atomchars, a)
  fun toint a = a
  fun fromint i = if i >= 0 andalso i < num_atoms
                  then SOME i else NONE

  val c_ = 0
  val e_ = 1
  val o_ = 2
  val r_ = 3
  val s_ = 4
  (* half-height vertical bar, like h = n + ' *)
  val tick_ = 5
  (* the dot on i and j *)
  val dot_ = 6
  (* just the asymmetrical descender hook of g, j *)
  val hook_ =  7

  fun decompose c =
    case c of
      #"a" => SOME [c_, tick_]
    | #"b" => SOME [tick_, tick_, c_]
    | #"c" => SOME [c_]
    | #"d" => SOME [tick_, tick_, c_]
    | #"e" => SOME [e_]
    | #"f" => SOME [hook_, tick_, tick_]
    | #"g" => SOME [hook_, tick_, c_]
    | #"h" => SOME [tick_, r_, tick_]
    | #"i" => SOME [tick_, dot_]
    | #"j" => SOME [hook_, tick_, dot_]
    | #"k" => SOME [tick_, tick_, tick_, tick_]
    | #"l" => SOME [tick_, tick_]
    | #"m" => SOME [r_, r_, tick_]
    | #"n" => SOME [tick_, r_]
    | #"o" => SOME [o_]
    | #"p" => SOME [tick_, tick_, c_]
    | #"q" => SOME [tick_, tick_, c_]
    | #"r" => SOME [r_]
    | #"s" => SOME [s_]
    | #"t" => SOME [tick_, tick_, tick_]
    | #"u" => SOME [tick_, r_]
    | #"v" => SOME [tick_, tick_]
    | #"w" => SOME [tick_, tick_, tick_, tick_]
    | #"x" => SOME [tick_, tick_, tick_, tick_]
    | #"y" => SOME [r_, tick_, hook_]
    | #"z" => SOME [tick_, tick_, tick_]
    | #" " => SOME []
    | _ => NONE
end

structure Inv :> ATOM =
struct
  val name = "inversions"

  type atom = int
  val START = ord #"a"
  val compare = Int.compare
  (* bq map to b, dp map to d, and nu maps to n. Everything else is identity. *)
  val num_atoms = 26 - 1 - 1 - 1
  fun tochar a =
    CharVector.sub("abcdefghijklmnorstvwxyz", a)

  fun toint a = a
  fun fromint i = if i >= 0 andalso i < num_atoms
                  then SOME i else NONE

  fun decompose c =
    case c of
      #"a" => SOME [ord #"a" - START]
    | #"b" => SOME [ord #"b" - START]
    | #"c" => SOME [ord #"c" - START]
    | #"d" => SOME [ord #"d" - START]
    | #"e" => SOME [ord #"e" - START]
    | #"f" => SOME [ord #"f" - START]
    | #"g" => SOME [ord #"g" - START]
    | #"h" => SOME [ord #"h" - START]
    | #"i" => SOME [ord #"i" - START]
    | #"j" => SOME [ord #"j" - START]
    | #"k" => SOME [ord #"k" - START]
    | #"l" => SOME [ord #"l" - START]
    | #"m" => SOME [ord #"m" - START]
    | #"n" => SOME [ord #"n" - START]
    | #"o" => SOME [ord #"o" - START]
    | #"p" => SOME [ord #"d" - START] (* reuse *)
    | #"q" => SOME [ord #"b" - START] (* reuse *)
    | #"r" => SOME [ord #"r" - START - 2]
    | #"s" => SOME [ord #"s" - START - 2]
    | #"t" => SOME [ord #"t" - START - 2]
    | #"u" => SOME [ord #"n" - START] (* reuse *)
    | #"v" => SOME [ord #"v" - START - 3]
    | #"w" => SOME [ord #"w" - START - 3]
    | #"x" => SOME [ord #"x" - START - 3]
    | #"y" => SOME [ord #"y" - START - 3]
    | #"z" => SOME [ord #"z" - START - 3]
    | #" " => SOME []
    | _ => NONE
end

structure CapInv :> ATOM =
struct
  val name = "capinv"

  type atom = int
  val START = ord #"a"
  val compare = Int.compare
  (* mw maps to m *)
  val num_atoms = 26 - 1
  fun tochar a =
    CharVector.sub("abcdefghijklmnopqrstuvxyz", a)

  fun toint a = a
  fun fromint i = if i >= 0 andalso i < num_atoms
                  then SOME i else NONE

  fun decompose c =
    case c of
      #"a" => SOME [ord #"a" - START]
    | #"b" => SOME [ord #"b" - START]
    | #"c" => SOME [ord #"c" - START]
    | #"d" => SOME [ord #"d" - START]
    | #"e" => SOME [ord #"e" - START]
    | #"f" => SOME [ord #"f" - START]
    | #"g" => SOME [ord #"g" - START]
    | #"h" => SOME [ord #"h" - START]
    | #"i" => SOME [ord #"i" - START]
    | #"j" => SOME [ord #"j" - START]
    | #"k" => SOME [ord #"k" - START]
    | #"l" => SOME [ord #"l" - START]
    | #"m" => SOME [ord #"m" - START]
    | #"n" => SOME [ord #"n" - START]
    | #"o" => SOME [ord #"o" - START]
    | #"p" => SOME [ord #"p" - START]
    | #"q" => SOME [ord #"q" - START]
    | #"r" => SOME [ord #"r" - START]
    | #"s" => SOME [ord #"s" - START]
    | #"t" => SOME [ord #"t" - START]
    | #"u" => SOME [ord #"u" - START]
    | #"v" => SOME [ord #"v" - START]
    | #"w" => SOME [ord #"m" - START]
    | #"x" => SOME [ord #"x" - START - 1]
    | #"y" => SOME [ord #"y" - START - 1]
    | #"z" => SOME [ord #"z" - START - 1]
    | #" " => SOME []
    | _ => NONE
end

(* Letters decompose to themselves only. *)
structure Letter :> ATOM =
struct
  val name = "letters"

  type atom = int
  val compare = Int.compare
  (* bdpq map to b, and nu maps to n. Everything else is identity. *)
  val num_atoms = 26
  fun tochar a = chr (a + ord #"a")

  fun toint a = a
  fun fromint i = if i >= 0 andalso i < num_atoms
                  then SOME i else NONE

  fun decompose #" " = SOME nil
    | decompose c =
    let val oc = ord c
    in
      if oc >= ord #"a" andalso oc <= ord #"z"
      then SOME [oc - ord #"a"]
      else NONE
    end
end

(* Pixel font comes from pixelfont.png, and uses data generated
   by makepixels.cc *)
structure Pixel :> ATOM =
struct
  val name = "pixels"
  type atom = unit
  val compare = Util.unit_compare
  val num_atoms = 1
  fun toint () = 0
  fun tochar () = #"a"
  fun fromint 0 = SOME ()
    | fromint _ = NONE
  fun decompose #" " = SOME nil
    | decompose c =
    case PixelData.count c of
      NONE => NONE
    | SOME n =>
        SOME (List.tabulate (n, fn _ => ()))
end

(* Special case for video *)
structure QOE :> ATOM =
struct

  val name = "qoe"
  exception Impossible of string

  type atom = int
  (* E -> F',  Q -> O' *)
  val atomchars = "abcdfghijklmnoprstuvwxyz'"
  val START = ord #"a"
  val compare = Int.compare
  val num_atoms = size atomchars
  fun tochar a = CharVector.sub(atomchars, a)

  fun toint a = a
  fun fromint i = if i >= 0 andalso i < num_atoms
                  then SOME i else NONE

  fun get c =
    case CharVector.findi (fn (_, cc) => c = cc) atomchars of
      SOME (i, _) => i
    | NONE => raise Impossible name

  (* XXX this approach is probably better for some of the
     above atoms too. *)
  val table = Array.array (256, NONE)
  (* Start with every atom mapped to itself (except special
     ones). *)
  val () = CharVector.app
    (fn c =>
     if c <> #"'"
     then Array.update (table, ord c, SOME [get c])
     else ()) atomchars

  val () = Array.update (table, ord #" ", SOME nil)
  val () = Array.update (table, ord #"q", SOME [get #"o", get #"'"])
  val () = Array.update (table, ord #"e", SOME [get #"f", get #"'"])

  fun decompose c = Array.sub (table, ord c)

end

(* structure Atom = Canonical *)
structure Atom = QOE

