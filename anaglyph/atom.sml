structure Canonical :> ATOM =
struct
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

structure InversionOnly :> ATOM =
struct
  type atom = int
  val START = ord #"a"
  val compare = Int.compare
  (* bdpq map to b, and nu maps to n. Everything else is identity. *)
  val num_atoms = 26 - 3 - 1
  fun tochar a =
    CharVector.sub("abcefghijklmnorstvwxyz", a)

  fun toint a = a
  fun fromint i = if i >= 0 andalso i < num_atoms
                  then SOME i else NONE

  fun decompose c =
    case c of
      #"a" => SOME [ord #"a" - START]
    | #"b" => SOME [ord #"b" - START]
    | #"c" => SOME [ord #"c" - START]
    | #"d" => SOME [ord #"b" - START] (* reuse *)
    | #"e" => SOME [ord #"e" - START - 1]
    | #"f" => SOME [ord #"f" - START - 1]
    | #"g" => SOME [ord #"g" - START - 1]
    | #"h" => SOME [ord #"h" - START - 1]
    | #"i" => SOME [ord #"i" - START - 1]
    | #"j" => SOME [ord #"j" - START - 1]
    | #"k" => SOME [ord #"k" - START - 1]
    | #"l" => SOME [ord #"l" - START - 1]
    | #"m" => SOME [ord #"m" - START - 1]
    | #"n" => SOME [ord #"n" - START - 1]
    | #"o" => SOME [ord #"o" - START - 1]
    | #"p" => SOME [ord #"b" - START] (* reuse *)
    | #"q" => SOME [ord #"b" - START] (* reuse *)
    | #"r" => SOME [ord #"r" - START - 3]
    | #"s" => SOME [ord #"s" - START - 3]
    | #"t" => SOME [ord #"t" - START - 3]
    | #"u" => SOME [ord #"n" - START] (* reuse *)
    | #"v" => SOME [ord #"v" - START - 4]
    | #"w" => SOME [ord #"w" - START - 4]
    | #"x" => SOME [ord #"x" - START - 4]
    | #"y" => SOME [ord #"y" - START - 4]
    | #"z" => SOME [ord #"z" - START - 4]
    | #" " => SOME []
    | _ => NONE
end

(* Letters decompose to themselves only. *)
structure Letter :> ATOM =
struct
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


structure Atom = Letter
