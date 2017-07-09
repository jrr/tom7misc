structure Atom :> ATOM =
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
