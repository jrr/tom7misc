structure Anaglyph =
struct
  exception Anaglyph of string

  structure Atom :>
  sig
    type atom
    val compare : atom * atom -> order
    val toint : atom -> int
    val fromint : int -> atom option
    val tochar : atom -> char

    (* Needed? *)
    val c_ : atom
    val e_ : atom
    val o_ : atom
    val r_ : atom
    val s_ : atom
    val tick_ : atom
    val dot_ : atom
    val hook_ : atom

    val num_atoms : int
    val atomchars : string
    val decompose : char -> atom list option
  end =
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

  (* some quesitonable ideas:
     e -> c '
     h -> y
     m -> w
     o -> c c
     *)
  end

  structure Atoms :>
  sig
    type atoms
    val compare : atoms * atoms -> order
    val ++ : atoms * atoms -> atoms
    val -- : atoms * atoms -> atoms
    val fromlist : Atom.atom list -> atoms
    val tostring : atoms -> string
  end =
  struct
    (* Always num_atoms length.
       For an individual word, word8vector would surely suffice,
       but in order to build long phrases we may have more than
       255 of some atom. *)
    type atoms = int Vector.vector
    fun compare (a, b) =
      let
        fun c i =
          if i = Atom.num_atoms then EQUAL
          else
          case Int.compare (Vector.sub (a, i),
                            Vector.sub (b, i)) of
            EQUAL => c (i + 1)
          | order => order
      in
        c 0
      end
    fun ++(a, b) =
      Vector.tabulate (Atom.num_atoms, fn i =>
                       Vector.sub (a, i) +
                       Vector.sub (b, i))
    fun --(a, b) =
      Vector.tabulate (Atom.num_atoms, fn i =>
                       Vector.sub (a, i) -
                       Vector.sub (b, i))

    (* TODO: compare ops, so we can test like
       a - b > 0,0,0,0,0. *)

    fun fromlist al =
      let
        val a = Array.array (Atom.num_atoms, 0)
        fun oneatom atom =
          let val i = Atom.toint atom
          in Array.update (a, i, Array.sub (a, i) + 1)
          end
      in
        app oneatom al;
        Array.vector a
      end

    fun tostring a =
      let
        fun go i =
          if i = Atom.num_atoms then ""
          else
            case Vector.sub (a, i) of
              0 => go (i + 1)
            | 1 => implode [CharVector.sub (Atom.atomchars, i)] ^ go (i + 1)
            | n => Int.toString n ^
                implode [CharVector.sub (Atom.atomchars, i)] ^ go (i + 1)
      in
        go 0
      end
  end

  fun get_atoms w =
    let
      fun getnorm c =
        case Atom.decompose c of
          NONE => raise Anaglyph ("Bad character in word: " ^ w)
        | SOME al => al
      val atom_list = List.concat (map getnorm (explode w))
    in
      Atoms.fromlist atom_list
    end

  fun load_dictionary () =
    let
      val lines = Script.linesfromfile "wordlist.asc"
      fun oneword w =
        case CharVector.find (StringUtil.ischar #" ") w of
          SOME _ => raise Anaglyph ("Dictionary word has space: " ^ w)
        | NONE => (w, get_atoms w)
    in
      map oneword lines
    end

  val dictionary = load_dictionary ()

  structure AM = SplayMapFn(type ord_key = Atoms.atoms
                            val compare = Atoms.compare)
  val clusters : string list AM.map ref = ref AM.empty
  fun oneword (w, atoms) =
    case AM.find (!clusters, atoms) of
      NONE => clusters := AM.insert(!clusters, atoms, [w])
    | SOME l => clusters := AM.insert(!clusters, atoms, w :: l)
  val () = app oneword dictionary

  (* For -dump command. *)
  fun canonized_file () =
    let
      (* Render one cluster, but don't bother if it's a singleton. *)
      fun makeline (atoms, [oneword]) = ""
        | makeline (atoms, words) =
        Atoms.tostring atoms ^ "  " ^ (StringUtil.delimit " " words) ^ "\n"
    in
      String.concat (map makeline (AM.listItemsi (!clusters)))
    end

  (* Make a plan for animating word1 to word2. They don't have to be in
     the dictionary, and can even be space-separated phrases. However,
     they must have the same set of atoms.
*)
(**
fun makeplan (word1, word2) =
    let
      val atoms1 =
*)

end
