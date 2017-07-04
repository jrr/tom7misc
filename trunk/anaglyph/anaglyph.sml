structure Anaglyph =
struct
  exception Anaglyph of string

  structure Atom :>
  sig
    eqtype atom
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
    eqtype atoms
    val compare : atoms * atoms -> order
    val ++ : atoms * atoms -> atoms
    val -- : atoms * atoms -> atoms
    val fromlist : Atom.atom list -> atoms
    val tolist : atoms -> Atom.atom list
    val tostring : atoms -> string
    val zero : atoms
    val count : atoms * Atom.atom -> int
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

    val zero = Vector.tabulate (Atom.num_atoms, fn _ => 0)

    fun count (atoms, atom) = Vector.sub (atoms, Atom.toint atom)

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

    fun tolist atoms =
      let
        fun go i =
          if i = Atom.num_atoms then nil
          else
            case Vector.sub (atoms, i) of
              0 => go (i + 1)
            | n =>
                let
                  val a = case Atom.fromint i of
                    NONE => raise Anaglyph "impossible"
                  | SOME a => a
                in
                  List.tabulate (n, fn _ => a) @ go (i + 1)
                end
      in
        go 0
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

  fun char_atoms c =
    case Atom.decompose c of
      NONE => raise Anaglyph ("Bad character in char_atoms: " ^ implode [c])
    | SOME al => Atoms.fromlist al

  fun word_atoms w =
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
        | NONE => (w, word_atoms w)
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
     they must have the same set of atoms. *)
  fun makeplan (word1, word2) =
    let
      (* What we want out of a plan is to know what atoms to move in order
         to effect the change. There are often ways to move them that
         work, so we're trying to choose a "best" one.

         The output is a set of records, like so:
            (piece, (slot1, piecenum1), (slot2, piecenum2))
         where "piece" is the atom type being moved,
         the "slots" are character indices in the original strings,
         and the piecenums indicate which instance of the piece is
         being moved, if the letter contains multiple of them.
         (It is not specified which one is which; we just use some
         consistent assignment.)

         We additionally have a "height" for each atom. These allow
         us to move pieces simultaneous without too much overlap
         (a common defect without this is for the atoms to just move
         along the x axis through one another). See below.

         TODO: It's probably good to move sets of pieces together,
         especially entire letters..

         For this first pass, I'm not trying at all to minimize motion,
         because I just want to get a sense of what needs to improve to
         make good animations. The pairs are chosen arbitrarily. *)
      val (atoms1, atoms2) = (word_atoms word1, word_atoms word2)
      val () = if atoms1 = atoms2 then ()
               else raise Anaglyph ("makeplan with words that don't have " ^
                                    "matching atoms: " ^
                                    Atoms.tostring atoms1 ^ " vs " ^
                                    Atoms.tostring atoms2)

      (* Remove zero-atom elements from the head of the list. *)
      fun removezeros nil = nil
        | removezeros ((h as { atoms, ... }) :: t) =
        if Atoms.zero = atoms then removezeros t
        else h :: t

      (* In this version, every atom moves. We just pick the first
         remaining occurrence of the atom in word1, and target its first
         remaining occurrence in word2.

         Each letter position in word1 and word2 gets broken into the
         atoms that are yet unaccounted for. The occurrences can be seen
         as positive in remain1 and negative in remain2, but both will
         have positive coefficients for simplicity. *)

      fun process (l1, l2) =
        case (removezeros l1, removezeros l2) of
          (nil, nil) => nil
        | (nil, _) => raise Anaglyph "impossible: mismatch"
        | (_, nil) => raise Anaglyph "impossible: mismatch"
        | (remain1 as ({ idx = idx1, atoms = atoms1, already = already1 } :: rest1),
           remain2) =>
        let
          (*
          fun remaineltstring { idx, atoms, already } =
            Int.toString idx ^ ". " ^ Atoms.tostring atoms

          val () = print "---------------- \n"
          val () = print "Remain1:\n"
          val () = app (fn e => print ("  " ^ remaineltstring e ^ "\n")) remain1
          val () = print "Remain2:\n"
          val () = app (fn e => print ("  " ^ remaineltstring e ^ "\n")) remain2
          *)

          (* Pick an arbitrary atom from a nonzero set. Return it and
             the remaining atoms. *)
          fun pick_atom atoms =
            (case Atoms.tolist atoms of
               nil => raise Anaglyph "impossible: checked nonzero"
             | a :: left => (a, Atoms.fromlist left))

          val (a, atoms1) = pick_atom atoms1

          (* Figure out the index of the piece, and update the used
             counts. *)
          val occurrence1 = Atoms.count (already1, a)
          val already1 = Atoms.++ (already1, Atoms.fromlist [a])

          val remain1 = { idx = idx1, atoms = atoms1, already = already1 } :: rest1

          (* Find where the atom a is sent.
             Returns a new "remain2" list, and the rhs of the row for the overall
             return from 'process'. *)
          fun alloc nil = raise Anaglyph "impossible: mismatch in alloc"
            | alloc ((h as { idx = idx2, atoms = atoms2,
                             already = already2 }) :: rest2) =
            if Atoms.count (atoms2, a) > 0
            then
              let
                val occurrence2 = Atoms.count (already2, a)
                val atoms2 = Atoms.-- (atoms2, Atoms.fromlist [a])
                val already2 = Atoms.++ (already2, Atoms.fromlist [a])
                val row_rhs = (idx2, occurrence2)
              in
                ({ idx = idx2, atoms = atoms2, already = already2 } :: rest2,
                 row_rhs)
              end
            else
              (* No matching atom here. *)
              let val (rest2, row) = alloc rest2
              in (h :: rest2, row)
              end

          val row1 = (idx1, occurrence1)
          val (remain2, row2) = alloc remain2
        in
          (a, row1 : int * int, row2 : int * int) :: process (remain1, remain2)
        end

      fun makeremains w =
        List.tabulate (size w,
                       fn i =>
                       let
                         val c = String.sub(w, i)
                         val atoms = char_atoms c
                       in
                         { idx = i, atoms = atoms, already = Atoms.zero }
                       end)
      val remain1 = makeremains word1
      val remain2 = makeremains word2

      val rows = process (remain1, remain2)

      (* Now, we allocate heights. A height is an integer; positive is up,
         negative is down, and zero is along the word's axis. The goal is
         to minimize the total height without pieces moving through one
         another. One piece moves through another if their vectors
         (src slot -> dst slot) overlap and they are at the same height.

         To begin, just setting arbitrary heights to see what it looks like. *)

      (* Asymmetric because the baseline is the x-axis; XXX should fix? *)
      val MAX_HEIGHT = 2
      val MIN_HEIGHT = ~3
      val NUM_HEIGHTS = MAX_HEIGHT - MIN_HEIGHT

      fun addheights _ nil = nil
        | addheights cur ((atom, src, dst) :: rest) = (atom, cur, src, dst) ::
        addheights (if cur = MAX_HEIGHT then MIN_HEIGHT else (cur + 1)) rest

      val rows = addheights MIN_HEIGHT rows

      fun rowstring (atom, height, (slot1, piece1), (slot2, piece2)) =
        "{a:\"" ^ implode [Atom.tochar atom] ^ "\",h:" ^ Int.toString height ^
        ",ss:" ^ Int.toString slot1 ^ ",sp:" ^ Int.toString piece1 ^
        ",ds:" ^ Int.toString slot2 ^ ",dp:" ^ Int.toString piece2 ^
        "}"
    in
      print ("let startword = '" ^ word1 ^ "';\n" ^
             "let endword = '" ^ word2 ^ "';\n" ^
             "let plan = [\n  ");
      print (StringUtil.delimit ",\n  " (map rowstring rows));
      print "\n];\n";
      ()
    end

end
