structure Anaglyph =
struct
  exception Anaglyph of string

  structure Atom =
  struct
    type atom = char
    val compare = Char.compare
    val atomchars = "ceors'.?"
    val c_ = chr 0
    val e_ = chr 1
    val o_ = chr 2
    val r_ = chr 3
    val s_ = chr 4
    (* half-height vertical bar, like h = n + ' *)
    val tick_ = chr 5
    (* the dot on i and j *)
    val dot_ = chr 6
    (* just the asymmetrical descender hook of g, j *)
    val hook_ = chr 7

    val num_atoms = size atomchars

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
      | _ => NONE

  (* some quesitonable ideas:
     e -> c '
     h -> y
     m -> w
     o -> c c
     *)
  end

  fun load_dictionary () =
    let
      val lines = Script.linesfromfile "wordlist.asc"
      fun oneword w =
        let
          fun getnorm c =
            case Atom.decompose c of
              NONE => raise Anaglyph ("Bad character in word: " ^ w)
            | SOME al => al
          val atom_list = List.concat (map getnorm (explode w))
          val atom_list = ListUtil.sort Atom.compare atom_list
          val atoms = implode (atom_list)

        in
          (w, atoms)
        end
    in
      map oneword lines
    end

  val _ = load_dictionary ()

end
