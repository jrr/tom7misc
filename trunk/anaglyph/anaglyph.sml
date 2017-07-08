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
    (* Multiset intersection (pointwise 'min') *)
    val intersect : atoms * atoms -> atoms
    val subseteq : atoms * atoms -> bool
    val size : atoms -> int
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

    fun intersect (a, b) =
      Vector.tabulate (Atom.num_atoms, fn i =>
                       Int.min (Vector.sub (a, i),
                                Vector.sub (b, i)))

    fun subseteq (a, b) =
      (* PERF *)
      intersect (a, b) = a

    fun size a = Vector.foldl op+ 0 a

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

      (* To make a good plan, we want as few collisions as possible,
         and we want to use as few different heights as possible.

         A collision happens between the atoms moving in two rows.
         Specifically, let dst{1,2} and src{1,2} be the destination
         and source slots for the two atoms. Then:
             v1 = dst1 - src1
             v2 = dst2 - src2
             overlap = ... XXX they are overlapping ...

             collision = sign(v1) != sign(v2) && overlap

        Actually this doesn't work: One atom moving to the right
        might need to cross a slower-moving atom also moving to
        the right.
        If it did work, we could fix all this pretty simply by just
        putting all the stuff with positive sign at height ~1 and
        with negative sign at 1. This is what I'm currently doing,
        actually. *)

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
        | (remain1 as ({ idx = idx1, atoms = atoms1,
                         already = already1 } :: rest1),
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

          val remain1 = { idx = idx1, atoms = atoms1,
                          already = already1 } :: rest1

          (* Find where the atom a is sent.
             Returns a new "remain2" list, and the rhs of the row for
             the overall return from 'process'. *)
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

         Here we just assign heights (to 3 different channels)
         depending on which direction the atom is moving, which is not
         so bad. *)

      (* Asymmetric because the baseline is the x-axis; XXX should fix? *)
      val MAX_HEIGHT = 2
      val MIN_HEIGHT = ~3
      val NUM_HEIGHTS = MAX_HEIGHT - MIN_HEIGHT

      fun addheights nil = nil
        | addheights ((atom, src as (sslot, _), dst as (dslot, _)) :: rest) =
        let
          val height =
            case Int.compare (sslot, dslot) of
              EQUAL => 0
            | LESS => ~1
            | GREATER => 1
        in
          (atom, height, src, dst) :: addheights rest
        end

      val rows = addheights rows

      (* Don't use ~ for json. *)
      fun itos n = if n < 0 then "-" ^ Int.toString (0 - n)
                   else Int.toString n

      fun rowstring (atom, height, (slot1, piece1), (slot2, piece2)) =
        "{a:\"" ^ implode [Atom.tochar atom] ^ "\",h:" ^ itos height ^
        ",ss:" ^ itos slot1 ^ ",sp:" ^ itos piece1 ^
        ",ds:" ^ itos slot2 ^ ",dp:" ^ itos piece2 ^
        "}"
    in
      print ("let startword = '" ^ word1 ^ "';\n" ^
             "let endword = '" ^ word2 ^ "';\n" ^
             "let plan = [\n  ");
      print (StringUtil.delimit ",\n  " (map rowstring rows));
      print "\n];\n";
      ()
    end

  structure Tree =
  struct
    (* The main act of anagramming (a phrase) is picking a word
       that can be made with the remaining letters in our set,
       then subtracting those letters from the set, and repeating.
       We'll call this an "eligible" word.

       We can find all the eligible words by looping over the entire
       dictionary, but that's wasteful because many words will be
       ineligible (especially when we have few letters remaining).

       So here we can create an eligibility tree. Take all the
       clusters above as leaves. Each cluster is a set of words
       and the exact set of letters that they contain. The words
       are eligible if the remaining letters are a superset of
       the letters in the set.

       Say we have two clusters

       rs5'?  = sky, flus
       rs5'.? = sizy, juts, just, fish

       These could be joined into a node containing the
       intersection of the sets.

          rs5'.?
          | |
          | +- rs5'? = sky, flus
          +--- rs5'.? = skizy, juts, just, fish

       The joined node means: "If you don't have at least these
       letters remaining, you won't be able to match any words in this
       subtree." We can always join two subtrees together (the intersection
       is always defined), until we get a tree for the entire dictionary
       (likely rooted with the empty set). There are two things to tend
       to when considering the quality of the tree:
        - The intersection operation is lossy; if the set gets too small
       then we have to search it for many cases where it won't end up
       being useful.
        - The tree should be balanced; search will always be linear
       if the tree structure is linear.

       Without any guarantees, a simple way to do this is to
       repeatedly take two trees of similar size and merge them. This
       could be done pretty efficiently by keeping a set (heap) of
       trees ordered by size (depth?) and then plucking off the two
       smallest ones and joining them. This would not do a good job
       of maximizing the size of the new intersected roots, though.

       We could also do this by insertion. Start with any tree, and
       insert clusters into it. Say we have

                    s1
                   /  \            inserting s
                  s2  s3

       We can insert s anywhere (well, anywhere we have a null child),
       but along the entire path we must intersect s with the interior
       node's set. At each steo how do we choose which node to descend
       into? We could pick the one that has the larger intersection,
       perhaps breaking ties by descending into the smaller of the two
       trees.

       Finally, note that there's no reason for the tree to be binary,
       and there's no meaningful ordering being used between the
       children. So when we insert, if the intersections with the
       existing children are too lossy, we could just insert an
       additional child. By the pigeonhole principle, if there are N
       (number of atoms) non-empty children of some node, at least one
       of them must have a non-empty intersection with any given set.
       So N makes a natural branching factor?
       *)

    (* A node contains its atoms, a list of subtrees, and the
       words that have exactly that set.

       Representation invariants:
       In each node, the atoms is exactly the intersection of
       all the atoms within the subtree.

       No child node's atoms can be a subset of another's
       (including being equal to it). *)
    datatype tree =
      Node of Atoms.atoms * tree list * string list

    fun get_atoms (Node (atoms, _, _)) = atoms
    fun get_trees (Node (_, trees, _)) = trees
    fun get_words (Node (_, _, words)) = words

    val empty = Node(Atoms.zero, nil, nil)

    fun depth (Node (_, nl, _)) = 1 + List.foldl Int.max 0 (map depth nl)

    fun size (Node (_, nl, _)) = 1 + List.foldl Int.+ 0 (map size nl)

    fun tostring tree =
      let
        fun ts depth (Node (atoms, children, words)) : string list =
          let
            val indent = CharVector.tabulate (depth * 2, fn _ => #" ")
            val aa = Atoms.tostring atoms
            val line = indent ^ Int.toString depth ^ ". " ^ aa ^
              (case words of
                 nil => "\n"
               | _ => " = " ^ StringUtil.delimit "," words ^ "\n")
          in
            line ::
            map (String.concat o ts (depth + 1)) children
          end
      in
        String.concat (ts 0 tree)
      end

    (* Insert the subtree into the tree.

       Requires that the subtree's atoms are a superset of the tree's,
       which means that the resulting tree keeps that same set of atoms. *)
    fun insert (orig as (Node (atoms, nodes, words)), subtree) : tree =
      let
        (*
        val () = print ("===========\nInsert: " ^ tostring subtree ^
                        "\n --- INTO --- \n" ^ tostring orig ^
                        "\n ============ \n")
        *)

        val Node (subtree_atoms, subtree_nodes, subtree_words) = subtree
        (* PERF debugging *)
        val () = if Atoms.subseteq (atoms, subtree_atoms)
                 then ()
                 else raise Anaglyph ("Invariant violation: Inserted " ^
                                      "subtree atoms " ^
                                      Atoms.tostring subtree_atoms ^
                                      " must be subset of tree's: " ^
                                      Atoms.tostring atoms)
      in
        (* If this subtree actually has the same atoms, we instead should
           insert all its children here. *)
        if atoms = subtree_atoms
        then
          let
            (* Merge in the words. *)
            val tree = Node (atoms, nodes, words @ subtree_words)
            (* Now we just need to deal with subtree_nodes. Insert
               each one into this tree. We know that it meets the
               atoms subset requirement if the existing subtree is
               well-formed. *)
            fun insertl (tree, nil) = tree
              | insertl (tree, n :: l) =
              insertl (insert (tree, n), l)
          in
            insertl (tree, subtree_nodes)
          end
        else
          let
            datatype pref =
              (* Carrying the size of the remainder. 0 means equal,
                 which is best. *)
              Superset of int
              (* Same, but we don't generate 0 *)
            | Subset of int
            (* Any overlap. Carries size; larger is better. *)
            | Overlap of int

            (* LESS means better. *)
            fun compare_pref (Superset x, Superset y) = Int.compare (x, y)
              | compare_pref (Superset _, _) = LESS
              | compare_pref (_, Superset _) = GREATER
              | compare_pref (Subset x, Subset y) = Int.compare (x, y)
              | compare_pref (Subset _, _) = LESS
              | compare_pref (_, Subset _) = GREATER
              | compare_pref (Overlap x, Overlap y) = Int.compare (y, x)

            fun node_pref (Node (natoms, _, _)) =
              let
                val isect = Atoms.intersect (natoms, subtree_atoms)
              in
                (* If the intersection is natoms, then it's a superset. *)
                if isect = natoms
                then Superset (Atoms.size subtree_atoms - Atoms.size natoms)
                else
                  if isect = subtree_atoms
                  then Subset (Atoms.size natoms - Atoms.size subtree_atoms)
                  else Overlap (Atoms.size isect)
              end

            (* This means that the subtrees atoms are strictly greater.
               We'll either insert it as a new child, or into one of
               the existing children. Now choose the best insertion node. *)

            fun gb NONE done nil = NONE
              | gb (SOME (pref, best)) done nil =
              (case pref of
                 (* An overlap of 0 is the worst and doesn't even count. *)
                 Overlap 0 => NONE
               | _ => SOME (pref, best, done))
              | gb cur done (node :: rest) =
                 let val pref = node_pref node
                 in
                   case cur of
                     NONE => gb (SOME (pref, node)) done rest
                   | SOME (best_pref, best_node) =>
                       (case compare_pref (pref, best_pref) of
                          LESS => gb (SOME (pref, node))
                            (best_node :: done) rest
                        | _ => gb (SOME (best_pref, best_node))
                            (node :: done) rest)
                 end
          in
            case gb NONE nil nodes of
              (* If there are no candidates, just add it as a new
                 child. *)
              NONE => Node (atoms, subtree :: nodes, words)
            | SOME (pref, child, others) =>
                (* If it is equal or a superset, we can just recurse. *)
                (case pref of
                   Superset _ =>
                     Node (atoms, insert (child, subtree) :: others, words)
                 | Subset _ =>
                     (* We know that no other tree is a superset of subtree,
                        but how do we know that none is a subset? XXX *)
                     Node (atoms, insert (subtree, child) :: others, words)
                 | Overlap _ =>
                     (* XXX should merge it with some child sometimes! *)
                     Node (atoms, subtree :: child :: others, words)
                     (*
                     (* But if we chose it due to overlap, then we
                        need to create an intermediate node with the
                        intersected set. *)
                     let val isect =
                       Atoms.intersect (subtree_atoms, get_atoms child)
                     in
                       (* XXX Why must the subtree have a superset of
                          the atoms?
                          XXX termination argument? *)
                       insert (Node (atoms, others, words),
                               (Node (isect, [subtree, child], nil)))

                       (* XXX this can violate the relationship between
                          this new node and its parent/siblings. hmm *)
                       (* Node(atoms,
                          Node (isect, [subtree, child], nil) :: others,
                          words) *)
                     end
                   *)
                     )
          end

      end

    fun treedot tree =
      let
        val nodes : string list ref = ref nil
        val links : string list ref = ref nil
        val ctr = ref 0
        fun traverse parent (Node (atoms, children, words)) =
          let
            val aa = Atoms.tostring atoms
            val name = "n" ^ Int.toString (!ctr)
            val () = ctr := !ctr + 1

            val () =
              case words of
                nil =>
                  nodes := (" " ^ name ^ " [label=\"" ^ aa ^ "\"]\n") :: !nodes
              | wl =>
                  let
                    val ww = StringUtil.delimit "," wl
                    val MAX_CHARS = 12
                    val ww =
                      if String.size ww > MAX_CHARS
                      then String.substring(ww, 0, MAX_CHARS - 3) ^ "..."
                      else ww
                  in
                    nodes := (" " ^ name ^ " [label=\"" ^ aa ^ "; " ^
                              ww ^ "\" shape=box]\n" (* " *)) :: !nodes
                  end
          in
            (case parent of
              NONE => ()
            | SOME p => links := (" " ^ p ^ " -> " ^ name ^ "\n") :: !links);
            app (traverse (SOME name)) children
          end
      in
        traverse NONE tree;
        "digraph tree {\n" ^
        String.concat (!nodes) ^
        String.concat (!links) ^
        "}\n"
      end

  end

  val tree = AM.foldli (fn (atoms, words, tree) =>
                        Tree.insert (tree, Tree.Node (atoms, nil, words)))
    Tree.empty (!clusters)

  fun tree_dotfile () = Tree.treedot tree
  fun tree_textfile () = Tree.tostring tree

  val () = TextIO.output
    (TextIO.stdErr,
     "Tree depth: " ^ Int.toString (Tree.depth tree) ^ "\n")
  val () = TextIO.output
    (TextIO.stdErr,
     "Tree size: " ^ Int.toString (Tree.size tree) ^ "\n")

  (* Print the 100 longest words that can be made from the phrase,
     after subtracting the required phrase. *)
  fun best_requiring req phrase =
    let
      val required_atoms = word_atoms req
      val orig_phrase_atoms = word_atoms phrase
      val () =
        if Atoms.intersect (required_atoms, orig_phrase_atoms) =
           required_atoms
        then ()
        else raise Anaglyph ("required phrase " ^ req ^ " is not possible " ^
                             "from anagraph phrase " ^ phrase)
      val phrase_atoms = Atoms.--(orig_phrase_atoms, required_atoms)

      val () = TextIO.output (TextIO.stdErr,
                              "Remaining atoms: " ^
                              Atoms.tostring phrase_atoms ^ "\n")

      val usable : string list ref = ref nil
      fun onenode (catoms, words) =
        let val isect = Atoms.intersect (catoms, phrase_atoms)
        in
          if isect = catoms
          then usable := words @ !usable
          else ()
        end

      val () = AM.appi onenode (!clusters)
      (* Descending *)
      fun bylength (a, b) = Int.compare (size b, size a)
      val () = TextIO.output (TextIO.stdErr,
                              (Int.toString (length (!usable)) ^
                               " words can be made\n"))
      val usable = ListUtil.sort bylength (!usable)
      val usable = ListUtil.takeupto 200 usable
    in
      app (fn w => print (w ^ "\n")) usable
    end

  fun anaglyph_requiring maxwords req phrase =
    let
      val required_atoms = word_atoms req
      val orig_phrase_atoms = word_atoms phrase
      val () =
        if Atoms.intersect (required_atoms, orig_phrase_atoms) =
           required_atoms
        then ()
        else raise Anaglyph ("required phrase " ^ req ^ " is not possible " ^
                             "from anagraph phrase " ^ phrase)
      val phrase_atoms = Atoms.--(orig_phrase_atoms, required_atoms)

      (* Call emit on all anagrams of the remaining atoms.
         An anagram is presented as a list of word clusters; each
         word in the cluster uses the same set of atoms so they
         can be freely chosen. *)
      fun enumerate (emit : string list list -> unit) remaining =
        let
          fun enum (l, wordsleft, r) =
            if r = Atoms.zero
            then emit l
            else
              let
                fun walk (Tree.Node (atoms, children, words), wordsleft, r) =
                  let
                    val isect = Atoms.intersect (atoms, r)
                  in
                    (* Do I have enough letters to enter this node? *)
                    if isect = atoms
                    then
                      let in
                        (* First recurse; this puts anagrams with longer
                           words first. *)
                        app (fn n => walk (n, wordsleft, r)) children;

                        (* Take words at the current node; this reduces
                           our set of atoms. *)
                        (case words of
                           nil => ()
                         | _ => enum (words :: l,
                                      wordsleft - 1,
                                      Atoms.-- (r, atoms)))
                      end
                    else ()
                  end
              in
                if wordsleft > 0
                then walk (tree, wordsleft, r)
                else ()
              end
        in
          enum (nil, maxwords, remaining)
        end

      fun printone l =
        let
          fun barre [w] = w
            | barre wl = StringUtil.delimit "|" wl
        in
          print (StringUtil.delimit " " (map barre l) ^ "\n")
        end
    in
      enumerate printone phrase_atoms
    end

  fun anaglyph phrase = anaglyph_requiring 1000 "" phrase

end
