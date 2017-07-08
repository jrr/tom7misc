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
    datatype node =
      N of Atoms.atoms * data
    and data =
        Words of string list
      | Nodes of node list

    val empty = N(Atoms.zero, Nodes nil)

    fun depth (N (_, Words _)) = 1
      | depth (N (_, Nodes nl)) = 1 + List.foldl Int.max 0 (map depth nl)

    fun size (N (_, Words _)) = 1
      | size (N (_, Nodes nl)) = 1 + List.foldl Int.+ 0 (map size nl)

    fun insert (orig as (N (node_atoms, data)), atoms, words) : node =
      let
        val node_atoms = Atoms.intersect (node_atoms, atoms)
      in
        case data of
          (* XXX this is not smart in the case that the atoms are
             equal, but that shouldn't happen the way we use it *)
          Words _ => N (node_atoms, Nodes [orig, N (atoms, Words words)])
        | Nodes nl =>
          (* There should be at most num_atoms children.
             Compare them all to find the one with the largest
             overlap. *)
            let
              (* gbo bestsofar done rest
                 Get the best overlap.
                 If "bestsofar" is SOME (overlap, node), then
                 we've found a node with nonzero intersection
                 with the current atoms.
                 "done" are nodes that we know aren't the best;
                 these must be part of the resturned list.

                 Doesn't preserve the order of the list. *)
              fun gbo NONE done (nil : node list) : node list =
                (* Didn't find any nonzero overlap, so just add
                   it as a new sibling. *)
                N (atoms, Words words) :: done
                | gbo (SOME (_, best_node)) done nil =
                let
                  (* Otherwise, insert it into the best node, which we
                     brought with us, and put that at the end. *)
                  (* PERF: computes intersection twice *)
                  val child as (N (child_atoms, _)) =
                    insert (best_node, atoms, words)

                  (* As a result of inserting into the child, it is
                     fairly common for the subtree's atoms field to
                     be equal to the current node's. When this happens,
                     we'll flatten all the grandchildren into the
                     current node. *)
                  fun flatten (child as (N (_, Words _)), done) =
                    (* But we can't flatten a word list. *)
                    child :: done
                    | flatten (N (_, Nodes gcl), done) =
                    (* XXX if two siblings have the same atoms,
                       they should also be merged. *)
                    gcl @ done
                in
                  if child_atoms = node_atoms
                  then flatten (child, done)
                  else child :: done
                end
                | gbo cur done ((child as N (child_atoms, _)) :: rest) =
                let
                  val overlap =
                    Atoms.size (Atoms.intersect (child_atoms, atoms))
                in
                  if overlap > 0
                  then
                    (case cur of
                       SOME (best_overlap, best_node) =>
                         if overlap > best_overlap
                         then gbo (SOME (overlap, child))
                                  (best_node :: done) rest
                         else gbo cur (child :: done) rest
                     | NONE => gbo (SOME (overlap, child)) done rest)
                  else gbo cur (child :: done) rest
                end
              val nl : node list = gbo NONE nil nl
            in
              N (node_atoms, Nodes nl)
            end
      end

    fun treedot tree =
      let
        val nodes : string list ref = ref nil
        val links : string list ref = ref nil
        val ctr = ref 0
        fun traverse parent (N (atoms, data)) =
          let
            val aa = Atoms.tostring atoms
            val name = "n" ^ Int.toString (!ctr)
            val () = ctr := !ctr + 1

            val () =
              case data of
                Words wl =>
                  let
                    val ww = StringUtil.delimit "," wl
                    val ww =
                      if String.size ww > 12
                      then String.substring(ww, 0, 9) ^ "..."
                      else ww
                  in
                    nodes := (" " ^ name ^ " [label=\"" ^ aa ^ "; " ^
                              ww ^ "\" shape=box]\n" (* " *)) :: !nodes
                  end
              | Nodes nl =>
                  nodes := (" " ^ name ^ " [label=\"" ^ aa ^ "\"]\n") :: !nodes
          in
            (case parent of
              NONE => ()
            | SOME p => links := (" " ^ p ^ " -> " ^ name ^ "\n") :: !links);
            (case data of
               Words _=> ()
             | Nodes nl => app (traverse (SOME name)) nl)
          end
      in
        traverse NONE tree;
        "digraph tree {\n" ^
        String.concat (!nodes) ^
        String.concat (!links) ^
        "}\n"
      end

    fun tostring tree =
      let
        fun ts depth (N (atoms, data)) : string list =
          let
            val indent = CharVector.tabulate (depth * 2, fn _ => #" ")
            val aa = Atoms.tostring atoms
            val line = indent ^ Int.toString depth ^ ". " ^ aa
            val line =
              case data of
                Words wl => line ^ " = " ^ StringUtil.delimit "," wl ^ "\n"
              | Nodes nl => line ^ "\n"
          in
            line ::
            (case data of
               Words wl => nil
             | Nodes nl =>
                 map (String.concat o ts (depth + 1)) nl)
          end
      in
        String.concat (ts 0 tree)
      end
  end

  val tree = AM.foldli (fn (atoms, words, tree) =>
                        Tree.insert (tree, atoms, words))
    Tree.empty (!clusters)

  fun tree_dotfile () = Tree.treedot tree
  fun tree_textfile () = Tree.tostring tree

  val () = print ("Tree depth: " ^ Int.toString (Tree.depth tree) ^ "\n")
  val () = print ("Tree size: " ^ Int.toString (Tree.size tree) ^ "\n")

  fun anaglyph phrase =
    let
      val atoms = word_atoms phrase

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
    in
      print (Atoms.tostring atoms ^ "\n")
    end

end
