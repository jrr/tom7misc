structure Anaglyph =
struct
  nonfix o
  structure G =
  struct
    datatype glyph =
      (* Regular letters *)
      a | b | c | d | e | f | g | h | i | j | k | l | m |
      n | o | p | q | r | s | t | u | v | w | x | y | z
      (* Funny business *)
      (* half-height vertical bar, like h = n + ' *)
      | tick
      (* the dot on i and j *)
      | dot
      (* j without dot *)
      | hook

    (* TODO: Compute the transitive closure. *)
    val maps =
      [(b, [l, c]),
       (d, [l, c]),
       (p, [l, c]),
       (q, [l, c]),
       (m, [r, n]),
       (w, [v, v]),
       (n, [u]),
       (k, [l, v]),
       (x, [v, v]),
       (v, [tick, tick]),
       (l, [tick, tick]),
       (z, [v, tick]), (* questionable *)
       (z, [tick, tick, tick]),
       (v, [tick, tick]),
       (t, [l, tick]),
       (d, [a, tick]),
       (i, [tick, dot]),
       (h, [n, tick]),
       (n, [r, tick]),
       (f, [hook, tick]),
       (j, [hook, dot]),
       (g, [hook, c]),

       (a, [c, tick])]

       (* some quesitonable ideas:
          e -> c '
          h -> y
          m -> w
          o -> c c
          *)

    fun gtoc (glyph : glyph) : char option =
      case glyph of
        a => SOME #"a"
      | b => SOME #"b"
      | c => SOME #"c"
      | d => SOME #"d"
      | e => SOME #"e"
      | f => SOME #"f"
      | g => SOME #"g"
      | h => SOME #"h"
      | i => SOME #"i"
      | j => SOME #"j"
      | k => SOME #"k"
      | l => SOME #"l"
      | m => SOME #"m"
      | n => SOME #"n"
      | o => SOME #"o"
      | p => SOME #"p"
      | q => SOME #"q"
      | r => SOME #"r"
      | s => SOME #"s"
      | t => SOME #"t"
      | u => SOME #"u"
      | v => SOME #"v"
      | w => SOME #"w"
      | x => SOME #"x"
      | y => SOME #"y"
      | z => SOME #"z"
      | _ => NONE
  end

(* This problem is embeddable in linear logic. You have a bunch of rules of the form
   !(m  -o  r (x) n)
   and you're trying to take a word like
     p (x) o (x) r (x) n
   and prove a proposition like
     m (x) o (x) p
   (but like for all possible tensorings of words in the dictionary, at least up to
   the accessible length).

   Since we don't have any higher-order facts, it's probably not useful to use a full
   linear logic prover here.

   Note that we can simplify search somewhat. For example, 'b', 'd', 'p', and 'q' are
   all accessible from one another (b rotates to p, but we can also deconstruct d to
   c + l, then rotate the c, to form b). So we could just treat these as a single
   letter, and we could start by putting these in canonical form. It may be possible
   to find a normal form for the entire rule system? Let's look:

   b, d, p, q -> l c -> ' ' c
   w, x -> v v -> ' ' ' '
   v -> ' '
   z -> ' ' '
   i -> ' .
   j -> ? .
   f -> ? '
   g -> ? c
   a -> c '
   k -> l v -> ' '  ' '
   t -> l ' -> ' '  '
   h -> n '
   u, n -> r '
   m -> r n -> r r '

   (Note that u and n are in the same symmetry class, like b q.)

   So this does work out. We could canonize all letters by breaking them into their
   smallest parts, which are these:
   c e o r s y ' . ?
   (where ? is the hook symbol, a j without a dot).

   One could imagine breaking these up further. For example,
   y could be v + a little curvy (which could be part of the r or s if they break down).
   y could also be u + part of the hook (maybe hook is ' + cup)
   o could be c + c
   e could be c + '

   Everything still holds up with additional rules; they just break down the letters further.
   Note that there's no point in breaking s down into pieces if any of them is unique,
   because you'll always have to use it to make an s. This is inefficient.

   video ideas:
   start with like, "if I can rearrange the letters, why can't I flip them around?"
   .. then generalize.
   hamburgefontsiv
   linear logic
   loops u -> v -> u -> v -> ...
   extreme version: 7 segment display
   make a font that actually realizes this compositionality
   animate word transitions
   *)

end
