
(* WordN for 0 <= N <= 32.
   For compatibility between mlton and NJ;
   mlton supports all of these, but NJ does
   not.

   Note that this won't overload constants or
   math ops at the new types. This is because
   overloading sucks.
*)

(* FIXME: sign extension is not implemented *)

functor WordNX(structure W : WORD
               val bits : int) :> WORD =
struct
    (* structure W = Word32 *)

    (* sanity check that we can implement a word of
       this size with the supplied structure *)
    val () = if W.wordSize < bits
             then raise Overflow (* maybe a better exception?? *)
             else ()

    (* representation invariant: always mod 2^bits *)
    type word = W.word

    val mask = W.>> (W.fromInt ~1, Word.fromInt W.wordSize - Word.fromInt bits)

    exception Unimplemented

    val wordSize = bits
    fun toLargeWord x = W.toLargeWord x
    fun toLargeWordX x = raise Unimplemented
    fun fromLargeWord x = W.andb(mask, W.fromLargeWord x)

    val toLarge = toLargeWord
    val toLargeX = toLargeWordX
    val fromLarge = fromLargeWord

    fun toInt x = W.toInt x
    fun toIntX x = raise Unimplemented

    fun toLargeInt x = W.toLargeInt x
    fun toLargeIntX x = raise Unimplemented

    fun fromLargeInt x = W.andb(mask, W.fromLargeInt x)
    fun fromInt x = W.andb(mask, W.fromInt x)

    fun ~ x = raise Unimplemented
    fun fmt _ = raise Unimplemented
    fun scan _ = raise Unimplemented

    val orb = W.orb
    val xorb = W.xorb (* ok since 0 xorb 0 = 0 *)
    val andb = W.andb
    fun notb x = W.andb(mask, W.notb x)

    (* assuming these are unsigned *)
    val compare = W.compare
    val min = W.min
    val max = W.max
    val op < = W.<
    val op > = W.>
    val op <= = W.<=
    val op >= = W.>=

    val toString = W.toString
    (* PERF: For some reason, sml/nj uses polyEqual for = here in popCount
       below. Doesn't make sense to me since it should be monomorphic... *)
    fun fromString s =
        (case W.fromString s of
             NONE => NONE
           | SOME w => if w = W.andb(mask, w)
                       then SOME w
                       else raise Overflow)


    fun ~>> _ = raise Unimplemented
    fun << (x, y) = W.andb(mask, W.<<(x, y))
    (* PERF: This doesn't need to be masked, right? *)
    fun >> (x, y) = W.andb(mask, W.>>(x, y))

    (* Population count is the number of 1-bits. This appears to be a new
       WORD extension ca. 2016, so for maximum compatibility I
       implemented it directly (rather than relying on an underlying
       Word.popCount). *)
    fun popCount (w : word) : int =
      let
        (* Note we cannot use pattern matching or literals on type
           Word, since it is a functor argument. *)
        fun pc (w : word, n : int) =
          if w = W.fromInt 0 then n
          else
            pc (W.>>(w, 0w1),
                if W.andb(w, W.fromInt 1) <> W.fromInt 0
                then n + 1
                else n)
      in
        pc (w, 0)
      end

    (* last, since overloaded.. *)
    fun x + y = W.andb(mask, W.+(x, y))
    (* ?? *)
    fun x - y = W.andb(mask, W.-(x, y))
    fun x * y = W.andb(mask, W.*(x, y))
    fun x div y = W.andb(mask, W.div(x, y))
    fun x mod y = W.andb(mask, W.mod(x, y))
end

(* word8 is not optional in basis *)
structure Word0 = WordNX(structure W = Word8 val bits = 0)
structure Word1 = WordNX(structure W = Word8 val bits = 1)
structure Word2 = WordNX(structure W = Word8 val bits = 2)
structure Word3 = WordNX(structure W = Word8 val bits = 3)
structure Word4 = WordNX(structure W = Word8 val bits = 4)
structure Word5 = WordNX(structure W = Word8 val bits = 5)
structure Word6 = WordNX(structure W = Word8 val bits = 6)
structure Word7 = WordNX(structure W = Word8 val bits = 7)

(* word32 is optional, but SML/NJ supports it *)
structure Word9  = WordNX(structure W = Word32 val bits = 9)
structure Word10 = WordNX(structure W = Word32 val bits = 10)
structure Word11 = WordNX(structure W = Word32 val bits = 11)
structure Word12 = WordNX(structure W = Word32 val bits = 12)
structure Word13 = WordNX(structure W = Word32 val bits = 13)
structure Word14 = WordNX(structure W = Word32 val bits = 14)
structure Word15 = WordNX(structure W = Word32 val bits = 15)
structure Word16 = WordNX(structure W = Word32 val bits = 16)
structure Word17 = WordNX(structure W = Word32 val bits = 17)
structure Word18 = WordNX(structure W = Word32 val bits = 18)
structure Word19 = WordNX(structure W = Word32 val bits = 19)
structure Word20 = WordNX(structure W = Word32 val bits = 20)
structure Word21 = WordNX(structure W = Word32 val bits = 21)
structure Word22 = WordNX(structure W = Word32 val bits = 22)
structure Word23 = WordNX(structure W = Word32 val bits = 23)
structure Word24 = WordNX(structure W = Word32 val bits = 24)
structure Word25 = WordNX(structure W = Word32 val bits = 25)
structure Word26 = WordNX(structure W = Word32 val bits = 26)
structure Word27 = WordNX(structure W = Word32 val bits = 27)
structure Word28 = WordNX(structure W = Word32 val bits = 28)
structure Word29 = WordNX(structure W = Word32 val bits = 29)
structure Word30 = WordNX(structure W = Word32 val bits = 30)
structure Word31 = WordNX(structure W = Word32 val bits = 31)

