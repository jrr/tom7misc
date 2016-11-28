structure Tactics =
struct

  exception Tactics of string

  infixr 9 `
  fun a ` b = a b

  open X86
  infix <- <~

  val PRINT_LOW : Word8.word = 0wx20
  val PRINT_HIGH : Word8.word =  0wx7e

  fun ftos f = Real.fmt (StringCvt.FIX (SOME 2)) f

  (* High, low. NONE means don't know (on input) or don't care (on output) *)
  type reg16value = Word8.word option * Word8.word option

  (* Find printable C such that AND (AL, C) = VL; of course not always possible. *)
  fun inverse_and (al, vl) =
    let
      (* PERF -- don't need to search all chars. *)
      fun r 0wx7f = NONE
        | r c = if Word8.andb(al, c) = vl
                then SOME c
                else r (Word8.+(c, 0w1))
    in
      r 0wx20
    end

  (* Load an arbitrary value into AX.
     If the requested value in AH is NONE ("don't care"), the existing value is preserved.
     Can trash flags. Only AX is modified.

     PERF! Should do some kind of search with dynamic programming, etc.
     There are still lots of really bad sequences in here (36 bytes to
     go from 0 to 80!!) *)
  (* Note that loading EAX (or any register) can be easily accomplished by pushing two
     16-bit literals with this routine, then POP with the operand size prefix. *)
  (* XXX probably better to separate this out into routines that do small parts,
     like one that loads ah with some known values. *)
  fun load_ax16 (ax : reg16value) (v : reg16value) : (reg16value * ins list) =
    case (ax, v) of
      (* Somehow we don't care what the value is at all *)
      (known, (NONE, NONE)) => (known, nil)
    | (known as (known_ah, SOME al), (NONE, SOME vl)) =>
        (* Loading AL only. *)
        if al = vl
        (* We already have the value we wanted! *)
        then ((known_ah, SOME vl), nil)
        else
          (* Best is if we can emit a single byte instruction INC or
             DEC to transform it. We don't DEC 0 nor INC FF, because
             these also change AH. Note that this issue does not
             apply to SUB, because it is an 8-bit operation.
             PERF: If we really don't care about the value of AH, or
             just want to propagate what we know about it, we could
             use 16-bit inc/dec here... *)
          if al <> 0wxFF andalso vl = Word8.+ (al, 0w1)
          then ((known_ah, SOME vl), [INC AX])
          else if al <> 0wx00 andalso vl = Word8.- (al, 0w1)
               then ((known_ah, SOME vl), [DEC AX])
               else
          (* If we know AL, we can often get the result we want
             immediately by XOR, SUB, or AND with an immediate. *)
          let val x = Word8.xorb (al, vl)
          in
            if x >= PRINT_LOW andalso x <= PRINT_HIGH
            then ((known_ah, SOME vl), [XOR_A_IMM ` I8 x])
            else
              let
                (* we want VL = AL - s,
                       s + VL = AL
                       s = AL - VL *)
                val s = Word8.- (al, vl)
              in
                (* print ("al: " ^ Word8.toString al ^
                   " vl: " ^ Word8.toString vl ^
                   " s: " ^ Word8.toString s ^ "\n"); *)
                if s >= PRINT_LOW andalso s <= PRINT_HIGH
                then ((known_ah, SOME vl), [SUB_A_IMM ` I8 s])
                else
                  (* AND is occasionally a good way, for example, if
                     AL already contains 0xFF and VL is printable. *)
                  case inverse_and (al, vl) of
                    SOME c => ((known_ah, SOME vl), [AND_A_IMM ` I8 c])
                  | NONE =>
                      let
                        (* XXX PERF be smarter here. This does terminate for
                           all pairs, but takes as many as 36 bytes (maybe more),
                           like for 0xFF -> 0x80. *)
                        val s1 = 0wx7e
                        val (kt, t) =
                          load_ax16 (known_ah, SOME (Word8.- (al, s1))) (NONE, SOME vl)
                      in
                        (kt, SUB_A_IMM ` I8 s1 :: t)
                      end
              end
          end
    | ((known_ah, NONE), value as (NONE, SOME vl)) =>
      (* Put *some* known value in AL so that we can do the routine above.
         PERF: Of course, some choices are better than others here! If
         we had a precomputed table of src*dst pairs, we could try to
         compute a good one. *)
          let val (kt, t) = load_ax16 (known_ah, SOME 0w0) value
          in
            (kt,
             AND_A_IMM ` I8 0wx40 ::   (* ASCII '@', dec 64 *)
             (* AL either contains 0x40 or 0x00. *)
             AND_A_IMM ` I8 0wx20 ::   (* ASCII ' ', dec 32 *)
             (* Now AL definitely contains 00. *)
             t)
          end

    (* PERF: Loading AH when something is known. *)

    | (known, value as (SOME vh, NONE)) =>
    (* Loading something into AH. One reasonably brief way to do this is to misalign the stack.
       Load the desired value into AL, so that we have AH=ah?, AL=vh.
       PUSH AX, giving
                AL AH
                 |  |
                 v  v
          SP -> vh ah? ?? ?? ?? ...
       DEC SP, giving
          SP -> ?? vh ah? ?? ?? ?? ...
       POP AX, giving
          SP -> ?? vh ah? ?? ?? ?? ...
                 |  |
                 v  v
                AL AH
       INC SP (keep stack aligned; avoid unbounded growth) *)
       let
         val (known, load_low) = load_ax16 known (NONE, SOME vh)
       in
         ((SOME vh, NONE),
         load_low @
          [PUSH AX,
           DEC SP,
           POP AX,
           INC SP])
       end

    | ((known_ah, known_al), value as (SOME vh, SOME vl)) =>
       (* If AH already has what we need, we can skip a lot. *)
       if known_ah = SOME vh
       then load_ax16 (known_ah, known_al) (NONE, SOME vl)
       else
       let
         val (known, load_high) = load_ax16 (known_ah, known_al) (SOME vh, NONE)
         val (known, load_low) = load_ax16 known (NONE, SOME vl)
       in
         (known, load_high @ load_low)
       end

  (* Load a 16-bit register (!= ax) with the specific value.
     May use stack but restores it.
     Can trash flags.
     Only AX and target register are modified. *)
  (* fun load_reg16 (ax : reg16value, rx : reg16value) (r : reg) (v : Word16.word) = *)

  (* XXX this should return the known values *)
  (* Generate code that prints the string. Uses the interrupt instruction, so non-ASCII. *)
  fun printstring s =
    let
      fun emit known_ax nil = nil
        | emit known_ax (c :: rest) =
        let
          val c = Word8.fromInt ` ord c
          (* Load AH=06, AL=char *)
          val (known_ax, axins) =
            load_ax16 known_ax (SOME 0wx06, SOME c)
        in
          axins @
          [PUSH AX,
           POP DX,
           INT 0wx21] @
          (* interrupt returns character written in AL *)
          emit (SOME 0wx06, SOME c) rest
        end
    in
      emit (NONE, NONE) (explode s)
    end
  (*
    List.concat `
    map (fn c =>
         (* PERF:
            - no need to repeatedly set AH (interrupt num)
            - can XOR with previous value of DL
            - AL gets the previous character written
            - if repeated character, skip loading *)
         [XOR (S8, D <- Register D),
          (* Need to use both AH and AL *)
          XOR (S16, A <- Register A),
          XOR_A_IMM (I8 ` Word8.fromInt ` ord c),
          XOR (S8, D <- Register A),
          (* A contains c. Make it contain 2. *)
          (* 02: Write character to stdout. DL=char. *)
          (* 06: Direct console output *)
          XOR_A_IMM (I8 ` Word8.xorb (0wx06, Word8.fromInt ` ord c)),
          XOR (S8, AH_SP <- Register A),
          INT 0wx21]) (explode s)
*)

  (* Exits the program. Uses the interrupt instruction, so non-ASCII. *)
  fun exit () =
    [XOR (S16, A <- Register A),
     XOR_A_IMM (I16 (Word16.fromInt 0x4c00)),
     INT 0wx21]

end
