structure WriteTest =
struct

  exception WriteTest of string

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
             these also change AH. PERF: If we really don't care
             about the value of AH, or just want to propagate what
             we know about it, we could use 16-bit inc/dec here... *)
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
                   so s = -(VL - AL) *)
                val s = Word8.- (0w0, Word8.- (vl, al))
              in
                if s >= PRINT_LOW andalso s <= PRINT_HIGH
                then ((known_ah, SOME vl), [SUB_A_IMM ` I8 x])
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
                      val (kt, t) = load_ax16 (known_ah, SOME (Word8.- (al, s1))) (NONE, SOME vl)
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

    | _ => raise WriteTest "unimplemented"

  (* Load a 16-bit register (!= ax) with the specific value.
     May use stack but restores it.
     Can trash flags.
     Only AX and target register are modified. *)
  (* fun load_reg16 (ax : reg16value, rx : reg16value) (r : reg) (v : Word16.word) = *)

  fun all_combinations () =
    let val total = ref 0
    in
      Util.for 0 255
      (fn src =>
       Util.for 0 255
       (fn dst =>
        let
          val al = Word8.fromInt src
          val vl = Word8.fromInt dst
          val (known, ins) = load_ax16 (NONE, SOME al) (NONE, SOME vl)
          val ctx = CTX { default_32 = false }
          val bytes = Word8Vector.concat (map (encode ctx) ins)
          val n = Word8Vector.length bytes
        in
          (case known of
             (_, SOME r) => if r <> vl
                            then raise WriteTest "WRONG known value"
                            else ()
           | _ => raise WriteTest "UNKNOWN known value?");
          print (Word8.toString al ^ " -> " ^ Word8.toString vl ^ ": " ^
                 Int.toString n ^ "\n");
          total := !total + n
        end));
      print ("Total bytes: " ^ Int.toString (!total) ^ " (avg " ^
             ftos (real (!total) / 65536.0) ^ ")\n")
    end

  fun printstring s =
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

  fun exit () =
    [XOR (S16, A <- Register A),
     XOR_A_IMM (I16 (Word16.fromInt 0x4c00)),
     INT 0wx21]

  fun writecom () =
    let

      val prog =
        printstring "hello this is my cool program it so good\r\n" @
        exit ()

(*
      val prog = [NOP, NOP,
                  XOR (S16, A <- Register A),
                  XOR_A_IMM (I16 ` Word16.fromInt 0x2A2A),
                  PUSH AX,
                  PUSH AX,
                  PUSH AX,
                  PUSH AX,
                  PUSH AX,
                  PUSH AX,
                  PUSH AX,
                  PUSH AX]
*)

      val ctx = CTX { default_32 = false }

      val bytes =
        Word8Vector.concat (map (encode ctx) prog)

      (* val num_pad = 0xFD38 - 0x100 - Word8Vector.length bytes *)
      (* val num_pad = 0xFCF0 - 0x100 - Word8Vector.length bytes *)
      val num_pad = 0x10000 - 0x100 - Word8Vector.length bytes
      val padding = Word8Vector.tabulate
        (num_pad, fn i =>
         case i mod 2 of
           0 => 0wx71 (* JNO *)
         | _ => 0wx7E (* 126 *))

      val bytes = Word8Vector.concat [bytes, padding]
    in
      StringUtil.writefilev8 "dos/test.com" bytes
    end

  fun writeexe () =
    let

      val XXX_ZERO = vec [0w0, 0w0]

      (* Aside from the keyword "signature" being replaced with "magic",
         these are the same names as the DOSBox source code. Every entry
         is 16-bit. *)
      (* EXE Signature MZ or ZM *)
      val magic = vec [0wx5A, 0wx4D]
      (* Image size mod 512. We have to give an invalid value here, since
         we can't write 01 or 00 for the high bit. *)
      val extrabytes = vec [0wx7e, 0wx7e]
      (* Pages in file .
         Number of 512-byte pages. Since the maximum size is 1MB, we also
         have to give an invalid value here, but DOSBox ANDs the value
         with 07ff. This gives 0x67E * 512 = 850944 bytes.*)
      val pages = vec [0wx7e, 0wx7e]
      (* Number of relocation entries. We'd probably just like this
         to be zero, but... *)
      val relocations = XXX_ZERO (* vec [0wx20, 0wx20]; *)
      (* Paragraphs in header. A paragraph is 16 bytes.
         The minimum printable value is 0x2020 * 16 = 131584, which
         is bigger than we'd like. (CR/LF here?) *)
      (* val headersize = vec [0wx20, 0wx20] *)
      (* Actually we want the header size to be apparently large so that
         the image size fits in RAM with extra minmemory *)
      val headersize = vec [0wx7e, 0wx7e]
      (* Min/max number of 16-byte paragraphs required above
         the end of the program. I think this is like BSS? *)
      (* val minmemory = vec [0wx20, 0wx20]
         val maxmemory = vec [0wx20, 0wx20] *)
      (* we want this to be small, since DOS only has about 40557 paragraphs to
         give us. This value (little-endian) is 0b0111000000100000, which gets
         shifted up 4, overflowing all the 1 bits at the top. (XXX doesn't work;
         I guess it gets extended to 32 bits) *)
      (* val minmemory = vec [0wx20, 0wx70] *)
        (* CR/LF (0x0A0x0D) yields 48314; still too big... *)
      val minmemory = vec [0wx0A, 0wx0D]
      val maxmemory = minmemory
      (* Stack segment displacement, in 16-byte paragraphs.
         what does this mean? *)
      val initSS = vec [0wx6e, 0wx6e]
      val initSP = vec [0wx7e, 0wx7e]
      (* Checksum; usually ignored. 'AB' *)
      val checksum = vec [0wx41, 0wx42]
      val initIP = vec [0wx20, 0wx20]
      (* Displacement of code segment. *)
      val initCS = vec [0wx20, 0wx20]
      val reloctable = vec [0wx20, 0wx20]
      (* Tells the OS what overlay number this is. Should be 0
         for the main executable, but it seems to work if it's not *)
      val overlay = vec [0wx20, 0wx20]

      val header =
        Word8Vector.concat [magic,
                            extrabytes,
                            pages,
                            relocations,
                            headersize,
                            minmemory,
                            maxmemory,
                            initSS,
                            initSP,
                            checksum,
                            initIP,
                            initCS,
                            reloctable,
                            overlay]

      val prog =
        printstring "this is an asciicutable!\n" @
        exit ()
      val ctx = CTX { default_32 = false }
      val codebytes = Word8Vector.concat (map (X86.encode ctx) prog)
      val () = print ("# of code bytes: " ^
                      Int.toString (Word8Vector.length codebytes) ^ "\n")

      val codeseg =
        Word8Vector.concat
        [Word8Vector.tabulate (0x2020, fn _ => 0wx90),
         codebytes]

      val padding_size = 1048576 - Word8Vector.length header
      val padding = Word8Vector.tabulate
        (padding_size,
         fn i =>
         if i >= 0x09e9c4 andalso i < 0x09e9c4 + Word8Vector.length codeseg
         then
           (* Code segment *)
           Word8Vector.sub (codeseg, i - 0x09e9c4)
         else
         case i mod 6 of
           (* Load EAX, unique 32 bit number *)
           0 => 0wx66
         | 1 => 0wx25
         | 2 => Word8.fromInt ((i - 2) mod 256)
         | 3 => Word8.fromInt (((i - 3) div 256) mod 256)
         | 4 => Word8.fromInt (((i - 4) div (256 * 256)) mod 256)
         | _ => Word8.fromInt (((i - 5) div (256 * 256 * 256)) mod 256))

      val bytes = Word8Vector.concat [header, padding]
    in
      StringUtil.writefilev8 "dos/header.exe" bytes
    end

end
