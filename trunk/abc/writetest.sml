structure WriteTest =
struct

  infixr 9 `
  fun a ` b = a b

  open X86
  infix <- <~

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

  fun writetest () =
    let

      (*
      val prog =
        printstring "hello this is my cool program it so good\r\n"
*)
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

end
