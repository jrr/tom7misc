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
          XOR_A_IMM (I8 ` Word8.xorb (0wx02, Word8.fromInt ` ord c)),
          XOR (S8, AH_SP <- Register A),
          (* 02: Write character to stdout. DL=char. *)
          INT 0wx21]) (explode s)

  fun exit () =
    [XOR (S16, A <- Register A),
     XOR_A_IMM (I16 (Word16.fromInt 0x4c00)),
     INT 0wx21]

  fun writetest () =
    let
      val prog =
        printstring "hello this is my cool program it so good\r\n" @
        exit ()

      val ctx = CTX { default_32 = false }

      val bytes =
        Word8Vector.concat (map (encode ctx) prog)
    in
      StringUtil.writefilev8 "dos/test.com" bytes
    end

end
