structure WriteTest =
struct

  exception WriteTest of string

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

  fun writecom () =
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
      val headersize = vec [0wx20, 0wx20]
      (* Min/max number of 16-byte paragraphs required above
         the end of the program. I think this is like BSS? *)
      val minmemory = vec [0wx20, 0wx20]
      val maxmemory = vec [0wx20, 0wx20]
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

      val padding_size = 1048576 - Word8Vector.length header
      val padding = Word8Vector.tabulate
        (padding_size,
         fn i =>
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
