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

    (*
  fun writecom () =
    let

      (* XXX: This won't work any more, since we assume that we can use temporaries in
         DS, but DS = CS. Maybe for COM files we can start by moving DS to the next 64k,
         just assuming that ram is safe to use? *)
      val prog =
        Tactics.initialize () @
        Tactics.printstring "hello this is my cool program it so good\r\n" @
        Tactics.exit ()

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
*)

  fun writeexe () =
    let

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
      (* Number of relocation entries. We want this to be the minimum
         value we can, since it's totally wasted space. *)
      val relocations = vec [0wx20, 0wx20]
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
      (* Offset in the file that begins the relocations. *)
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

      val (mach, insa) = Tactics.initialize ()
      val (mach, insb) = Tactics.load_ax16 mach (Word16.fromInt 0xABCD)
      val (mach, insc) = Tactics.not_ax16 mach
      val (mach, insd) = Tactics.printstring mach "this is an asciicutable!\n"
      val (inse) = Tactics.exit mach ()

      local
        val /// = Tactics.Instructions.///
        infix ///
      in
        val prog =
          Tactics.Instructions.get
          (insa ///
           insb ///
           insc ///
           insd ///
           inse)
      end

      val ctx = CTX { default_32 = false }
      val codebytes = Word8Vector.concat (map (X86.encode ctx) prog)
      val () = print ("# of code bytes: " ^
                      Int.toString (Word8Vector.length codebytes) ^ "\n")

      val codeseg =
        Word8Vector.concat
        [Word8Vector.tabulate (0x2020, fn _ => 0wx90),
         codebytes]

      val RELOCTABLE_START = 0x2020
      val NUM_RELOCATIONS = 0x2020

      val padding_size = 1048576 - Word8Vector.length header
      val padding = Word8Vector.tabulate
        (padding_size,
         fn i =>
         if i >= RELOCTABLE_START - Word8Vector.length header andalso
            i < RELOCTABLE_START - Word8Vector.length header + NUM_RELOCATIONS * 2
         then
           (* relocation table. *)
           let
             val off = i - RELOCTABLE_START - Word8Vector.length header
           in
             if i mod 4 = 0 then 0wx19
             else 0wx20
           end
         else
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
    end handle Tactics.Tactics s => print ("Tactics: " ^ s ^ "\n")

end
