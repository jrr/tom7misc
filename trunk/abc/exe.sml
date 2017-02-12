structure EXE : EXE =
struct

  exception EXE of string

  infixr 9 `
  fun a ` b = a b

  type word8 = Word8.word
  type word8vector = Word8Vector.vector
  infix @@
  val vec : word8 list -> Word8Vector.vector = Word8Vector.fromList
  fun (v1 : word8vector) @@ (v2 : word8vector) =
    Word8Vector.concat [v1, v2]

  fun w16v (w16 : Word16.word) = vec [Word8.fromInt `
                                      Word16.toInt `
                                      Word16.>>(w16, 0w8),
                                      Word8.fromInt `
                                      Word16.toInt `
                                      Word16.andb(w16, Word16.fromInt 255)]

  fun write_exe { init_ip, init_sp, cs, ds } filename =
    let
      (* Aside from the keyword "signature" being replaced with "magic",
         these are the same names as the DOSBox source code. Every entry
         is 16-bit. *)
      (* EXE Signature MZ or ZM *)
      val magic = vec [0wx5A, 0wx4D]
      (* Image size mod 512. We have to give an invalid value here, since
         we can't write 01 or 00 for the high bit. *)
      val extrabytes = vec [0wx7e, 0wx7e]
      (* Pages in file.
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
         What does this mean? SS becomes 0x705B, which is mostly zeroes. *)
      val initSS = vec [0wx6e, 0wx6e]
      (* Machine stack grows downward, so we want this to be a large number.
         We actually place the temporaries stack right after this, growing
         upward (towards 0xFFFF); so the machine stack and temporaries stack
         each get about half the address space. *)
      (* vec [0wx7e, 0wx7e] *)
      val initSP = w16v init_sp
      (* Checksum; usually ignored. 'AB' *)
      val checksum = vec [0wx41, 0wx42]
      (* vec [0wx20, 0wx20] *)
      val initIP = w16v init_ip
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

      val () = if Word8Vector.length cs = 65536 then ()
               else raise (EXE "Code segment must be exactly 65536 bytes.")
      val () = if Word8Vector.length ds = 65536 then ()
               else raise (EXE "Data segment must be exaclty 65536 bytes.")

      val RELOCTABLE_START = 0x2020
      val NUM_RELOCATIONS = 0x2020

      (* Compute empirically for the given header values above. *)
      val CODESEG_AT = 0x09e9c4

      (* Computed empirically for the given header values above.
         I *believe* that this is predictable, but it's not really documented.
         See dos_execute.cpp in dosbox/src for some notes. *)
      (* XXX: This works(ish?) but starting at DS:0x78c I see "`f" overwriting
         the file's data every 16 bytes. It goes on until DS:0xb8c, then comes
         back at 0xf8c, then away again at 138c, etc. for the rest of the segment.
         This can't be the relocations, can it? *)
      val DATASEG_AT = 0x07e7c8 - 0x0104

      (* If code and data segments are overlapping, we're screwed. Sanity check. *)
      val () = if CODESEG_AT >= DATASEG_AT andalso CODESEG_AT < DATASEG_AT + 65536
               then raise EXE "Ut oh: code segment starts in data segment!"
               else ()

      val () = if DATASEG_AT >= CODESEG_AT andalso DATASEG_AT < CODESEG_AT + 65536
               then raise EXE "Ut oh: data segment starts in code segment!"
               else ()


      val padding_size = 0x100000 - Word8Vector.length header
      val padding = Word8Vector.tabulate
        (padding_size,
         fn i =>
         if i >= RELOCTABLE_START - Word8Vector.length header andalso
            i < RELOCTABLE_START - Word8Vector.length header + NUM_RELOCATIONS * 4
         then
           (* relocation table. *)
           let
             val off = i - RELOCTABLE_START - Word8Vector.length header
           in
             (* XXXX FIXME! This is not printable, and I didn't carefully verify that
                this is outside the program/data segments. We need to find a
                printable address that's safe to modify. *)
             if i mod 4 = 0 then 0wx19
             else 0wx20
           end
         else
         if i >= 0x09e9c4 andalso i < 0x09e9c4 + 65536
         then
           (* Code segment *)
           Word8Vector.sub (cs, i - 0x09e9c4)
         else
         if i >= DATASEG_AT andalso i < DATASEG_AT + 65536
         then
           (* Data segment *)
           Word8Vector.sub (ds, i - DATASEG_AT)
         else
         case i mod 8 of
           (* Load EAX, unique 32 bit number *)
           0 => Word8.fromInt ` ord #"|"
         | 1 => Word8.fromInt ((i - 1) mod 256)
         | 2 => Word8.fromInt (((i - 2) div 256) mod 256)
         | 3 => Word8.fromInt (((i - 3) div (256 * 256)) mod 256)
         | 4 => Word8.fromInt (((i - 4) div (256 * 256 * 256)) mod 256)
         | _ => Word8.fromInt ` ord #"<"
             )

      val bytes = Word8Vector.concat [header, padding]
    in
      StringUtil.writefilev8 filename bytes
    end
end
