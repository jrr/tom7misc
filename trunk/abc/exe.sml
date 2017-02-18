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

  (* Little-endian. *)
  fun w16v (w16 : Word16.word) = vec [Word8.fromInt `
                                      Word16.toInt `
                                      Word16.andb(w16, Word16.fromInt 255),
                                      Word8.fromInt `
                                      Word16.toInt `
                                      Word16.>>(w16, 0w8)]
  fun vw16 (v : Word8Vector.vector) : Word16.word =
    if Word8Vector.length v <> 2
    then raise EXE "vw16 requires a length-2 vector"
    else Word16.fromInt (Word8.toInt (Word8Vector.sub(v, 1)) * 256 +
                         Word8.toInt (Word8Vector.sub(v, 0)))

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
         with 07ff. This gives 0x7e7e gives 0x67E * 512 = 850944 bytes.
         (We could make this 77e if we wanted...)

         We can use so little of the file for anything meaningful; it's
         basically just the code and data segments, each at 64k, and
         this huge useless header before that where we store the paper
         and some untouchable stuff like the relocation table.

         We can write most values here; the most-significant byte can
         be 20 to 27, covering that whole range because the 2 get ANDed
         off, and the LSB can be anything printable (20 to 7e).

         Here I choose a value that results in 1397 pages, which is
         just after the natural end of the code segment. This results
         in a file size of 715264 bytes.
         *)
      val pages = w16v (Word16.fromInt 0x2575)
      (* Number of relocation entries. We want this to be the minimum
         value we can, since it's totally wasted space. *)
      val relocations = vec [0wx20, 0wx20]
      (* Paragraphs in header. A paragraph is 16 bytes.
         The image size, which we need to keep small to fit in memory,
         is (head.pages & 07ff) * 512 - head.headersize * 16.

         So we actually report a large header size in order to account
         for the largeness of the file. (A large header also allows us
         to fit the relocation table as well as our paper :))

         The minimum printable value is 0x2020 * 16 = 131584, which
         is bigger than we'd like. (CR/LF here?) *)
      (* Actually we want the header size to be apparently large so that
         the image size fits in RAM with extra minmemory.

         TODO: This can probably come down, though shrinking the overall
         size of the binary? *)
      val headersize = vec [0wx7e, 0wx7e]
      (* Min/max number of 16-byte paragraphs required above
         the end of the program. I think this is like BSS? *)
      (* we want this to be small, since DOS only has about 40557 paragraphs to
         give us. We'd like to use 0x20,0x70 which little-endian is
         is 0b0111000000100000, hoping that when it's left-shifted 4, all
         the 1 bits overflow. (Unfortunately it seems to be extended to
         32 bits, so that doesn't work. *)
      (* What gets computed here is
         long2para(imagesize + minmemory<<4 + 256), where long2para
         is roughly >>4. The result has to be <maxfree, which is 40482.
         imagesize is (pages * 512) - headersize, both of which we control.
         This is why we give a large header size. The result with
         minmemory=0x2020 is 29042, which is less than 40482 as needed. *)
      val minmemory = vec [0wx20, 0wx20]
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
      (* Displacement of code segment (within _image_). *)
      val initCS = vec [0wx20, 0wx20]
      (* Offset in the _file_ that begins the relocations. *)
      val reloctable = vec [0wx20, 0wx20]
      (* Tells the OS what overlay number this is. Should be 0
         for the main executable, but it seems to work if it's not *)
      val overlay = vec [0wx20, 0wx20]

      val header_struct =
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
      val HEADER_STRUCT_BYTES = Word8Vector.length header_struct

      (* masked pages value, times size of page *)
      val FILE_BYTES = Word16.toInt (Word16.andb(vw16 pages, Word16.fromInt 0x7ff)) * 512
      val HEADER_BYTES = Word16.toInt (vw16 headersize) * 16
      val IMAGE_BYTES = FILE_BYTES - HEADER_BYTES

      val () = print ("File size: " ^ Int.toString FILE_BYTES ^ " (" ^
                      Int.toString HEADER_BYTES ^ " header + " ^
                      Int.toString IMAGE_BYTES ^ " image)\n")

      val () = if Word8Vector.length cs = 65536 then ()
               else raise (EXE "Code segment must be exactly 65536 bytes.")
      val () = if Word8Vector.length ds = 65536 then ()
               else raise (EXE "Data segment must be exaclty 65536 bytes.")

      val () = if HEADER_BYTES >= Word8Vector.length header_struct then ()
               else raise (EXE "Alleged header size doesn't even include the header struct?")

      val RELOCTABLE_START = Word16.toInt ` vw16 reloctable
      val NUM_RELOCATIONS = Word16.toInt ` vw16 relocations

      (* Note: I think it might be allowed that the relocation table is not
         actually in the header. Maybe it would be more efficient to put it between
         the data and code segments? *)
      val () = if HEADER_BYTES >= RELOCTABLE_START + NUM_RELOCATIONS * 4 then ()
               else raise (EXE "Relocation table doesn't fit in alleged header size.")

      val () = if RELOCTABLE_START mod 4 = 0 then ()
               else raise (EXE "This code assumes that the relocation table is 32-bit aligned.")

      (* This is the chunk of the file that DOS interprets as the header (HEADER_BYTES in size),
         starting with the header struct and containing the relocation table. *)
      val header = Word8Vector.tabulate
        (HEADER_BYTES,
         fn i =>
         if i < Word8Vector.length header_struct
         then Word8Vector.sub(header_struct, i)
         else
         if i >= RELOCTABLE_START andalso
            i < RELOCTABLE_START + NUM_RELOCATIONS * 4
         then
         let
           val off = (i - RELOCTABLE_START) div 4
         in
           (* XXXX FIXME! This is not printable, and I didn't carefully
              verify that this is outside the program/data segments. We
              need to find a printable address that's safe to modify. *)
           if i mod 4 = 0 then 0wx19
           else 0wx20
         end
         else
           (case i mod 3 of
              0 => Word8.fromInt ` ord #"h"
            | 1 => Word8.fromInt ` ord #"d"
            | _ => Word8.fromInt ` ord #"r"))

      (* Computed empirically for the given header values above.
         I *believe* that this is predictable, but it's not really documented.
         See dos_execute.cpp in dosbox/src for some notes. *)

      (* We "start" the data segment before the image location, and don't even
         write the first 256 bytes. This is because those bytes get overwritten
         by the PSP. *)
      val DATASEG_AT = ~256
      val CODESEG_AT = 16 * Word16.toInt ` vw16 initCS

      (* If code and data segments are overlapping, we're screwed.
         Sanity check. *)
      val () = if CODESEG_AT >= DATASEG_AT
               andalso CODESEG_AT < DATASEG_AT + 65536
               then raise EXE "Ut oh: code segment starts in data segment!"
               else ()

      val () = if DATASEG_AT >= CODESEG_AT
               andalso DATASEG_AT < CODESEG_AT + 65536
               then raise EXE "Ut oh: data segment starts in code segment!"
               else ()

      val image = Word8Vector.tabulate
        (IMAGE_BYTES,
         fn i =>
         if i >= CODESEG_AT andalso i < CODESEG_AT + 65536
         then
           (* Code segment *)
           Word8Vector.sub (cs, i - CODESEG_AT)
         else
         if i >= DATASEG_AT andalso i < DATASEG_AT + 65536
         then
           (* Data segment *)
           Word8Vector.sub (ds, i - DATASEG_AT)
         else
         let
           val pos = i div 16 * 16
           val s = StringCvt.padLeft #"0" 8 (Word32.toString `
                                             Word32.fromInt pos)
           fun ch c = Word8.fromInt ` ord c
         in
           case i mod 16 of
           (* Load EAX, unique 32 bit number *)
           0 => ch #"*"
         | 1 => ch #" "
         | 2 => ch #"<"
         | 3 => ch #"-"
         | 4 => ch #"-"
         | 5 => ch #" "
         | 14 => ch #"\r"
         | 15 => ch #"\n"
         | m => ch (String.sub (s, m - 6))
         end)

      val bytes = Word8Vector.concat [header, image]
      val num_bytes = Word8Vector.length bytes
    in
      if num_bytes = FILE_BYTES then ()
      else raise (EXE ("File is not the expected size (got " ^ Int.toString num_bytes ^
                       " but expected " ^ Int.toString FILE_BYTES));
      StringUtil.writefilev8 filename bytes;
      print ("Wrote " ^ Int.toString num_bytes ^ " to " ^ filename ^ "\n")
    end
end
