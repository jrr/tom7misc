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

  fun repeatingstring str i =
    let val sz = size str
    in Word8.fromInt ` ord ` String.sub (str, i mod sz)
    end

  fun write_exe { init_ip, init_sp, cs, ds } filename =
    let

      val () = if Word8Vector.length cs = 65536 then ()
               else raise (EXE "Code segment must be exactly 65536 bytes.")
      val () = if Word8Vector.length ds = 65536 then ()
               else raise (EXE "Data segment must be exactly 65536 bytes.")

      (* Aside from the keyword "signature" being replaced with "magic",
         these are the same names as the DOSBox source code. Every entry
         is 16-bit. *)
      (* EXE Signature MZ or ZM *)
      val magic = vec [0wx5A, 0wx4D]
      (* Image size mod 512. We have to give an invalid value here, since
         we can't write 01 or 00 for the high bit. DOSBox ignores this
         value. *)
      val extrabytes = vec [0wx7e, 0wx7e]
      (* Pages in file.

         Number of 512-byte pages. Since the maximum size is 1MB, we
         also have to give an invalid value here, but DOSBox ANDs the
         value with 07ff. We have a wide range of values available
         here; the most-significant byte can be 20 to 27, covering
         that whole range because the 2 gets ANDed off, and the LSB
         can be anything printable (20 to 7e). The largest value we
         can use here is 0x2f7e (not 7e7e), which yields 1918 pages.

         We can use so little of the file for anything meaningful; it's
         basically just the code and data segments, each at 64k, and
         this huge useless header before that where we store the paper
         and some untouchable stuff like the relocation table.

         The last byte we actually need to store in the file is the
         end of the code segment, which starts at (paragraph) 0x2020
         and runs 65536 bytes. ((0x2020 * 16 + 65536) div 512) = 385,
         so this gives us a good starting point for the minimum possible
         size. But of course we need a header. 0x217e is just shy
         of 385 anyway, and the next biggest printable value is 0x2220,
         which gives us 544 pages. At this size, even with the
         smallest possible header (0x2020 * 16 bytes), the 64k of code
         doesn't fit in the image. 0x227e is merely 2kb shy, ugh! So
         0x2320 seems to be the smallest workable value here, for 800
         pages. (Maybe could get smaller if header used CR/LF.) *)
      val pages = w16v (Word16.fromInt 0x2320)
      (* Number of relocation entries. We want this to be the minimum
         value we can, since it's totally wasted space. *)
      val relocations = vec [0wx20, 0wx20]
      (* Paragraphs in header. A paragraph is 16 bytes.
         The image size, which we need to keep small to fit in memory,
         is (head.pages & 07ff) * 512 - head.headersize * 16.

         For the setting of "pages" above, the minimum value of 0x2020
         works. It's much larger than the actual header, but it helps
         make the memory requirements of the program smaller by
         reducing the amount of the file that needs to be loaded into
         memory (only the part after the header is loaded). This is a
         potential issue because we also have to declare a minimum
         memory requirement of 0x2020 below. (We also get some space
         to put the paper :)) *)
      val headersize = w16v (Word16.fromInt 0x2020)
      (* Min/max number of 16-byte paragraphs required above
         the end of the program. This is useful for programs that

         Useful for programs that can change segment registers to
         allocate some space. But this space will be basically useless
         to us. Also, we need the total memory requirement of our program
         to be small, since DOS only has about 40,000 paragraphs to
         give us. Note that this value gets shifted up by 4 bits, so it
         would be nice to be able to shift off some high bits (e.g. supply
         0x7020, yielding 0x0200.), but at least in DOSBox this calculation
         is done at 32-bit width. So the smallest we can manage is 0x2020.

         Note that the memory check includes this value and the image
         size, which is the file size (head.pages) minus the header
         size. See the sanity check below. *)
      val minmemory = vec [0wx20, 0wx20]
      val maxmemory = minmemory
      (* Stack segment displacement, in 16-byte paragraphs.

         I believe this is specified the same way as the code segment
         (i.e., within the _image_), so it has to be after it, and
         should probably be within the program image + allocated
         memory above. We could initialize the stack segment with some
         data, but this grows the size of the executable and is
         difficult to manage (DOS will corrupt the stack segment at
         arbitrary times with interrupts, including during
         initialization, and we don't have access to the instructions
         to turn interupts off!)

         Put this immediately after CS. *)
      val initSS = w16v (Word16.fromInt 0x3020)  (* vec [0wx6e, 0wx6e] *)
      (* Machine stack grows downward (towards 0x0000), so we want this to
         be a large number. We actually place the temporaries stack
         right after this, growing upward (towards 0xFFFF); so the
         machine stack and temporaries stack each get about half the
         address space. *)

      (* TODO: Could maybe get some text into the title here, because initSP
         only needs to be some large number less than 0x7fff, checksum can
         be anything, and we can probably control initIP as well. Would be
         a delicate matter. *)
      val initSP = w16v init_sp
      (* Checksum; usually ignored. 'AB' *)
      val checksum = Word8Vector.tabulate (2, repeatingstring "AB")
      val initIP = w16v init_ip
      (* Displacement of code segment (within _image_). *)
      val initCS = vec [0wx20, 0wx20]
      (* Offset in the _file_ that begins the relocations. *)
      val reloctable = vec [0wx20, 0wx20]
      (* Tells the OS what overlay number this is. Should be 0
         for the main executable, but it seems to work if it's not *)
      val overlay = Word8Vector.tabulate (2, repeatingstring "C!")

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


      (* Following dos_execute *)
      fun long2para s =
        if s > 0xFFFF0 then 0xffff
        else if s mod 16 <> 0 then (s div 16 + 1)
             else s div 16

      (* This is what dosbox reports as the maximum available memory; not actually sure if
         it is reliably this high? *)
      val AVAILABLE_MEMORY = 40482
      val minsize = long2para(IMAGE_BYTES + (Word16.toInt (vw16 minmemory) * 16) + 256)
      val () = print ("Computed minsize: " ^ Int.toString minsize ^ "\n")
      val () = if minsize <= AVAILABLE_MEMORY then ()
               else raise (EXE ("Requested memory won't fit: " ^ Int.toString minsize ^
                                " > available " ^ Int.toString AVAILABLE_MEMORY))

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
         repeatingstring "hdr" i)

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
      val () = if CODESEG_AT >= DATASEG_AT andalso
                  CODESEG_AT < DATASEG_AT + 65536
               then raise EXE "Ut oh: code segment starts in data segment!"
               else ()

      val () = if DATASEG_AT >= CODESEG_AT andalso
                  DATASEG_AT < CODESEG_AT + 65536
               then raise EXE "Ut oh: data segment starts in code segment!"
               else ()

      val () = if CODESEG_AT + 65536 < IMAGE_BYTES then ()
               else raise EXE ("Code segment doesn't fit inside image! " ^
                               Int.toString ((CODESEG_AT + 65536) - IMAGE_BYTES) ^
                               " bytes short.")

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
           (* Mark position in source code for forensics. *)
           case i mod 16 of
             0 => ch #"*"
           | 1 => ch #" "
           | 2 => ch #"<"
           | 3 => ch #"-"
           | 4 => ch #"-"
           | 5 => ch #" "
           | 14 => ch #" "
           | 15 => ch #" "
           | m => ch (String.sub (s, m - 6))
         end)

      val bytes = Word8Vector.concat [header, image]
      val num_bytes = Word8Vector.length bytes

      val num_nonprintable = ref 0
      fun onebyte b = if Tactics.printable b then ()
                      else num_nonprintable := !num_nonprintable + 1
      val () = Word8Vector.app onebyte bytes
    in
      if !num_nonprintable = 0 then ()
      else print ("\nWARNING: " ^ Int.toString (!num_nonprintable) ^
                  " non-printable bytes remain!\n\n");
      if num_bytes = FILE_BYTES then ()
      else raise (EXE ("File is not the expected size (got " ^
                       Int.toString num_bytes ^
                       " but expected " ^ Int.toString FILE_BYTES));
      StringUtil.writefilev8 filename bytes;
      print ("Wrote " ^ Int.toString num_bytes ^ " bytes to " ^ filename ^ "\n")
    end
end
