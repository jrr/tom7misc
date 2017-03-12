(* XXX Testing code that should be deleted. *)
structure WriteTest =
struct

  exception WriteTest of string

  infixr 9 `
  fun a ` b = a b

  open X86

  open Acc
  infix // ?? ++ --

  val vec = Word8Vector.fromList

    (*
  fun writecom () =
    let

      (* XXX: This won't work any more, since we assume that we can use
         temporaries in DS, but DS = CS. *)
      val prog =
        Tactics.old_initialize () @
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

  fun eprint (e, s) =
    print ("Exception: " ^ s ^ "\n" ^
           StringUtil.delimit "\n  " (Port.exnhistory e) ^ "\n")

  fun writeexe () =
    let
      val init_sp = Word16.fromInt 0x7e7e
      val init_ip = Word16.fromInt 0x2020

      val acc = Tactics.old_initialize ()
      val acc = Tactics.printstring acc "this is an asciicutable!\n"
      val acc = Tactics.exit acc

      val prog = Acc.insns acc

      val ctx = CTX { default_32 = false }
      val codebytes = Word8Vector.concat (map (EncodeX86.encode ctx) prog)
      val () = print ("# of code bytes: " ^
                      Int.toString (Word8Vector.length codebytes) ^ "\n")

      val codeseg =
        Word8Vector.concat
        [vec [Word8.fromInt ` ord #"["],
         Word8Vector.tabulate (0x2020 - 1,
                               fn i => if i mod 2 = 0
                                       then Word8.fromInt ` ord #"c"
                                       else Word8.fromInt ` ord #"s"),
         codebytes,
         Word8Vector.tabulate (0xFFFF - 0x2020 - Word8Vector.length codebytes,
                               fn i => if i mod 2 = 0
                                       then Word8.fromInt ` ord #"C"
                                       else Word8.fromInt ` ord #"S"),
         vec [Word8.fromInt ` ord #"]"]]

      val dataseg_vec =
        Word8Vector.concat
        [vec [Word8.fromInt ` ord #"["],
        Word8Vector.tabulate (0xFFFE,
                              fn i => if i mod 2 = 0
                                      then Word8.fromInt ` ord #"d"
                                      else Word8.fromInt ` ord #"s"),
         vec [Word8.fromInt ` ord #"]"]]

      val dataseg = Segment.empty ()
      val () = Segment.set_vec dataseg 0 dataseg_vec

    in
      EXE.write_exe { init_ip = init_ip,
                      init_sp = init_sp,
                      cs = codeseg,
                      ds = dataseg } "dos/header.exe"
    end handle e as (Tactics.Tactics s) => eprint (e, "Tactics: " ^ s ^ "\n")
             | e as (Acc.Acc s) => eprint (e, "Acc: " ^ s ^ "\n")
             | e as (EXE.EXE s) => eprint (e, "EXE: " ^ s ^ "\n")
end
