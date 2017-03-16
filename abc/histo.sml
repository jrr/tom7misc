structure Histo =
struct
  infixr 9 `
  fun a ` b = a b

  exception Histo of string

  fun go filename =
    let
      val contents = StringUtil.readfile filename
      val arr = Array.array (256, 0)

      fun onec c =
        let val idx = ord c
        in Array.update (arr, idx, Array.sub (arr, idx) + 1)
        end

      fun must_be_0 i =
        if Array.sub (arr, i) > 0
        then raise Histo ("NO! Not printable: " ^
                          Int.toString i ^ " x " ^
                          Int.toString (Array.sub (arr, i)))
        else ()
      val total = ref 0
    in
      CharVector.app onec contents;
      Util.for 0 (0x20 - 1) must_be_0;
      Util.for 0x7f 0xff must_be_0;
      Util.for 0x20 0x7e
      (fn i =>
       let
         val count = Array.sub (arr, i)
       in
         total := !total + count;
         print ("        " ^ implode [chr i] ^ "   0x" ^
                Word8.toString (Word8.fromInt i) ^
                "    " ^ Int.toString count ^ "\n")
       end);
      print ("        total       " ^ Int.toString (!total) ^ "\n")

    end

end

val () = Params.main1 "histo.exe input_file" Histo.go
       handle e as Histo.Histo s =>
         let in
           TextIO.output(TextIO.stdErr, s ^ "\n");
           raise e
         end
