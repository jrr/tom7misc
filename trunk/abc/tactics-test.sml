structure TacticsTest =
struct

  exception TacticsTest of string

  infixr 9 `
  fun a ` b = a b

  open X86
  infix <- <~

  val PRINT_LOW : Word8.word = 0wx20
  val PRINT_HIGH : Word8.word =  0wx7e

  fun ftos f = Real.fmt (StringCvt.FIX (SOME 2)) f

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
          val (known, ins) = Tactics.load_ax16 (NONE, SOME al) (NONE, SOME vl)
          val ctx = X86.CTX { default_32 = false }
          val bytes = Word8Vector.concat (map (encode ctx) ins)
          val n = Word8Vector.length bytes
        in
          Word8Vector.app (fn c => if c < PRINT_LOW orelse c > PRINT_HIGH
                                   then raise TacticsTest "byte not printable!"
                                   else ());
          (case known of
             (_, SOME r) => if r <> vl
                            then raise TacticsTest "WRONG known value"
                            else ()
           | _ => raise TacticsTest "UNKNOWN known value?");
          print (Word8.toString al ^ " -> " ^ Word8.toString vl ^ ": " ^
                 Int.toString n ^ "\n");
          total := !total + n
        end));
      print ("Total bytes: " ^ Int.toString (!total) ^ " (avg " ^
             ftos (real (!total) / 65536.0) ^ ")\n")
    end

end