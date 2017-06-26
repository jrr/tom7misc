
SMLofNJ.Internals.GC.messages false;

structure Test =
struct
  open Util

  fun test_old s =
    let
      val s = explode s
      val (key1, s) = makevect 6 s
      val (key2, s) = makevect 6 s
      val (key3, s) = makevect 6 s
      val (sector, s) = makevect 2048 s

		val bls = bls o (Vector.foldr op:: nil)
(*
		val _ = print (bls key1 ^ "\n")
		val _ = print (bls key2 ^ "\n")
		val _ = print (bls key3 ^ "\n")
*)
      val key = ExCSS.titlekey2(key3, ExCSS.titlekey1(key2, key1));
		val _ = print (bls key ^ "\n")
    in
		  bvs (ExCSS.descramble (sector,key))
    end

  fun test_simp s =
		let
			 val s = explode s
			 val (key1, s) = makelist 6 s
			 val (key2, s) = makelist 6 s
			 val (key3, s) = makelist 6 s
			 val (sector, s) = makelist 2048 s
			 val key1 = map (W8S.const nat8 o Word.fromInt o Word8.toInt) key1
			 val key2 = map (W8S.const nat8 o Word.fromInt o Word8.toInt) key2
			 val key3 = map (W8S.const nat8 o Word.fromInt o Word8.toInt) key3
			 val sector = map (W8S.const nat8 o Word.fromInt o Word8.toInt) sector

			 val bls =
				  fn l => bls (map (W8.fromInt o W8S.toIntX) l)
(*
			 val _ = print (bls key1 ^ "\n")
			 val _ = print (bls key2 ^ "\n")
			 val _ = print (bls key3 ^ "\n")
*)
			 val key = ExCSS_S.titlekey2 key3 (ExCSS_S.titlekey1 key2 key1);
				  
			 val _ = print (bls key ^ "\n")
		in
			 bls (ExCSS_S.descramble sector key)
		end

  fun test () = 
		let
			 val s = TestData.testdata
			 val _ = print "running old...\n"
			 val a = test_old s
			 val _ = print "running simplified...\n"
			 val b = test_simp s
			 val _ = print "testing equality...\n"
		in
			 print (if a = b then "OK!!\n" else "NO!!!\n")
		end

  fun test_print s =
			 print (test_old s)


end