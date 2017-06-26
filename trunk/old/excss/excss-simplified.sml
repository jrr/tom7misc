
(* dumbing down ExCSS in preparation for rewriting
   in some really abstract language *)

(* Gone: Vector
         Word32
         Word8
         Pairs
         Remove integer constants
         Remove word constants

*)

(* To do: Combine common behavior in titlekey/decode?
*)


structure ExCSS_S :>
  sig
    type bytevector = W8S.word list

    (* keys are 6 bytes *)
    type key = bytevector

    (* an example player key *)
    val playerkey : key

    (* takes encrypted disk key and player key, 
       returns decrypted disk key *)
    val titlekey1 : key -> key -> key
      
    (* takes encrypted title key and disk key, 
       returns decrypted title key *)
    val titlekey2 : key -> key -> key
      
    (* takes an encrypted title key and encrytped disk key,
       returns a decrypted title key *)
    val detitlekey : key -> key -> key
      
    (* takes a 2048-byte sector and decryption key, 
       returns decrypted sector *)
    val descramble : bytevector -> key -> bytevector
      
  end =
struct

(* FIXME FIXME FIXME *)
    open Util
    
    open Nat

	 open WConst

	 structure WD = SimpleWord

    val nat1 = inc zero
    val nat2 = inc nat1
    val nat3 = inc nat2
    val nat4 = inc nat3
    val nat5 = inc nat4
    val nat8  = inc(inc(inc(inc(inc(inc(inc(inc(zero))))))))
    val nat7  = Nat.dec nat8
    val nat16 = inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(zero))))))))))))))))
    val nat24 = inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(zero))))))))))))))))))))))))
    val nat32 = Nat.app nat8 inc nat24 

    val nat64 = Nat.app nat32 inc nat32
    val nat128 = Nat.app nat64 inc nat64

	 fun w8to32 n = h0 (h0 (h0 (h0 (h0 (h0 n)))))

	 val w8zero = h0(h0 hh)
	 val w32zero = w8to32 w8zero

	 val w32ff = w8to32 (hF(hF hh))

	 fun w32to8 n = WD.trunc nat8 n

	 val w32seven = w8to32 (h0 (h7 hh))

  exception Oops

  exception Unimplemented and Impossible

  (* some basis functions that I use, rewritten here *)

  fun listmap f nil = nil
    | listmap f (h::t) = (f h)::(listmap f t)

  fun listappend nil l = l
    | listappend (h::t) l = h::(listappend t l)

  fun listnthn (h::t) zero = h
    | listnthn (h::t) (inc b) = listnthn t b
    | listnthn _ _ = raise Impossible

  fun listtake l zero = nil
    | listtake (h::t) (inc b) = h :: (listtake t b)
    | listtake _ _ = raise Impossible

  fun listdrop l zero = l
    | listdrop (h::t) (inc b) = (listdrop t b)
    | listdrop _ _ = raise Impossible
    
  (* functional vector modify *)
  fun modify (nil)  _ _ = nil
    | modify (h::t) zero a = a::t
    | modify (h::t) (inc i) a = h :: (modify t i a)

  type bytevector = WD.word list
    
  type key = WD.word list

  val playerkey : bytevector = 
      [h5(h1 hh), h6(h7 hh), h6(h7 hh), hC(h5 hh), hE(h0 hh), h0(h0 hh)]
    
local 

  val table1 = 
    listmap w8to32
[
h3(h3 hh),h7(h3 hh),h3(hB hh),h2(h6 hh),
h6(h3 hh),h2(h3 hh),h6(hB hh),h7(h6 hh),
h3(hE hh),h7(hE hh),h3(h6 hh),h2(hB hh),
h6(hE hh),h2(hE hh),h6(h6 hh),h7(hB hh),
hD(h3 hh),h9(h3 hh),hD(hB hh),h0(h6 hh),
h4(h3 hh),h0(h3 hh),h4(hB hh),h9(h6 hh),
hD(hE hh),h9(hE hh),hD(h6 hh),h0(hB hh),
h4(hE hh),h0(hE hh),h4(h6 hh),h9(hB hh),
h5(h7 hh),h1(h7 hh),h5(hF hh),h8(h2 hh),
hC(h7 hh),h8(h7 hh),hC(hF hh),h1(h2 hh),
h5(hA hh),h1(hA hh),h5(h2 hh),h8(hF hh),
hC(hA hh),h8(hA hh),hC(h2 hh),h1(hF hh),
hD(h9 hh),h9(h9 hh),hD(h1 hh),h0(h0 hh),
h4(h9 hh),h0(h9 hh),h4(h1 hh),h9(h0 hh),
hD(h8 hh),h9(h8 hh),hD(h0 hh),h0(h1 hh),
h4(h8 hh),h0(h8 hh),h4(h0 hh),h9(h1 hh),
h3(hD hh),h7(hD hh),h3(h5 hh),h2(h4 hh),
h6(hD hh),h2(hD hh),h6(h5 hh),h7(h4 hh),
h3(hC hh),h7(hC hh),h3(h4 hh),h2(h5 hh),
h6(hC hh),h2(hC hh),h6(h4 hh),h7(h5 hh),
hD(hD hh),h9(hD hh),hD(h5 hh),h0(h4 hh),
h4(hD hh),h0(hD hh),h4(h5 hh),h9(h4 hh),
hD(hC hh),h9(hC hh),hD(h4 hh),h0(h5 hh),
h4(hC hh),h0(hC hh),h4(h4 hh),h9(h5 hh),
h5(h9 hh),h1(h9 hh),h5(h1 hh),h8(h0 hh),
hC(h9 hh),h8(h9 hh),hC(h1 hh),h1(h0 hh),
h5(h8 hh),h1(h8 hh),h5(h0 hh),h8(h1 hh),
hC(h8 hh),h8(h8 hh),hC(h0 hh),h1(h1 hh),
hD(h7 hh),h9(h7 hh),hD(hF hh),h0(h2 hh),
h4(h7 hh),h0(h7 hh),h4(hF hh),h9(h2 hh),
hD(hA hh),h9(hA hh),hD(h2 hh),h0(hF hh),
h4(hA hh),h0(hA hh),h4(h2 hh),h9(hF hh),
h5(h3 hh),h1(h3 hh),h5(hB hh),h8(h6 hh),
hC(h3 hh),h8(h3 hh),hC(hB hh),h1(h6 hh),
h5(hE hh),h1(hE hh),h5(h6 hh),h8(hB hh),
hC(hE hh),h8(hE hh),hC(h6 hh),h1(hB hh),
hB(h3 hh),hF(h3 hh),hB(hB hh),hA(h6 hh),
hE(h3 hh),hA(h3 hh),hE(hB hh),hF(h6 hh),
hB(hE hh),hF(hE hh),hB(h6 hh),hA(hB hh),
hE(hE hh),hA(hE hh),hE(h6 hh),hF(hB hh),
h3(h7 hh),h7(h7 hh),h3(hF hh),h2(h2 hh),
h6(h7 hh),h2(h7 hh),h6(hF hh),h7(h2 hh),
h3(hA hh),h7(hA hh),h3(h2 hh),h2(hF hh),
h6(hA hh),h2(hA hh),h6(h2 hh),h7(hF hh),
hB(h9 hh),hF(h9 hh),hB(h1 hh),hA(h0 hh),
hE(h9 hh),hA(h9 hh),hE(h1 hh),hF(h0 hh),
hB(h8 hh),hF(h8 hh),hB(h0 hh),hA(h1 hh),
hE(h8 hh),hA(h8 hh),hE(h0 hh),hF(h1 hh),
h5(hD hh),h1(hD hh),h5(h5 hh),h8(h4 hh),
hC(hD hh),h8(hD hh),hC(h5 hh),h1(h4 hh),
h5(hC hh),h1(hC hh),h5(h4 hh),h8(h5 hh),
hC(hC hh),h8(hC hh),hC(h4 hh),h1(h5 hh),
hB(hD hh),hF(hD hh),hB(h5 hh),hA(h4 hh),
hE(hD hh),hA(hD hh),hE(h5 hh),hF(h4 hh),
hB(hC hh),hF(hC hh),hB(h4 hh),hA(h5 hh),
hE(hC hh),hA(hC hh),hE(h4 hh),hF(h5 hh),
h3(h9 hh),h7(h9 hh),h3(h1 hh),h2(h0 hh),
h6(h9 hh),h2(h9 hh),h6(h1 hh),h7(h0 hh),
h3(h8 hh),h7(h8 hh),h3(h0 hh),h2(h1 hh),
h6(h8 hh),h2(h8 hh),h6(h0 hh),h7(h1 hh),
hB(h7 hh),hF(h7 hh),hB(hF hh),hA(h2 hh),
hE(h7 hh),hA(h7 hh),hE(hF hh),hF(h2 hh),
hB(hA hh),hF(hA hh),hB(h2 hh),hA(hF hh),
hE(hA hh),hA(hA hh),hE(h2 hh),hF(hF hh)]

  val table2 = 
listmap w8to32
[
h0(h0 hh),h0(h1 hh),h0(h2 hh),h0(h3 hh),
h0(h4 hh),h0(h5 hh),h0(h6 hh),h0(h7 hh),
h0(h9 hh),h0(h8 hh),h0(hB hh),h0(hA hh),
h0(hD hh),h0(hC hh),h0(hF hh),h0(hE hh),
h1(h2 hh),h1(h3 hh),h1(h0 hh),h1(h1 hh),
h1(h6 hh),h1(h7 hh),h1(h4 hh),h1(h5 hh),
h1(hB hh),h1(hA hh),h1(h9 hh),h1(h8 hh),
h1(hF hh),h1(hE hh),h1(hD hh),h1(hC hh),
h2(h4 hh),h2(h5 hh),h2(h6 hh),h2(h7 hh),
h2(h0 hh),h2(h1 hh),h2(h2 hh),h2(h3 hh),
h2(hD hh),h2(hC hh),h2(hF hh),h2(hE hh),
h2(h9 hh),h2(h8 hh),h2(hB hh),h2(hA hh),
h3(h6 hh),h3(h7 hh),h3(h4 hh),h3(h5 hh),
h3(h2 hh),h3(h3 hh),h3(h0 hh),h3(h1 hh),
h3(hF hh),h3(hE hh),h3(hD hh),h3(hC hh),
h3(hB hh),h3(hA hh),h3(h9 hh),h3(h8 hh),
h4(h9 hh),h4(h8 hh),h4(hB hh),h4(hA hh),
h4(hD hh),h4(hC hh),h4(hF hh),h4(hE hh),
h4(h0 hh),h4(h1 hh),h4(h2 hh),h4(h3 hh),
h4(h4 hh),h4(h5 hh),h4(h6 hh),h4(h7 hh),
h5(hB hh),h5(hA hh),h5(h9 hh),h5(h8 hh),
h5(hF hh),h5(hE hh),h5(hD hh),h5(hC hh),
h5(h2 hh),h5(h3 hh),h5(h0 hh),h5(h1 hh),
h5(h6 hh),h5(h7 hh),h5(h4 hh),h5(h5 hh),
h6(hD hh),h6(hC hh),h6(hF hh),h6(hE hh),
h6(h9 hh),h6(h8 hh),h6(hB hh),h6(hA hh),
h6(h4 hh),h6(h5 hh),h6(h6 hh),h6(h7 hh),
h6(h0 hh),h6(h1 hh),h6(h2 hh),h6(h3 hh),
h7(hF hh),h7(hE hh),h7(hD hh),h7(hC hh),
h7(hB hh),h7(hA hh),h7(h9 hh),h7(h8 hh),
h7(h6 hh),h7(h7 hh),h7(h4 hh),h7(h5 hh),
h7(h2 hh),h7(h3 hh),h7(h0 hh),h7(h1 hh),
h9(h2 hh),h9(h3 hh),h9(h0 hh),h9(h1 hh),
h9(h6 hh),h9(h7 hh),h9(h4 hh),h9(h5 hh),
h9(hB hh),h9(hA hh),h9(h9 hh),h9(h8 hh),
h9(hF hh),h9(hE hh),h9(hD hh),h9(hC hh),
h8(h0 hh),h8(h1 hh),h8(h2 hh),h8(h3 hh),
h8(h4 hh),h8(h5 hh),h8(h6 hh),h8(h7 hh),
h8(h9 hh),h8(h8 hh),h8(hB hh),h8(hA hh),
h8(hD hh),h8(hC hh),h8(hF hh),h8(hE hh),
hB(h6 hh),hB(h7 hh),hB(h4 hh),hB(h5 hh),
hB(h2 hh),hB(h3 hh),hB(h0 hh),hB(h1 hh),
hB(hF hh),hB(hE hh),hB(hD hh),hB(hC hh),
hB(hB hh),hB(hA hh),hB(h9 hh),hB(h8 hh),
hA(h4 hh),hA(h5 hh),hA(h6 hh),hA(h7 hh),
hA(h0 hh),hA(h1 hh),hA(h2 hh),hA(h3 hh),
hA(hD hh),hA(hC hh),hA(hF hh),hA(hE hh),
hA(h9 hh),hA(h8 hh),hA(hB hh),hA(hA hh),
hD(hB hh),hD(hA hh),hD(h9 hh),hD(h8 hh),
hD(hF hh),hD(hE hh),hD(hD hh),hD(hC hh),
hD(h2 hh),hD(h3 hh),hD(h0 hh),hD(h1 hh),
hD(h6 hh),hD(h7 hh),hD(h4 hh),hD(h5 hh),
hC(h9 hh),hC(h8 hh),hC(hB hh),hC(hA hh),
hC(hD hh),hC(hC hh),hC(hF hh),hC(hE hh),
hC(h0 hh),hC(h1 hh),hC(h2 hh),hC(h3 hh),
hC(h4 hh),hC(h5 hh),hC(h6 hh),hC(h7 hh),
hF(hF hh),hF(hE hh),hF(hD hh),hF(hC hh),
hF(hB hh),hF(hA hh),hF(h9 hh),hF(h8 hh),
hF(h6 hh),hF(h7 hh),hF(h4 hh),hF(h5 hh),
hF(h2 hh),hF(h3 hh),hF(h0 hh),hF(h1 hh),
hE(hD hh),hE(hC hh),hE(hF hh),hE(hE hh),
hE(h9 hh),hE(h8 hh),hE(hB hh),hE(hA hh),
hE(h4 hh),hE(h5 hh),hE(h6 hh),hE(h7 hh),
hE(h0 hh),hE(h1 hh),hE(h2 hh),hE(h3 hh)]

in

  fun tab0n zero = nat5
    | tab0n (inc zero) = zero
    | tab0n (inc (inc zero)) = nat1
    | tab0n (inc (inc (inc zero))) = nat2
    | tab0n (inc (inc (inc (inc zero)))) = nat3
    | tab0n (inc (inc (inc (inc (inc zero))))) = nat4
    | tab0n (inc (inc (inc (inc (inc t))))) = tab0n t


  val tab3l = listmap w8to32
      [ h0(h0 hh), h2(h4 hh), h4(h9 hh), h6(hD hh), 
        h9(h2 hh), hB(h6 hh), hD(hB hh), hF(hF hh) ]

  fun tab3 n = (listnthn tab3l n) 

  (* substitutions above *)
  fun tab1 n = (listnthn table1 n)
  fun tab2 n = (listnthn table2 n)

  (* reverse bits *)
  fun tab4 n =
		let val t = w32to8 n
		in w8to32 (WD.revbits t)
		end

  (* reverse bits, then not *)
  fun tab5 n = 
      WD.xorb (tab4 n) (w8to32 (hF(hF hh)))

  val tab1w = tab1 o WD.toNat
  val tab2w = tab2 o WD.toNat
  val tab3w = tab3 o WD.toNat o (WD.andb w32seven)

end

fun report s l =
    let
        val res = foldl (fn (a,b) => b ^ " " ^ Util.w32ss a) "" l
    in
        print ("% " ^ s ^ ": " ^ res ^ "\n")
    end

(* this functional takes a table as its
   first parameter. It is used to generate
   titlekey1 and titlekey2 *)
fun decode table (enc    : key)
                 (pk     : key) : key =
  let

    fun im x = w8to32 (listnthn pk x)
    val t1 = WD.+ (im zero) (h0(h0(h0(h0(h0(h1(h0(h0 hh))))))))
    val t2 = im nat1
    val t3 = WD.orb (WD.<< (im nat5) nat24)
                     (WD.orb (WD.<< (im nat4) nat16)
                              (WD.orb (WD.<< (im nat3) nat8) (im nat2)))

    val t4 = WD.andb (im nat2) (w8to32 (h0 (h7 hh)))

(*     val _ = report "decode_1" [ t1, t2, t3, t4 ]  *)

    val t3 = WD.- (WD.<< (WD.+ t3 (w8to32 (h0 (h4 hh)))) (inc zero)) t4

    fun l1 _ _ _ _ k zero = rev k
      | l1 t1 t2 t3 t5 k (inc n) = 
      let
          val t4 = WD.xorb (tab3w t1) (tab2w t2) 

          val t2 = WD.>> t1 (inc zero)
          val t1 = WD.xorb (WD.<< (WD.andb t1 (w8to32 (h0 (h1 hh)))) nat8) t4
          val t4 = tab4 t4
              
          val t6 = WD.xorb (WD.>> t3 (inc (inc (inc zero)))) t3
          val t6 = WD.>> t6 (inc zero)
          val t6 = WD.xorb t6 t3
          val t6 = WD.>> t6 (nat8)
          val t6 = WD.xorb t6 t3
          val t6 = WD.>> t6 (inc(inc(inc(inc(inc zero)))))
          val t6 = WD.andb t6 w32ff
              
          val t3 = WD.orb (WD.<< t3 nat8) t6
          val t6 = table t6
          val t5 = WD.+ t5 (WD.+ t6 t4)
          val res = WD.andb t5 w32ff
          val t5 = WD.>> t5 nat8
      in
          l1 t1 t2 t3 t5 (res :: k) n
      end

    val k = l1 t1 t2 t3 w32zero nil nat5

(*     val _ = report "decode_k" k  *)

    fun l2 n key =
        let val xx = listnthn k (tab0n (inc n))
            val yy = listnthn key (tab0n (inc n))
            val zz = listnthn key (tab0n n)
            val aa = w32to8 (WD.xorb xx (WD.xorb (tab1w yy) (w8to32 zz)))
            val k = modify key (tab0n (inc n)) aa
        in
            case n of
                inc nm => l2 nm k
              | zero => k
        end
  in
    l2 (inc nat8) enc
  end

val titlekey1 = decode tab4
val titlekey2 = decode tab5

fun detitlekey (tkey : key) (dkey : key) : key =
  titlekey2 tkey (titlekey1 playerkey dkey)

fun descramble (sector  : WD.word list)
               (dec_key : key) =
  let

      val nat80 = Nat.app nat16 inc nat64
      val nat84 = Nat.app nat4 inc nat80
      val nat85 = inc nat84
      val nat86 = inc nat85
      val nat87 = inc nat86
      val nat88 = inc nat87
      val nat89 = inc nat88

    fun sec x = w8to32 (listnthn sector x)
    fun key x = w8to32 (listnthn dec_key x)
        
    val t1 = WD.orb (h0(h0(h0(h0(h0(h1(h0(h0 hh)))))))) (WD.xorb (key zero) (sec nat84))

    val t2 = WD.xorb (key nat1) (sec nat85)

    val t3 = WD.xorb
        (WD.orb (WD.<< (key nat5) nat24)
         (WD.orb (WD.<< (key nat4) nat16)
          (WD.orb (WD.<< (key nat3) nat8) (key nat2))))

        (WD.orb (WD.<< (sec nat89) nat24)
         (WD.orb (WD.<< (sec nat88) nat16)
          (WD.orb (WD.<< (sec nat87) nat8) (sec nat86))))

    val t4 = WD.andb t3 (w8to32 (h0 (h7 hh)))

    val t3 = WD.- (WD.<< (WD.+ t3 (w8to32 (h0 (h4 hh)))) (inc zero)) t4
      
    val _ = report "descramble_1" [t1, t2, t3, t4]

    val r = ref 0
    fun ++ x = (x := (!x + 1); !x)

    fun folder (b::tail) t1 t2 t3 t5 l =
      let
          val _ = (++ r > 10 andalso
                   ! r < 20) andalso (report "foldme" [t1, t2, t3, t5]; true)

        val t4 = WD.xorb (tab2w t2) (tab3w t1)
        val t2 = WD.>> t1 (inc zero)
        val t1 = WD.xorb (WD.<< (WD.andb t1 (w8to32 (h0 (h1 hh)))) nat8) t4
        val t4 = tab5 t4

        val t6 = WD.xorb (WD.>> t3 (inc(inc(inc zero)))) t3 
        val t6 = WD.>> t6 (inc zero) 
        val t6 = WD.xorb t6 t3
        val t6 = WD.>> t6 nat8
        val t6 = WD.xorb t6 t3
        val t6 = WD.>> t6 (inc(inc(inc(inc(inc zero)))))
        val t6 = WD.andb t6 w32ff

        val t3 = WD.orb (WD.<< t3 nat8) t6
          
        val t6 = tab4 t6

        val t5 = WD.+ t4 (WD.+ t5 t6)


        val oo = (WD.xorb (w32to8 (tab1w b)) (w32to8 t5))
            
        val _ = (++ r > 10 andalso
                 !  r < 20) andalso (report "folduu" [t1, t2, t3, t5, t6,
                                                      WD.const nat32 (Word.fromInt (WD.toIntX oo))];
                                     true)

      in
          folder tail t1 t2 t3 (WD.>> t5 nat8) (oo :: l)
      end
      | folder nil a b c d e = e
  in

      listappend
         (listtake sector nat128)
         (rev (folder (listdrop sector nat128) t1 t2 t3 w32zero nil))
      
  end

end
