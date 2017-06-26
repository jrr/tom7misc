
structure Util =
struct
  structure W8 = Word8
  structure W32 = Word32

  val inc = Nat.inc
  val zero = Nat.zero
  val nat8  = inc(inc(inc(inc(inc(inc(inc(inc(zero))))))))
  val nat7  = Nat.dec nat8
  val nat16 = inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(zero))))))))))))))))
  val nat24 = inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(zero))))))))))))))))))))))))
		
  fun digit x = CharVector.sub ("0123456789ABCDEF", W8.toInt x)

  (* byte to hex string *)
  fun bhs (b : W8.word) = implode
    [ digit (b div 0w16), digit (b mod 0w16) ]

  fun bhs' (b : W32.word) = bhs (W8.fromInt (W32.toIntX b))

  fun w32hs (b : W32.word) = 
    bhs' (W32.andb(0wxFF, W32.>> (b, 0w24))) ^
    bhs' (W32.andb(0wxFF, W32.>> (b, 0w16))) ^
    bhs' (W32.andb(0wxFF, W32.>> (b, 0w8))) ^
    bhs' (W32.andb(0wxFF, b))

  fun byt x = bhs (W8.fromInt (W32S.toIntX (W32S.andb (W32S.const nat8 0wxff) x)))
  fun w32ss (b : W32S.word) =
		byt (W32S.>> b nat24) ^
		byt (W32S.>> b nat16) ^
		byt (W32S.>> b nat8)  ^
		byt b


  fun bvs vec = Vector.foldl (fn (a, "") => (bhs a)
                               | (a, b)  => b ^ " " ^ (bhs a)) "" vec

  fun bls vec = foldl (fn (a, "") => (bhs a)
                    | (a, b)  => b ^ " " ^ (bhs a)) "" vec


  fun w32vs vec = Vector.foldl (fn (a, "") => (bhs' a)
                                 | (a, b)  => b ^ " " ^ (bhs' a)) "" vec

  fun printk v = print (bvs v)

  fun printwl v = 
    let in
      print "[ ";
      List.app (fn x => (print (w32hs x); print " ")) v;
      print "]\n"
    end

  fun vtolist l = Vector.foldr op:: nil l

  exception Impossible and Oops

  fun lose 0 l = l
    | lose n (h::t) = lose (n - 1) t
    | lose _ _ = raise Oops    

  fun nybble h = 
    if h >= #"0" andalso h <= #"9" then (ord h - ord #"0")
    else if h >= #"A" andalso h <= #"F" then ( (10 + (ord h - ord #"A")))
         else raise Oops

  fun readbytes nil = nil
    | readbytes (h::t) = rb2 (nybble h) t
  and rb2 _ nil = raise Oops
    | rb2 c (h::t) = (W8.fromInt(c * 16 + (nybble h))) :: readbytes t

  fun makelist n s =
		((readbytes (List.take (s, n * 2))), lose (n*2) s)

  fun makevect n s =
		let val (a, b) = makelist n s
		in (Vector.fromList a, b)
		end

end