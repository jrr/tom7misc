
(* removethis *)
val _ = Compiler.Control.Print.printDepth := 100;

structure Sum :>
sig
	 datatype ('a, 'b) sum = A of 'a | B of 'b
end =
struct
	 datatype ('a, 'b) sum = A of 'a | B of 'b
end

structure Nat :>
sig
	 datatype nat = zero | inc of nat
		  
	 val app : nat -> ('a -> 'a) -> 'a -> 'a

	 val add : nat -> nat -> nat

	 val dec : nat -> nat

end =
struct

	 datatype nat = zero | inc of nat

	 fun dec zero = zero 
		| dec (inc b) = b

	 fun add zero b = b
		| add (inc b1) b2 = inc (add b1 b2)

	 fun app zero _ z = z
		| app (inc b) f z = f(app b f z)
end

structure SimpleWord :
sig
	 datatype word = E | X of word | O of word

	 val orb : word -> word -> word
	 val xorb : word -> word -> word
	 val andb : word -> word -> word
	 val toIntX : word -> int
	 val << : word -> Nat.nat -> word
	 val >> : word -> Nat.nat -> word

	 val toNat : word -> Nat.nat

	 val const : Nat.nat -> Word.word -> word
	 val fromInt : Nat.nat -> int -> word
	 val iszero : word -> bool
	 val + : word -> word -> word
	 val - : word -> word -> word

	 val revbits : word -> word
	 val trunc : Nat.nat -> word -> word
end =
struct

	 datatype word = E | X of word | O of word

	 (* representation: O(O(O(...(X(E))...))) <- 1 *)

	 (* these give us a way of writing constants in hex
	    (keeping the right length is up to you! *)

		 
	 exception Unimplemented

	 fun revbits b =
		  let
				fun rapp E r = r
				  | rapp (O x) r = rapp x (O r)
				  | rapp (X x) r = rapp x (X r)
		  in
				rapp b E
		  end

	 fun trunc n b =
		  let
				val r = revbits b
				fun chp Nat.zero _ = E
				  | chp (Nat.inc n) (X r) = X (chp n r)
				  | chp (Nat.inc n) (O r) = O (chp n r)
				  | chp _ _ = raise Unimplemented
		  in
				revbits (chp n r)
		  end

	 fun orb (X(b1)) (X(b2)) = X(orb b1 b2)
		| orb (X(b1)) (O(b2)) = X(orb b1 b2)
		| orb (O(b1)) (X(b2)) = X(orb b1 b2)
		| orb (O(b1)) (O(b2)) = O(orb b1 b2)
		| orb _ _ = E

	 fun xorb (X(b1)) (X(b2)) = O(xorb b1 b2)
		| xorb (X(b1)) (O(b2)) = X(xorb b1 b2)
		| xorb (O(b1)) (X(b2)) = X(xorb b1 b2)
		| xorb (O(b1)) (O(b2)) = O(xorb b1 b2)
		| xorb _ _ = E

	 fun andb (X(b1)) (X(b2)) = X(andb b1 b2)
		| andb (X(b1)) (O(b2)) = O(andb b1 b2)
		| andb (O(b1)) (X(b2)) = O(andb b1 b2)
		| andb (O(b1)) (O(b2)) = O(andb b1 b2)
		| andb _ _ = E

	 fun shh E = O(E)
		| shh (X b) = X(shh b)
		| shh (O b) = O(shh b)

	 fun shl (X b) = shh b
		| shl (O b) = shh b
		| shl E = E (* bogus *)

	 fun << b n = Nat.app n shl b

	 fun shr _ E = E
		| shr last (X b) = last (shr X b)
		| shr last (O b) = last (shr O b)

	 fun >> b n = Nat.app n (shr O) b

	 fun toIntX b =
		  let
				fun go a E = a
				  | go a (X b) = go (a*2 + 1) b
				  | go a (O b) = go (a*2) b
		  in
				go 0 b
		  end

    fun dbl Nat.zero = Nat.zero
      | dbl (Nat.inc t) = Nat.inc (Nat.inc (dbl t))

	 fun toNat b =
		  let
				fun go a E = a
				  | go a (X b) = go (Nat.inc (dbl a)) b
				  | go a (O b) = go (dbl a) b
		  in
				go Nat.zero b
		  end


	 fun iszero E = true
		| iszero (O b) = iszero b
		| iszero (X _) = false

(* this is all that uses the S.size param to the functor.
   in a future version, expose that const takes nat as an argument to
   remove the functor. *)

	 fun mconst n = 
		  let
				fun makepad E x = x
				  | makepad (O b) x = O (makepad b x)
				  | makepad (X b) x = X (makepad b x)

				fun getinside (O b) = b
				  | getinside (X b) = b
				  | getinside E = E (* bogus*)

				fun makelength E zero = (makepad zero)
				  | makelength b zero =
					 makelength (getinside b) (getinside zero)

				val zero = Nat.app n O E
				fun go b d 0w0 = (makelength b zero) b
				  | go b d nn = 
					 let val bit = Word.andb(Word.<<(0w1, Word.fromInt d), nn)
					 in if bit > 0w0 then
						  go (X b) (d + 1) (nn - bit)
						 else
						  go (O b) (d + 1) nn
					 end
					 
				fun me nn = go E 0 nn
		  in
				me
		  end

	 val const = mconst

	 fun fromInt n = (const n) o Word.fromInt

	 (* last, since I use + in this structure... *)

	 fun plus (X a) (O b) = plus (O b) (X a)
		| plus (O a) (O b) =
		  (case plus a b of
				 (s, true) => (X s, false)
			  | (s, false) => (O s, false))
		| plus (X a) (X b) =
		  (case plus a b of
				 (s, true) => (X s, true)
			  | (s, false) => (O s, true))
		| plus (O a) (X b) =
		  (case plus a b of
				 (s, true) => (O s, true)
			  | (s, false) => (X s, false))
		| plus _ _ = (E, false) (* bogus when not the same length *)
		  
	 fun op+ a b = #1 (plus a b)
		  
	 (* two's complement: *)
	 fun inc' E = (E, true)
		| inc' (X b) =
		  (case inc' b of
				 (rest, true) => (O rest, true)
			  | (rest, false) => (X rest, false))
		| inc' (O b) =
  		  (case inc' b of
				 (rest, true) => (X rest, false)
			  | (rest, false) => (O rest, false))

	 fun flip E = E
		| flip (X b) = O (flip b)
		| flip (O b) = X (flip b)

	 fun neg b = #1 (inc' (flip b))

	 fun op- a b = op+ a (neg b)

end

structure WConst :
sig

	 type word = SimpleWord.word

    val hh : word
	 val h0 : word -> word
	 val h1 : word -> word
	 val h2 : word -> word
	 val h3 : word -> word
	 val h4 : word -> word
	 val h5 : word -> word
	 val h6 : word -> word
	 val h7 : word -> word
	 val h8 : word -> word
	 val h9 : word -> word
	 val hA : word -> word
	 val hB : word -> word
	 val hC : word -> word
	 val hD : word -> word
	 val hE : word -> word
	 val hF : word -> word

end = struct

    datatype word = datatype SimpleWord.word
    
	 val hh = E

	 val h0 = O o O o O o O
	 val h1 = O o O o O o X
	 val h2 = O o O o X o O
	 val h3 = O o O o X o X
	 val h4 = O o X o O o O
	 val h5 = O o X o O o X
	 val h6 = O o X o X o O
	 val h7 = O o X o X o X

	 val h8 = X o O o O o O
	 val h9 = X o O o O o X
	 val hA = X o O o X o O
	 val hB = X o O o X o X
	 val hC = X o X o O o O
	 val hD = X o X o O o X
	 val hE = X o X o X o O
	 val hF = X o X o X o X


end

structure W32S = SimpleWord
structure W8S = SimpleWord