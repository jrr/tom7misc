
structure ListUtil :> LISTUTIL =
struct

    exception ListUtil

    (* Association lists *)
    structure Alist =
    struct
	exception NotFound

	fun find eq nil key = NONE
	  | find eq ((a,b)::t) key =
	    if eq (a, key) then SOME b
	    else find eq t key
		
	fun removefirst eq nil key = raise NotFound
	  | removefirst eq ((a,b)::t) key =
	    if eq (a, key) then t
	    else (a,b) :: removefirst eq t key
		
	fun removeall eq nil key = nil
	  | removeall eq ((a,b)::t) key =
	    if eq (a, key) then removeall eq t key
	    else (a,b) :: removeall eq t key
		
	fun bycompare f x = EQUAL = f x

    end

    fun combinel f nil = raise ListUtil
      | combinel f (h::t) = foldl f h t

    fun combiner f [x] = x
      | combiner f (h::t) = f (h, combiner f t)
      | combiner f nil = raise ListUtil

    fun list x = [x]

    fun position' _ _ nil = NONE
      | position' f n (h::t) = if f h then SOME n else position' f (n + 1) t
    fun position f l = position' f 0 l

    fun all2 f nil nil = true
      | all2 f (a::ta) (b::tb) = f (a, b) andalso all2 f ta tb
      | all2 f _ _ = false

    fun mapsecond f nil = nil 
      | mapsecond f ((a,b)::t) = (a,f b) :: mapsecond f t

    fun mapfirst  f nil = nil
      | mapfirst  f ((a,b)::t) = (f a, b) :: mapfirst f t

    (* should be optimized to do less consing. (split mainly) *)
    fun sort cmp l =
	let
	    fun split l =
		let fun s a1 a2 nil = (a1, a2)
		      | s a1 a2 (h::t) = s a2 (h::a1) t
		in s nil nil l
		end

	    fun merge a nil = a
	      | merge nil b = b
	      | merge (aa as (a::ta)) (bb as (b::tb)) =
		case cmp (a, b) of
		    EQUAL => (a :: b :: merge ta tb)
		  | LESS => (a :: merge ta bb)
		  | GREATER => (b :: merge aa tb)

	    fun ms nil = nil
	      | ms [s] = [s]
	      | ms [a,b] = merge [a] [b]
	      | ms ll = 
		let val (a,b) = split ll
		in merge (ms a) (ms b)
		end
	in ms l
	end

    fun byfirst f ((a,b),(aa,bb)) = f (a, aa)
    fun bysecond f ((a,b),(aa,bb)) = f (b, bb)

    fun allfirst f ((a,b)::t) = f a andalso allfirst f t
      | allfirst f nil = true

    fun allsecond f ((a,b)::t) = f b andalso allsecond f t
      | allsecond f nil = true

    fun alladjacent f nil = true
      | alladjacent f [_] = true
      | alladjacent f (a::(l as (b::_))) = f (a, b) andalso alladjacent f l

    (* assumes f reflexive *)
    fun allpairs f l =
	let 
	    fun apa _ nil = true
	      | apa a (b::t) = f (a, b) andalso f (b, a) andalso apa a t

	    fun ap nil = true
	      | ap [_] = true
	      | ap (h::t) = apa h t andalso ap t
	in
	    ap l
	end

    (* if f is symmetric *)
    fun allpairssym f l = 
	let 
	    fun apa _ nil = true
	      | apa a (b::t) = f (a, b) andalso apa a t

	    fun ap nil = true
	      | ap [_] = true
	      | ap (h::t) = apa h t andalso ap t
	in
	    ap l
	end


end