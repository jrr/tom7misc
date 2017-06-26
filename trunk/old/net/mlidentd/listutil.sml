
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

	fun get eq nil key = NONE
	  | get eq ((h as (a,b))::t) key =
	    if eq (a, key) then SOME h
	    else get eq t key

	fun haskey f l k = Option.isSome (find f l k)

	fun update eq l a b = map (fn (aa,bb) => (a, if eq(aa,a) then b else bb)) l
		
	fun removefirst eq nil key = raise NotFound
	  | removefirst eq ((a,b)::t) key =
	    if eq (a, key) then t
	    else (a,b) :: removefirst eq t key
		
	fun removeall eq nil key = nil
	  | removeall eq ((a,b)::t) key =
	    if eq (a, key) then removeall eq t key
	    else (a,b) :: removeall eq t key
		
	fun bycompare f x = EQUAL = f x

	fun s (a, b) = (b, a)
	fun swap l = map s l

    end

    (* sorted lists *)

    structure Sorted =
    struct
	fun insert cmp nil a = [a]
	  | insert cmp (h::t) a =
	    case cmp (h, a) of
		LESS => h :: insert cmp t a
	      | _ => a :: h :: t

	fun reverse c a = 
	    case c a of
		LESS => GREATER
	      | GREATER => LESS
	      | EQUAL => EQUAL
    end

    fun combinel f nil = raise ListUtil
      | combinel f (h::t) = foldl f h t

    fun combiner f [x] = x
      | combiner f (h::t) = f (h, combiner f t)
      | combiner f nil = raise ListUtil

    fun list x = [x]

    fun aslongas f nil = nil
      | aslongas f (h::t) = if f h then h :: aslongas f t else nil

    fun after f nil = nil
      | after f (l as (h::t)) = if f h then l else after f t

    fun partitionaslongas f l =
	let
	    fun pala a (u as (h::t)) = if f h then pala (h::a) t
				       else (rev a, u)
	      | pala a nil = (l, nil)
	in
	    pala nil l
	end

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

    fun existfirst f nil = false
      | existfirst f ((a,b)::t) = f a orelse existfirst f t

    fun existsecond f nil = false
      | existsecond f ((a,b)::t) = f b orelse existsecond f t

    fun mapi f l =
	let fun mm n nil = nil
	      | mm n (h::t) = f (h, n) :: mm (n + 1) t
	in
	    mm 0 l
	end

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


    fun min f l =
        let
            fun m nil a = a
              | m (h::t) a =
                case f(h, a) of
                    LESS => m t h
                  | _ => m t a
        in
            m (tl l) (hd l)
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

   fun count f l =
        let
            fun c nil a = a
              | c (h::t) a = c t (if f h then a + 1
				  else a)
        in
            c l 0
        end
end