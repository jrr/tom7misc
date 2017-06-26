
signature LISTUTIL =
sig

    exception ListUtil

    (* Association lists *)
    structure Alist : 
	sig
	    val find : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> 'b option

	    val removeall : ('a * 'a -> bool) -> 
		            ('a * 'b) list -> 'a -> ('a * 'b) list
	    val removefirst : ('a * 'a -> bool) -> 
		              ('a * 'b) list -> 'a -> ('a * 'b) list

	    val bycompare : ('a * 'a -> order) -> ('a * 'a) -> bool
	end

    val combiner : ('a * 'a -> 'a) -> 'a list -> 'a
    val combinel : ('a * 'a -> 'a) -> 'a list -> 'a

    val list : 'a -> 'a list

    (* position f l
       returns smallest SOME n such that f (List.nth (l,n)) = true,
       or NONE if no such element exists. *)
    val position : ('a -> bool) -> 'a list -> int option

    (* all2 f a b
       true iff a and b are the same length and f(a,b) for corresponding
       pairs. NOT the same as ListPair.all, which does not require that
       a and b be the same length. *)
    val all2 : ('a * 'b -> bool) -> 'a list -> 'b list -> bool

    val mapsecond : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
    val mapfirst  : ('a -> 'b) -> ('a * 'c) list -> ('b * 'c) list

    val allfirst  : ('a -> bool) -> ('a * 'b) list -> bool
    val allsecond : ('a -> bool) -> ('b * 'a) list -> bool

    (* alladjacent f l
       true if f(lm, lm+1) for all m < (length l - 1)
       in the list *)
    val alladjacent  : ('a * 'a -> bool) -> 'a list -> bool

    (* allpairs f l
       true if f(la, lb) for all a, b in the list when a <> b.
       *)
    val allpairs     : ('a * 'a -> bool) -> 'a list -> bool

    (* same as allpairs, but assumes f is symmetric. *)
    val allpairssym : ('a * 'a -> bool) -> 'a list -> bool

    val sort : ('a * 'a -> order) -> 'a list -> 'a list

    (* (byfirst String.compare) will sort a (string * 'a) list. *)
    val byfirst  : ('a * 'b -> 'c) -> ('a * 'd) * ('b * 'e) -> 'c
    val bysecond : ('a * 'b -> 'c) -> ('d * 'a) * ('e * 'b) -> 'c

end