
signature LISTUTIL =
sig

    exception ListUtil

    (* Association lists *)
    structure Alist : 
	sig
	    val find : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> 'b option
	    val get : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> ('a * 'b) option
	    val haskey : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> bool

	    val removeall : ('a * 'a -> bool) -> 
		            ('a * 'b) list -> 'a -> ('a * 'b) list
	    val removefirst : ('a * 'a -> bool) -> 
		              ('a * 'b) list -> 'a -> ('a * 'b) list

	    val bycompare : ('a * 'a -> order) -> ('a * 'a) -> bool

	    val update : ('a * 'a -> bool) -> ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list

	    val swap : ('a * 'b) list -> ('b * 'a) list
	end

    structure Sorted :
	sig
	    (* insert in ascending order *)
	    val insert : ('a * 'a -> order) -> 'a list -> 'a -> 'a list

	    (* reverse a sorting function *)
	    val reverse : ('a * 'a -> order) -> 'a * 'a -> order

	    (* ... *)
	end

    val combiner : ('a * 'a -> 'a) -> 'a list -> 'a
    val combinel : ('a * 'a -> 'a) -> 'a list -> 'a

    val list : 'a -> 'a list

    (* returns elements satisfying predicate until first failure *)
    val aslongas : ('a -> bool) -> 'a list -> 'a list

    (* return the first success and the entire list behind it.
       nil if nothing satisfies it. *)
    val after : ('a -> bool) -> 'a list -> 'a list

    (* return all that satisfy until the first failure, 
       and separately the rest of the list *)
    val partitionaslongas : ('a -> bool) -> 'a list -> 'a list * 'a list

    (* position f l
       returns smallest SOME n such that f (List.nth (l, n)) = true,
       (that's 0 based), or NONE if no such element exists. *)
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

    val existsecond : ('a -> bool) -> ('b * 'a) list -> bool
    val existfirst  : ('a -> bool) -> ('a * 'b) list -> bool

    (* like Vector.mapi, but whole list *)
    val mapi : ('a * int -> 'b) -> 'a list -> 'b list

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

    (* like hd o sort, but linear time *)
    val min : ('a * 'a -> order) -> 'a list -> 'a

    (* (byfirst String.compare) will sort a (string * 'a) list. *)
    val byfirst  : ('a * 'b -> 'c) -> ('a * 'd) * ('b * 'e) -> 'c
    val bysecond : ('a * 'b -> 'c) -> ('d * 'a) * ('e * 'b) -> 'c

    (* like length o filter, but no allocation *)
    val count : ('a -> bool) -> 'a list -> int

end