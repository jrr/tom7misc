(* Imperative version of ORD_MAP.

   The functions are patterned after ORD_MAP, and just modify
   the map in place instead of returning it. TODO: Some functions
   like intersection aren't implemented because the interface
   needs to be through through.

   XXX Maybe this was not smart. So far it's only used in ABC.
   New idea:
    - Have a single functor like "Containers" taking ORD_KEY once.
    - It contains a SplayMap and SplaySet
    - It contains ImperativeMap and ImperativeSet, or utilities
      that just take SplayMap ref (etc.). Note that the imperative
      versions can probably be written more efficiently, for what
      it's worth.
    - It contains utilities that work between these types

   Added by Tom 7; not part of sml/nj library. *)
signature IMPERATIVE_MAP =
sig

  structure Key: ORD_KEY

  (* maps from Key.ord_key to 'a *)
  type 'a map

  (* The empty map *)
  val empty : unit -> 'a map
  val isempty : 'a map -> bool

  (* Return the element with the smallest key, if the map has any values. *)
  val head : 'a map -> 'a option
  val headi : 'a map -> (Key.ord_key * 'a) option

  (* Insert an item. Existing item with same key is overwritten. *)
  val insert : 'a map * Key.ord_key * 'a -> unit

  (* Update the item in place. Funtion is called on NONE if there
     is no item with the given key. *)
  val update : 'a map * Key.ord_key * ('a option -> 'a) -> unit

  (* Look for an item, return NONE if the item doesn't exist *)
  val find: 'a map * Key.ord_key -> 'a option
  val lookup: 'a map * Key.ord_key -> 'a

  (* Remove an item, returning value removed.
     Raises LibBase.NotFound if not found. *)
  val remove: 'a map * Key.ord_key -> 'a
  (* Remove the item; no effect if it doesn't exist. *)
  val erase : 'a map * Key.ord_key -> unit

  (* Return the number of items in the map *)
  val numItems: 'a map -> int

  (* Return an ordered list of the items (and their keys) in the map. *)
  val listItems : 'a map -> 'a list
  val listItemsi: 'a map -> (Key.ord_key * 'a) list

  (* given an ordering on the map's range, return an ordering
     on the map. *)
  val collate: ('a * 'a -> order) -> ('a map * 'a map) -> order

  (* TODO: Figure out what to do with this
  (* return a map whose domain is the union of the domains of the two input
     maps, using the supplied function to define the map on elements that
     are in both domains. *)
  val unionWith : ('a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
  val unionWithi: (Key.ord_key * 'a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    *)

  (* TODO: Figure out what to do with this
  (* return a map whose domain is the intersection of the domains of the
     two input maps, using the supplied function to define the range. *)
  val intersectWith : ('a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
  val intersectWithi: (Key.ord_key * 'a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
    *)

  (* Apply a function to the entries of the map in map order. *)
  val app : ('a -> unit) -> 'a map -> unit
  val appi: ((Key.ord_key * 'a) -> unit) -> 'a map -> unit

    (* TODO: Figure out what to do with this
  (* Create a new map by applying a map function to the
     name/value pairs in the map. *)
  val map : ('a -> 'b) -> 'a map -> 'b map
  val mapi: (Key.ord_key * 'a -> 'b) -> 'a map -> 'b map
    *)

  (* Apply a folding function to the entries of the map
     in increasing map order. *)
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldli: (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b

  (* Apply a folding function to the entries of the map
     in decreasing map order. *)
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val foldri: (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b

  (* Filter out those elements of the map that do not satisfy the
     predicate.  The filtering is done in increasing map order. *)
  val filter : ('a -> bool) -> 'a map -> unit
  val filteri: (Key.ord_key * 'a -> bool) -> 'a map -> unit

    (* TODO: Figure out what to do with this
  (* map a partial function over the elements of a map in increasing
     map order. *)
  val mapPartial : ('a -> 'b option) -> 'a map -> 'b map
  val mapPartiali: (Key.ord_key * 'a -> 'b option) -> 'a map -> 'b map
    *)
end
