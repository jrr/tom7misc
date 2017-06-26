
signature SHOP =
sig
  
  (* takes the shop owner name, object id, price, and buys it. *)
  val buyitem : Neo.session -> string -> int -> int -> unit

  (* take your own id, object's name, returns the lowest price for the
     object, the shop owner's name, the quantity, and your current
     cash *) 

  val getlowestprice : Neo.session -> string -> string -> 
                            (int * string * int * int * bool) option
  
  (* buythresh cookie userid name id price pricemine limit
     keeps buying items that are less than a certain price, until you run out
     of money or buy 'limit' of them.
     pricemine is the threshold when it is "your" area of the marketplace
     (ie, people who are competing with you)
     *)
  val buythresh : string -> string -> string -> int -> 
                     int -> int -> int -> unit

  (* cleartill cookie
     takes all the money out of your shop till.
   *)
  val cleartill : Neo.session -> unit

  (* moveitemtoshop cookie id name limit
     moves n copies of an item to your shop. *)
  val moveitemtoshop : Neo.session -> int -> string -> int -> unit

end