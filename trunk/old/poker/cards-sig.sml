
signature CARDS =
sig

  datatype suit = Hearts | Diamonds | Clubs | Spades

  (* int: 2..13 *)
  type card = suit * int

  (* status of a card *)
  datatype status =
    Shared 
  | Enemy
  | Ours
  | Unknown
  | Used

  (* knowledge about a deck.
     the deck is indexed by card *)
  type deck = status Array.array
  (* 0..52 *)
  val idx : card -> int
  val card : int -> card

  val tostring : card -> string

end