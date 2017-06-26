
structure Cards :> CARDS =
struct

  exception Impossible

  exception Cards of string

  datatype suit = Hearts | Diamonds | Clubs | Spades

  (* int: 2..14 *)
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

  fun idx (s, i) =
    (case s of
       Hearts => 0
     | Diamonds => 1
     | Clubs => 2
     | Spades => 3) + ((i-2) * 4)

  fun card i =
    ((case i mod 4 of
        0 => Hearts
      | 1 => Diamonds
      | 2 => Clubs
      | 3 => Spades
      | _ => raise Impossible),
        (i div 4) + 2)
        
  fun tostring (suit, n) =
    (if n <= 10 then Int.toString n
     else 
       case n of
         11 => "J"
       | 12 => "Q"
       | 13 => "K"
       | 14 => "A"
       | _ => raise Cards "bad card to string") ^ "-" ^
    (case suit of
       Hearts => "H"
     | Diamonds => "D"
     | Clubs => "C"
     | Spades => "S")
          


end