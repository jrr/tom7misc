
(* hands with standard poker rules *)
signature POKER =
sig

  (* must have five cards *)
  val compare : Cards.card list * Cards.card list -> order

  (* given all our information, return all of the possible (for
     opponents) hands that could beat this one.

     we will count as 'unavailable' any card marked as Ours. All of
     the cards in the supplied hand should be Ours.

     the hand must have five cards. The hands in the return list may
     have fewer than five cards if it represents a four-of-a-kind,
     three-of-a-kind, two-pair, pair, or high card. In that case the
     remaining cards may be any cards that do not pair with the cards
     supplied, or with one another. *)

  val rank : Cards.deck -> Cards.card list -> (Cards.card list) list

end