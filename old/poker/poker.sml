
structure Poker (* :> POKER *) =
struct

  exception Poker of string
  exception Unimplemented
  
  structure C = Cards

(*
  (* general type of a hand *)
  datatype typ =
    HIGHCARD of int
  | PAIR of int
  | TWOPAIR of int
  | THREE of int
  (* highest card in straight *)
  | STRAIGHT of int
  (* ditto *)
  | FLUSH of int
  | FULLHOUSE of int
  | FOUR of int
  | STRAIGHTFLUSH of int
*)

  datatype typ =
    HIGHCARD 
  | PAIR 
  | TWOPAIR 
  | THREE 
  | STRAIGHT 
  | FLUSH 
  | FULLHOUSE 
  | FOUR 
  | STRAIGHTFLUSH 


  fun printhands hands =
    app (fn (t, h) => print (StringUtil.delimit ", " 
                             (map C.tostring h) ^ "\n")) hands

  (* the 'is' functions assume sorted order *)

  fun isstraight ((_, n)::rest) =
    let
      fun iss _ nil = true
        | iss m ((_, x)::t) =
        (m - 1) = x andalso iss x t
    in
      length rest = 4
      andalso
      case n of
        (* ace can be in two different straights *)
        14 => iss n rest orelse iss 6 rest
      | _ => iss n rest
    end
    | isstraight nil = false

  fun isflush l =
    ListUtil.alladjacent (fn ((s1, _), (s2, _)) => s1 = s2) l
    andalso length l = 5

  fun allsame l =
    ListUtil.alladjacent (fn ((_, n1), (_, n2)) => n1 = n2) l

  fun isfour (l as [_,_,_,_]) = allsame l
    | isfour (a :: (l as [b,c,d,e])) =
    allsame l orelse allsame [a, b, c, d]
    | isfour _ = false

  (* return the length of, and card used, in the longest run *)
  fun max_run l =
    let
      fun mr last nso (bestc, bestn) nil = (bestc, bestn)
        | mr last nso (bestc, bestn) ((_, n) :: rest) =
        if last = n
        then mr n (nso + 1) (if (nso + 1) > bestn
                             then (last, nso + 1)
                             else (bestc, bestn)) rest
        else mr n 1 (bestc, bestn) rest
    in
      mr ~1 0 (~1, 0) l
    end

  fun isthree l = #2 (max_run l) >= 3

  fun ispair l = #2 (max_run l) >= 2

  fun istwopair l =
    let
      val (c, n) = max_run l
    in
      if n >= 2
      then let
             val l' = List.filter (fn (_, x) => x <> c) l
           in
             ispair l'
           end
      else false
    end

  fun typeof hand =
    let
      val hand = 
        ListUtil.sort 
        (ListUtil.bysecond 
         (ListUtil.Sorted.reverse Int.compare)) hand

      val str = isstraight hand
      val flu = isflush hand

    in
      if str andalso flu then STRAIGHTFLUSH
      (* both flush and straight imply it's not a full house
         or four of a kind. *)
      else if flu then FLUSH
           else if str then STRAIGHT
                else if isfour hand then FOUR
                     else let
                            val is3 = isthree hand
                            val is2p = istwopair hand
                          in
                            if is3 andalso is2p
                            then FULLHOUSE
                            else if is3 then THREE
                                 else if is2p then TWOPAIR
                                      else if ispair hand
                                           then PAIR
                                           else HIGHCARD
                          end
    end

  fun typ_cmp (STRAIGHTFLUSH, STRAIGHTFLUSH) = EQUAL
    | typ_cmp (STRAIGHTFLUSH, _) = GREATER
    | typ_cmp (_, STRAIGHTFLUSH) = LESS
    | typ_cmp (FOUR, FOUR) = EQUAL
    | typ_cmp (FOUR, _) = GREATER
    | typ_cmp (_, FOUR) = LESS
    | typ_cmp (FULLHOUSE, FULLHOUSE) = EQUAL
    | typ_cmp (FULLHOUSE, _) = GREATER
    | typ_cmp (_, FULLHOUSE) = LESS
    | typ_cmp (FLUSH, FLUSH) = EQUAL
    | typ_cmp (FLUSH, _) = GREATER
    | typ_cmp (_, FLUSH) = LESS
    | typ_cmp (STRAIGHT, STRAIGHT) = EQUAL
    | typ_cmp (STRAIGHT, _) = GREATER
    | typ_cmp (_, STRAIGHT) = LESS
    | typ_cmp (THREE, THREE) = EQUAL
    | typ_cmp (THREE, _) = GREATER
    | typ_cmp (_, THREE) = LESS
    | typ_cmp (TWOPAIR, TWOPAIR) = EQUAL
    | typ_cmp (TWOPAIR, _) = GREATER
    | typ_cmp (_, TWOPAIR) = LESS
    | typ_cmp (PAIR, PAIR) = EQUAL
    | typ_cmp (PAIR, _) = GREATER
    | typ_cmp (_, PAIR) = LESS
    | typ_cmp (HIGHCARD, HIGHCARD) = EQUAL

  fun compare _ = raise Unimplemented

  (* we assume hand has no pairs, etc. *)
  fun gen_highcard deck hand =
      let
        val hand = ListUtil.sort 
                       (ListUtil.bysecond 
                        (ListUtil.Sorted.reverse Int.compare)) hand

        fun ghc _ deck nil = nil
          | ghc m deck ((hc as (hss, hrr)) :: handrest) =
          let
          in
            (* return all cards available in deck that beat it. 
               a card that ties should be followed by a card
               that beats our second best card. *)
            Array.foldli (fn (i, state, l) =>
                          let 
                            val card as (suit, num) = C.card i
                            val avail = 
                              (state = C.Unknown 
                               orelse state = C.Enemy
                               orelse state = C.Shared)
                              andalso (num <= m)
                              
                          in
                            if not avail then l
                            else
                              case Int.compare (num, hrr) of
                                GREATER => [card]::l
                              | LESS => l
                              | EQUAL => 
                                  let
                                    (* need to claim this card *)
                                    val _ = Array.update(deck, i, C.Used)
                                    val tails = ghc (num - 1) deck handrest
                                  in
                                    Array.update(deck, i, state);
                                    map (fn ha => card::ha) tails @ l
                                  end
                          end) nil (deck, 0, NONE)
          end
      in
        List.mapPartial 
        (fn x =>
         case typeof x of
           HIGHCARD => SOME (HIGHCARD, x)
         | _ => NONE)
         (ghc 14 deck hand)
      end

  fun remainrank deck n =
    List.filter 
    (fn c => 
     case Array.sub(deck, C.idx c) of
       C.Shared => true
     | C.Enemy => true
     | C.Unknown => true
     | _ => false)
    [(C.Diamonds, n),
     (C.Hearts, n),
     (C.Clubs, n),
     (C.Spades, n)]

  fun using deck l f =
    let 
      val i = C.idx l
      val old = Array.sub(deck, i)
    in
      Array.update(deck, i, C.Used);
      f () before
      Array.update(deck, i, old)
    end

  fun usingl deck nil f = f ()
    | usingl deck (h::t) f =
    using deck h (fn () =>
                  usingl deck t f)

  fun gen_pair deck hand =
    let 
      (* generate all remaining pairs
         with num x *)
      fun pairsat x =
        case remainrank deck x of
          [a, b, c, d] => [[a, b], [a, c], [a, d],
                           [b, c], [b, d], [c, d]]
        | [a, b, c] => [[a, b], [a, c], [b, c]]
        | [a, b] => [[a, b]]
        | _ => nil

      fun pairsafter 14 = nil
        | pairsafter x =
        pairsat (x + 1) @ pairsafter (x + 1)

      val (ca, nu) = max_run hand
    in
      if nu > 2 then raise Poker "gen_pair on hand better than pair"
      else 
        if nu = 2 then
           let
             val _ = print "kicker mode\n"
             val ourkickers =
               List.filter (fn (_, x) => x <> ca) hand

             (* should just be one at most *)
             val tiepairs = pairsat ca : Cards.card list list

             val _ = print (Int.toString (length tiepairs) ^ " tiepairs\n")
             val kickers : Cards.card list list =
               List.concat 
               (map (fn used =>
                     usingl deck used 
                     (fn () =>
                      let 
                        val kicks = gen_highcard deck ourkickers
                        val _ = printhands kicks
                      in
                        map (fn (_, rest) => used @ rest) kicks
                      end))
                    tiepairs)
           in
             (* anything that ties, followed by a lexicographically
                better hand... *)
             List.mapPartial (fn h =>
                              case typeof h of
                                PAIR => SOME (PAIR, h)
                              | _ => NONE) kickers @
             (* plus anything that just beats it *)
             map (fn x => (PAIR, x)) (pairsafter ca)
             
           end
        else
          (* beating high card; just generate all pairs that remain *)
          map (fn x => (PAIR, x)) (pairsafter 1)

    end

  fun gen_twopair _ _ = nil
  fun gen_three _ _ = nil
  fun gen_straight _ _ = nil
  fun gen_flush _ _ = nil
  fun gen_fullhouse _ _ = nil
  fun gen_four _ _ = nil
  fun gen_straightflush _ _ = nil

  fun rank deck hand =
    let
      val gens =
        [(HIGHCARD, gen_highcard),
         (PAIR, gen_pair),
         (TWOPAIR, gen_twopair),
         (THREE, gen_three),
         (STRAIGHT, gen_straight),
         (FLUSH, gen_flush),
         (FULLHOUSE, gen_fullhouse),
         (FOUR, gen_four),
         (STRAIGHTFLUSH, gen_straightflush)]

      fun gens_after t =
        let
          val toh = typeof hand

          fun ga nil = nil
            | ga ((tt, f) :: rest) =
            case typ_cmp (toh, tt) of 
              GREATER => ga rest
            | _ => f :: ga rest
        in
          ga gens
        end
    in
      (* use only the generator functions for hand types that are
         better than (or the same as) this one *)      
      List.concat (map (fn f => f deck hand) (gens_after hand))
    end

  (* generate a deck for a solo hand *)
  fun solodeck hand =
    Array.tabulate 
    (52,
     fn i =>
     let val c = C.card i
     in
       if List.exists (fn z => z = c) hand
       then C.Ours
       else C.Unknown
     end)

end
