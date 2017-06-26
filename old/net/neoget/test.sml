
structure Test =
struct

(*  val (h,r) = Http.get "Explodo/1.0" "http://www.neopets.com/browseshop.phtml?owner=frooder" "np_uniq=2001-09-30; neologin=frooder%2BdhXJHUvfIMLjM; neoremember=frooder; np_randseed=764690-63739952564905" 80 "neopets.com" "/buyitem.phtml?owner=frooder&obj_info_id=12023&xhs=3oo7s75s&old_price=65"
*)

(*  val (h,r) = Http.get "Explodo/1.0" "nocookie" "http://neopets.com/" 80 "neopets.com" "/" *)

    val (h,r) = Http.post "Explodo/1.0" "nocookie" "http://spacebar.org/" 80 "spacebar.org" "/fcgi-bin/aphid" "p=froodspeak"

  val _ = 
    let in
      print h;
      print "\n";
      print r;
      print "\n"
    end

end
