
structure Shop :> SHOP =
struct

    val host = "www.neopets.com"
    val port = 80
    val ua = "Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)"
  
    val itos = Int.toString
    fun itosn n s = StringUtil.pad (~ n) (Int.toString s)

    fun stoi s = Option.valOf (Int.fromString s)
    fun sctoi s = stoi (StringUtil.filter
			(StringUtil.isn'tchar #",") s)


    val checktill_freq = 20
    val moveitem_freq = 21

    exception Unimplemented

    fun moveitemtoshop session id name limit =
	let
	    val itemre = REUtil.findall
("<tr><td width=220 align=left bgcolor='#ffffcc'>([-A-Za-z 0-9]+)</td><input type='hidden' name='id_[0-9]+' value='([0-9]+)'>\n" ^
 "<input type='hidden' name='name_([0-9]+)' value='([-A-Za-z 0-9]+)'>\n" ^
 "<td align=center bgcolor='#ffffcc' width=50><input type='radio' value='stock' name='radio_[0-9]+'></td>\n" ^
 "<td align=center bgcolor='#ffffcc' width=50><input type='radio' value='deposit' name='radio_[0-9]+'></td>\n" ^
 "<td align=center bgcolor='#ffffcc' width=50><input type='radio' value='donate' name='radio_[0-9]+'></td>\n" ^
 "<td align=center bgcolor='#ffffcc' width=50><input type='radio' value='discard' name='radio_[0-9]+'></td>\n" ^
 "</tr>")
            val rfr = "http://www.neopets.com/quickstock.phtml"
	    val path = "/quickstock.phtml"
	    val (_, body) = Http.get ua (Neo.cookie session) rfr port host path

	    val all = itemre body
		
	    fun one f = 
		let val q = [("id_" ^ (f 5), (f 3)),
			     ("name_" ^ (f 5), (f 1))]
		in
		    q @ (if (f 1) = name then [("radio_" ^ (f 5), "stock")]
		    else nil)
		end

	    val qs = ("buyitem", "0") :: List.concat (map one all) 
			     
	    val path = "/process_quickstock.phtml"

(*	    val _ = print ((Http.querystring qs) ^ "\n") *)

	    (* Send it if our item appears anywhere ... *)
	    val _ = if List.exists (fn (_, s) => s = "stock") qs
		      then ignore (Http.post ua (Neo.cookie session) rfr port host path
				   (Http.querystring qs))
		    else ()

	in
	    case all of
		nil => print ("no items in stock!\n")
	      | _ => app (fn f => print ((f 1) ^ ", " ^ (f 3) ^ ", " ^ (f 5) ^ "\n")) all;
		    

	    ()
	end

    val tillre = REUtil.find "currently have <b>([0-9,]+) NP</b> in"
    fun cleartill session =
	let
	    val rfr = "http://www.neopets.com/market.phtml"
	    val path = "/market.phtml?type=till"
	    val (_, body) = Http.get ua (Neo.cookie session) rfr port host path
	in
	    case tillre body of
		NONE => print "Couldn't match till amount...!\n"
	      | SOME f =>
		    (case sctoi (f 1) of
			 0 => ()
		       | n => 
			     let 
				 val rfr = "http://" ^ host ^ path
				 val qs = [("type", "withdraw"),
					   ("amount", itos n)]
				 val path = "/process_market.phtml"
			     in
				 print ("----- withdrawing " ^ itos n ^ " from till.\n");
				 (Http.post ua (Neo.cookie session) rfr port host path 
				                (Http.querystring qs));
				 ()
			     end)
	end

    (* takes the shop owner name, object id, price, and buys it. *)
    fun buyitem session owner id price =
      let
        val restr = ("buyitem\\.phtml\\?owner=" ^
                     owner ^ "&obj_info_id=" ^
                     (itos id) ^ "&xhs=([0-9a-zA-Z]+)&old_price=" ^
                     (itos price))
(*        val _ = print ("RE: [" ^ restr ^ "]\n") *)

        val xhsre = REUtil.find restr
        val cookie = Neo.cookie session
        val rfr = "http://www.neopets.com/market.phtml"
        val path = "/browseshop.phtml?owner=" ^ owner

        val body = #2 (Http.get ua cookie rfr port host path)
      in
        case xhsre body of
          NONE => 
            let in
              print ("Problem buying from " ^ owner ^ "...:\n")
            end
        | SOME f => 
            let 
              val _ = print ("XHS is " ^ f 1 ^ "...\n");
              val (_, body) = 
              Http.get ua cookie 
                    ("http://www.neopets.com/browseshop.phtml?owner=" ^ owner)
                    80 host ("/" ^ f 0);
            in
              print body;
              print "\n"
            end
      end



    local 
	val cashre = REUtil.find "/neopoints\\.phtml'>([0-9,]+)</a>"
    in

	(* take object's name, returns the lowest price for the object,
	   the shop owner's name, and your current cash *)
	fun getlowestprice session userid name =
	    let
		val cookie = Neo.cookie session
		val rfr = "http://www.neopets.com/market.phtml?type=wizard"
		val path = "/market.phtml"
		    
		val qs = Http.querystring [("type", "process_wizard"),
                                           ("shopwizard", name),
                                           ("criteria", "exact"),
                                           ("min_price", ""),
                                           ("max_price", "")]
		    
(*                val _ = (print qs ; print "\n") *)

		val (_, body) = Http.post ua cookie rfr port host path qs
		    
		val pricere = REUtil.find ("browseshop\\.phtml\\?owner=[A-Za-z0-9_]+'><b>([A-Za-z0-9_]+)</b></a></td><td align=center bgcolor='#ffffcc'>" ^ name ^ "</td><td align=center bgcolor='#ffffcc'>([0-9]+)</td><td align=center bgcolor='#ffffcc'><b>([0-9,]+) NP</b></td>")

	    in
		case cashre body of
		    SOME g =>
			let 
                          val cash = sctoi (g 1)
                        in
			    case pricere body of
				SOME f => 
				    let 
                                      val price = sctoi (f 5)
                                    in
                                        SOME (price, f 1, stoi (f 3),
                                              cash, REUtil.ismatch userid body)
				    end
			      | NONE => 
                                    let in
                                      print "didn't match price re\n";
                                      print body;
                                      NONE
                                    end
			end
		  | NONE => 
                        let in
                          print "didn't match money re\n";
                          NONE
                        end

	    end
    end

  fun sleep n = Posix.Process.sleep (Time.fromMilliseconds n)


    fun buythresh session userid name id price pricemine limit =
      let
        val userid = "owner=" ^ userid ^ "'><b>" ^ userid ^ "</b>"
  fun bt _ 0 = print "Limit reached."
    | bt nn limit =
      let in
	  if nn mod checktill_freq = 0 then
	      let in
		  cleartill session;
		  sleep 1000;
		  bt (nn+1) limit
	      end
	  else if nn mod moveitem_freq = 0 then
	      let in
		  moveitemtoshop session id name 5; (* XXX? *)
		  sleep 2000;
		  bt (nn+1) limit
	      end
	  else
        (case getlowestprice session userid name of
           NONE => 
             let in
               ("oops, no results...\n");
               sleep 5000;
               bt (nn+1) limit
             end
         | SOME (lowprice, owner, qty, cash, withme) => 
             (print ("lowp: " ^ itosn 5 lowprice ^ " cash: " ^ itosn 6 cash ^
                     " qty: " ^ itosn 3 qty ^ 
                     " owner: " ^ owner ^ (if withme then "(*)" else ""));
              if ((lowprice <= price orelse
                   (withme andalso lowprice <= pricemine))
                  andalso lowprice <= cash) then
               let
                 fun reptbuy 0 =
                   let in
                     sleep 1000;
                     bt (nn+1) limit
                   end
                   | reptbuy n =
                   let in
                     buyitem session owner id lowprice;
                     sleep 1000;
                     reptbuy (n-1)
                   end
               in
                 print (" ------------------- buying! [ " ^ 
                        itos lowprice ^ " ]\n");
                 reptbuy qty
               end
             else
               let in
                 print " - not buying.\n";
                 sleep 2000;
                 bt (nn+1) (if cash > (lowprice div 2) then limit 
                            else limit - 1)
               end))
      end (* handle _ => bt nn session name id price limit *)
    in
      bt 0 limit
    end



end