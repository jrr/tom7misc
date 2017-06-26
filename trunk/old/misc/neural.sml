
type net = real list list

val thresh = (fn x => if x > 0.0 then 1.0 else 0.0)

val speed = 0.05
val epsilon = 0.0000001

fun eval inputs (nil : net) = inputs
  | eval inputs (ws :: rest) =
    let 
	(* weights are ordered output-major *)
	fun calc nil ws acc = acc :: calc inputs ws 0.0
	  | calc (inp::it) (w::wt) acc = calc it wt (inp * w + acc)
	  | calc _ nil _ = nil

	val pre = calc inputs ws 0.0

    in
	(* don't thresh the very last output, so that we
	   can do least-squares analysis *)
	case rest of
	    nil => pre
	  | _ => eval (map thresh pre) rest 
    end

fun classify i n = map thresh (eval i n)

(* if thresholded value is wrong,
   then use sqr(actual - thresh),
   else 0.0. *)
fun compare net nil = 0.0
  | compare net ((inp, out)::rest) =
    let val actual = eval inp net
    in 
	compare net rest +
	foldl op+ 0.0
	(ListPair.map (fn (act, exp) =>
		       if Real.== (thresh act, exp)
		       then 0.0
		       else epsilon + Real.abs(act)) (actual, out))
    end


val examples =
    [([8.0, 4.0, 1.0], [0.0]),
     ([4.0, 1.0, 1.0], [0.0]),
     ([4.5, 3.0, 1.0], [1.0]),
     ([3.0, 9.0, 1.0], [0.0]),
     ([2.0, 6.0, 1.0], [1.0]),
     ([7.0, 7.5, 1.0], [1.0]),
     ([9.0, 9.0, 1.0], [0.0]),
     ([1.0, 5.0, 1.0], [1.0]),
     ([5.0, 7.0, 1.0], [1.0]),
     ([1.0, 1.0, 1.0], [0.0]),
     ([6.0, 7.5, 1.0], [1.0]),
     ([1.0, 8.5, 1.0], [0.0]),
     ([9.0, 2.0, 1.0], [0.0]),
     ([6.0, 3.0, 1.0], [0.0]),
     ([3.0, 3.0, 1.0], [1.0]),
     ([8.5, 7.0, 1.0], [0.0]),
     ([4.0, 5.5, 1.0], [1.0]),
     ([1.5, 3.0, 1.0], [0.0]),
     ([0.0, 7.9, 1.0], [1.0]),
     ([7.0, 5.5, 1.0], [0.0])]


val rand = Random.rand (42, 999)

fun onereal lo hi =
    let val n = Random.randRange (0, 0x1FFFFFFF) rand
	val r = real n / (real 0x1FFFFFFF)
    in
	lo + ((hi - lo) * r)
    end

fun mkrandom lo hi nil = nil
  | mkrandom lo hi (n::t) =
    List.tabulate(n, fn _ => onereal lo hi) 
    :: mkrandom lo hi t

fun minimize net examples =
    let
	exception Done of net
	fun mini lhs cur nil _ = lhs
	  | mini lhs cur (nil::t) n = mini (lhs @ [cur]) nil t n
	  | mini lhs cur ((h::r)::t) n =
	    let
		(* working on h *)

		fun net hh = lhs @ ((cur @ (hh :: r)) :: t)
		fun score hh =
		    compare (net hh) examples

		fun findbest h =
		    let
			val now = score h
(*
			val _ = print ("[" ^ Int.toString n ^ 
				       "] Score: " ^ 
				       Real.toString now ^ "\n")
*)
			val change = score(h + speed) - now
		    in
			if score(h + (speed * now)) < now
			then h + (speed * now)
			else if score(h - (speed * now)) < now
			     then h - (speed * now)
			     else h
		    end
	    in
		mini lhs (cur @ [findbest h]) (r::t) (n + 1)
	    end

	fun many net prev =
	    let val mmm = mini nil nil net 0
		val new = compare mmm examples
	    in
		if (prev - new) > epsilon
		then many mmm new
		else mmm
	    end
	val init = compare net examples
    in
	(many net init) handle Done net => net
    end 

fun showperf net examples =
    let 
	fun prl l = app (fn r => print (Real.toString r ^ " ")) l;
    in
	app (fn (inp, ou) =>
	     let val act = eval inp net
		 val dif = 
		     foldl op+ 0.0
		     (ListPair.map (fn (act, exp) =>
				    if Real.== (thresh act, exp)
				    then 0.0
				    else epsilon + Real.abs(act)) (act, ou))
	     in
	     (print "[";
	      prl inp;
	      print "]: {";
	      prl ou;
	      print "}/{";
	      prl (eval inp net);
	      print "} = ";
	      print (Real.toString dif);
	      print "\n")
	     end) examples
    end

fun find sizes exs =
    let
	fun f iters (best, bn) =
	    let
		val net = minimize (mkrandom ~2.0 2.0 sizes) exs

		val res = compare net exs
	    in
		if iters mod 100 = 0
		then print (Int.toString iters ^ " iters: " ^ 
			    Real.toString best ^ "\n")
		else ();

		if Real.== (res, 0.0)
		then net
		else f (iters + 1) (if res < best then 
					let in
					    print ("New best: \n");
					    showperf net exs;
					    (res, net) 
					end
				    else (best, bn))
	    end
    in
	f 0 (99999.0, [])
    end


(* val solve = find [15, 5] examples; *)

fun show net = app (fn l =>
		    (print "[";
		     app (fn x =>
			  (print (Real.toString x);
			   print ", ")) l;
		     print "]")) net
    