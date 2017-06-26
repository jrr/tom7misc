
(* Proof "checker" for propositional logic, grid-style. *)

let

    (* propositions *)
    datatype prop =
        AND of prop * prop
      | OR of prop * prop 
      | $ of int
      | IMP of prop * prop
      | F of unit
      | T of unit
        
    infix 8 AND
    infix 6 OR
    infixr 3 IMP


    fun ptos (a AND b) = "(" ^ ptos a ^ " /\\ " ^ ptos b ^ ")"
      | ptos (a OR b)  = "(" ^ ptos a ^ " \\/ " ^ ptos b ^ ")"
      | ptos ($n)      = Int.toString n
      | ptos (a IMP b) = "(" ^ ptos a ^ " -> " ^ ptos b ^ ")"
      | ptos (F ())    = "F"
      | ptos (T ())    = "T"


    fun eq_prop ($ v1, $ v2) = v1 = v2
      | eq_prop (a AND b, aa AND bb) =
        eq_prop (a, aa) andalso eq_prop(b, bb)
      | eq_prop (a OR b, aa OR bb) =
        eq_prop (a, aa) andalso eq_prop(b, bb)
      | eq_prop (a IMP b, aa IMP bb) =
        eq_prop (a, aa) andalso eq_prop(b, bb)
      | eq_prop (F (), F ()) = true
      | eq_prop (T (), T ()) = true
      | eq_prop _ = false



    datatype ('a, 'b) sequent = ==> of 'a * 'b
    infix 2 ==>

    (* contexts *)
    val empty = nil

    fun ++(a, b) = b :: a
    infix 1 ++

    fun has G p = List.exists (fn pp => eq_prop (p, pp)) G

    (* parallel tuple construction *)
    fun &&(f1, f2) = (f1 (), f2 ())
(*
        let val t1 = spawn f1
            val t2 = spawn f2

            val res = syncall [| t1, t2 |]
        in
            (sub (res, 0), sub (res, 1))
        end
*)
    infix &&

	
    datatype proof = 
	Initial of prop
      | TrueR of unit
      | FalseL of unit
      | AndL of proof
      | AndR of proof * proof
      | ImpR of proof
      | OrL of proof * proof
	
      (* passive *)
      | OrR1 of proof
      | OrR2 of proof
      | ImpL of proof * proof

    fun pftos p =
	(case p of
	     Initial a => "Initial ?"
	   | TrueR () => "TrueR"
	   | FalseL () => "FalseL"
	   | AndL p => "AndL " ^ pftos p
	   | ImpR p => "ImpR " ^ pftos p
	   | OrR1 a => "OrR1 " ^ pftos a
	   | OrR2 a => "OrR2 " ^ pftos a
	   | AndR (a, b) => concat [ "AndR (", pftos a, ", ", 
				      pftos b, ")" ]
	   | OrL (a, b) => concat [ "OrL (", pftos a, ", ", 
				      pftos b, ")" ]		
	   | ImpL (a, b) => concat [ "ImpL (", pftos a, ", ", 
				      pftos b, ")" ])

    fun through f x =
	case x of
	    NONE => NONE
	  | SOME y => SOME(f y)

    fun through2 f x =
	case x of
	    (SOME a, SOME b) => SOME (f (a, b))
	  | _ => NONE

    fun stos (G ==> P) =
	StringUtil.delimit ", " (map ptos G) ^
	" ==> " ^ ptos P

    (* proving *)
    fun prove (G ==> P) =
        let 

	    val _ = print ("prove: " ^ stos (G ==> P) ^ "\n")
	    

            (* Collect all possible sequents, 
               then try them all *)
            fun try_passive (G ==> P) =
                let 
		    datatype ('a, 'b) sum = L of 'a | R of 'b

                    val next = 
                    (case P of
                         A OR B => L(G ==> A, OrR1) :: L(G ==> B, OrR2) :: nil
                       | _ => nil) @
                    let
                        (* Completeness relies on the conjecture that
                           if G,AIMPB ==> A and G,AIMPB,B ==> P 
                           then G ==> A and G,B ==> P. 
			   Otherwise one of our sequents does not get
			   smaller and we may not terminate. *)

                        fun lefts ((p as (A IMP B)) :: t, G') =
                                     (R((G' @ t) ==> A,
                                       (B :: G' @ t) ==> P,
				       ImpL) :: nil) @
                                     lefts (t, p :: G')
                          | lefts (p :: t, G') = lefts (t, p :: G')
                          | lefts (nil, _) = nil
                    in
                        lefts (G, nil)
                    end

		    fun tryall (R(s1, s2, rule)::rest) =
			(case through2 rule ((fn () => prove s1) &&
					     (fn () => prove s2)) of
			     NONE => tryall rest
			   | pf => pf)

		      | tryall (L(s, rule)::rest) =
			(case through rule (prove s) of
			     NONE => tryall rest
			   | pf => pf)

		      | tryall (nil) = NONE

                in
		    tryall next
                end

            fun try_initial (G ==> P) = 
                if has G P
		then SOME (Initial P)
                else try_passive (G ==> P)

            fun invert_right G =
                case P of
                    A AND B =>
			through2 AndR
                        ((fn () => prove (G ==> A)) &&
			 (fn () => prove (G ==> B)))
                  | T () => SOME (TrueR ())
                  | A IMP B => through ImpR (prove ((G ++ A) ==> B))
                  | _ => try_initial (G ==> P)


            fun invert_left ((A OR B) :: t, G') =
		through2 OrL
                ((fn () => prove ((A :: (G' @ t)) ==> P)) &&
		 (fn () => prove ((B :: (G' @ t)) ==> P)))

              | invert_left ((A AND B) :: t, G') =
                through AndL
		(invert_left (t, A :: B :: G'))

	      (* not really a rule... *)
              | invert_left (T() :: t, G') =
                invert_left (t, G')

              | invert_left (F() :: _, _) = SOME (FalseL ())

              | invert_left (h :: t, G') =
                invert_left (t, h :: G')

              | invert_left (nil, G') = invert_right G'

        in
            invert_left (G, nil)
        end


    val A = $0
    val B = $1
    val C = $2
    val D = $3

    val s = (A IMP B IMP C) IMP (A IMP B) IMP A IMP C
    val k = A IMP B IMP A
    val wrongk = (A IMP B) IMP A
    val commute_or = A OR B IMP B OR A

    val nontheorem = A OR B IMP A AND B
    val hellno = A IMP B

    val seq = empty ==> s AND k AND commute_or

    fun runcord f = f ()
in

    case runcord (fn () => prove seq) of
	SOME pf => 
	    let in
		print "It was proven!:\n\n";
		print (pftos pf);
		print "\n"
	    end
      | _ => print "The theorem is not true!\n"
    
end
