(* "pretty" printer for CPS terms. *)

(* Layout is probably not necessary here since CPS terms are mostly
   "linear" *)

(* XXX however, this is slow because, for one thing, I use ^ all the time
   instead of concat. porting to layout might make sense in that light. *)

structure CPSPrint :> CPSPRINT =
struct

    structure V = Variable
    open CPS
    open Primop

   fun nspaces j = StringUtil.tabulate j #" "

    fun vtos (Var v) = V.tostring v
      | vtos (Label l) = "*" ^ V.tostring l
      | vtos (Int i) = Int.toString i
      | vtos (String s) = "\"" ^ String.toString s ^ "\""

    fun ttos INT = "INT"
      | ttos STRING = "STRING"
      | ttos CODE = "CODE"
      | ttos REF = "REF"
      (* int * t. Used to tag object language sums. *)
      | ttos (INT_T n) = "INT_" ^ Int.toString n ^ "_T"
      (* tuple of the supplied length. Limited to ??? *)
      | ttos (TUPLE i) = "TUPLE_" ^ Int.toString i

    fun etosi i (App (v, vl)) = 
           nspaces i ^ 
           vtos v ^ " [" ^ (StringUtil.delimit ", " (map vtos vl)) ^ "]\n"
      | etosi i (Alloc (t, vas, v, rest)) =
           nspaces i ^ 
           V.tostring v ^ " = (" ^ ttos t ^ " | " ^ 
               (StringUtil.delimit ", " (map vtos vas)) ^ ")\n"
           ^ etosi i rest
      | etosi i (Project (j, vl, vr, rest)) =
           nspaces i ^ V.tostring vr ^ " = #" ^ Int.toString j ^ 
           " " ^ vtos vl ^ "\n"
           ^ etosi i rest
      | etosi i (Fix ([], rest)) =
           nspaces i ^ "fix (no functions)\n" ^
           etosi i rest
      | etosi i (Fix (vael, rest)) =
           nspaces i ^ 
           "fix " ^ (StringUtil.delimit (nspaces i ^ "and ") 
                     (map (fn (v, a, e) =>
                           V.tostring v ^ "(" ^ StringUtil.delimit ", "
                           (map V.tostring a) ^ ") as\n" ^
                           etosi (i + 4) e) vael))
           ^ etosi i rest
      | etosi i (Primop (PBind, [vl], [vr], [c])) =
           nspaces i ^
           V.tostring vr ^ " = " ^ vtos vl ^ "\n" ^
           etosi i c
      | etosi i (Primop (po, vls, vrs, cs)) =
           nspaces i ^
           (if length vrs > 0 
            then if length vrs = 1 then V.tostring (hd vrs) ^ " = "
                 else "(" ^ StringUtil.delimit ", " (map V.tostring vrs) 
                    ^ ") = "
            else "") ^ Primop.tostring po ^ " (" ^
                StringUtil.delimit ", " (map vtos vls) ^ ")\n" ^
                (if length cs <> 1 
                 then StringUtil.delimit (nspaces (i + 2) ^ "or\n") 
                                         (map (etosi (i + 4)) cs)
                 else (etosi i (hd cs)))
      | etosi i (Deferred os) =
           (case Util.Oneshot.deref (os()) of
                NONE => nspaces i ^ "(XXX DEFERRED)\n"
              | SOME e => etosi i e)
      | etosi i (Sumswitch(va, vr, icl, def)) = 
           nspaces i ^
             "sumswitch " ^ V.tostring vr ^ " = " ^ vtos va ^ " of\n" ^ 
             String.concat 
                 (map (fn (j, c) =>
                       nspaces (i + 2) ^ 
                       Int.toString j ^ " =>\n" ^ etosi (i + 8) c) icl) ^
             nspaces (i + 2) ^ "_ =>\n" ^ etosi (i + 8) def
      | etosi _ _ = "XXX intswitch/sumswitch\n"


    fun printe e = print (etosi 0 e)

end