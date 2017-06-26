
structure Print =
struct

    open AST

    fun sts nil = ""
      | sts [h] = ts h
      | sts (h::t) = ts h ^ "; " ^ sts t

    and stp nil = ""
      | stp [(s,h)] = s ^ " = " ^ ts h
      | stp ((s,h)::t) = s ^ " = " ^ ts h ^ ", " ^ stp t
	
    and stl nil = ""
      | stl [(s,ss)] = s ^ " = " ^ ss
      | stl ((s,ss)::t) = s ^ " = " ^ ss ^ ", " ^ stl t

    and ts True = "true"
      | ts False = "false"
      | ts (SC l) = "(SC " ^ Loc.tostring l ^ ")"
      | ts (RC l) = "(RC " ^ Loc.tostring l ^ ")"
      | ts (Let (s,a,b)) = "(let " ^ s ^ " = " ^ ts a ^ " in " ^ ts b ^ ")"
      | ts (Var s) = s
      | ts (Print e) = "(print " ^ ts e ^ ")"
      | ts (Str s) = "\"" ^ s ^ "\""
      | ts (Seq sl) = "seq " ^ sts sl ^ " end"
      | ts (App (a,b)) = "(" ^ ts a ^ " " ^ ts b ^ ")"
      | ts (Lapp (a,b)) = "(" ^ ts a ^ "^" ^ ts b ^ ")"
      | ts (Tapp (a,t)) = "(" ^ ts a ^ "[-])"
      | ts (Tfn (s,e)) = "(/\\" ^ s ^ " . " ^ ts e ^ ")"
      | ts (Tuple sel) = "{" ^ stp sel ^ "}"
      | ts (Lett (ssl,a,b)) = "lett {" ^ stl ssl ^ "} = " ^ ts a ^ " in " ^ ts b ^ " end"
      | ts (Send (a,b)) = "(Send (" ^ ts a ^ ", " ^ ts b ^ "))"
      | ts (Recv a) = "(Recv " ^ ts a ^ ")"
      | ts (Close a ) = "(Close " ^ ts a ^ ")"
      | ts (Chan _) = "(Chan _)"
      | ts (Spawn e) = "(Spawn " ^ ts e ^ ")"
      | ts (If (c, t, f)) = "(If " ^ ts c ^ " then " ^ ts t ^ " else " ^ ts
	                      f ^ ")"
      | ts (Lfn (x, _, bod)) = "(L\\ " ^ x ^ " . " ^ ts bod ^ ")"
      | ts (Lfix (f, x, _, _, bod)) = "(" ^ f ^ " as L\\ " ^ x ^ " . " ^ 
	                                    ts bod ^ ")"
      | ts (Fix (f, x, _, _, bod)) = "(" ^ f ^ " as \\ " ^ x ^ " . " ^
					    ts bod ^ ")"
      | ts (Use (s, a, b)) = "(use " ^ s ^ " = " ^ ts a ^ " in " ^ ts b ^ ")"
      | ts (Exp e) = "(exp " ^ ts e ^ ")"
      | ts _ = "..."


    and fs (Flet (s,e)) = "(F1let " ^ s ^ " = [] in " ^ ts e ^ ")"
      | fs (Fseq el) = "(Fseq " ^ sts el ^ ")"
      | fs (Fapp a) = "(Fapp [] " ^ ts a ^ ")"
      | fs (F1lapp a) = "(F1lapp []^" ^ ts a ^ ")"
      | fs (F2lapp a) = "(F2lapp " ^ ts a ^ "^[])"
      | fs (Ftapp _) = "(Ftapp)"
      | fs (Ftuple (b,s,a)) = "(Ftuple " ^ stp b ^ ", " ^ s ^ " = " ^ " [] in " ^ stp a ^ ")"
      | fs (Flett (ssl,e)) = "(Flett {" ^ stl ssl ^ "} = [] in " ^ ts e ^ ")"
      | fs (F1send a) = "(F1send " ^ ts a ^ ")"
      | fs (F2send a) = "(F2send " ^ ts a ^ ")"
      | fs (Fclose) = "Fclose"
      | fs (Frecv) = "Frecv"
      | fs (Fuse (s,b)) = "(Fuse " ^ s ^ " = [] in " ^ ts b ^ ")"
      | fs (Finj (s,t)) = "(Finj " ^ s ^ " [])"
      | fs (Fif (t,f)) = "(Fif " ^ ts t ^ ", " ^ ts f ^ ")"
      | fs Fprint = "Fprint"
      | fs Fspawn = "Fspawn"
      | fs _ = "..."

    and ss nil = "(*)"
      | ss (h::t) = fs h ^ " |> " ^ ss t

    fun ps (fr,e) = "(" ^ ss fr ^ ", " ^ ts e ^ ")"

    fun pss nil = ""
      | pss [pr] = ps pr
      | pss (h::t) = ps h ^ " ||\n" ^ pss t

end