
structure AST =
struct

    datatype typ =
	Arrow  of typ * typ
      | Lolli  of typ * typ
      | With   of (string * typ) list  (* length   > 1 *)
      | Tensor of (string * typ) list  (* length =/= 1 *)
      | Sum    of (string * typ) list  (* length =/= 1 *)
      | Bang   of typ
      | All    of string * typ
      | Tvar   of string
      | Bool
      | String
      | Schan  of typ
      | Rchan  of typ

    datatype exp =
	Let    of string * exp * exp
      | Var    of string
	(*          f        x      dom   cod   body *)
      | Fix    of string * string * typ * typ * exp
      | Lfix   of string * string * typ * typ * exp
      | Lfn    of string * typ * exp
      | Tfn    of string * exp
      | Seq    of exp list (* length > 0 *)
      | App    of exp * exp
      | Lapp   of exp * exp
      | Tapp   of exp * typ
      | If     of exp * exp * exp
      | Ltuple of (string * exp) list
      | Tuple  of (string * exp) list
      | Exp    of exp   (* ! *)
      | Inj    of string * exp * typ
      | Lproj  of string * exp
	         (* lab       var *)
      | Lett   of (string * string) list * exp * exp
      | Case   of exp * string * (string * exp) list
      | Send   of exp * exp
      | Recv   of exp
      | Abort  of typ
      | Use    of string * exp * exp(* use ! *)
      | Chan   of typ
      | Close  of exp
      | Spawn  of exp
      | Print  of exp
      | True
      | False
      | Str of string
      (* only during eval *)
      | SC of Loc.loc
      | RC of Loc.loc


    (* evaluation contexts *)
    datatype frame =
	(* let s = [] in exp *)
	Flet of string * exp
	(* ([]; e1; e2; ...) *)
      | Fseq of exp list
	(* [] e *)
      | Fapp of exp
      | F1lapp of exp
      | F2lapp of exp
	(* []{t} *)
      | Ftapp of typ
        (* if [] then exp else exp *)
      | Fif of exp * exp
	(* (l1=v1, l2=v2, ..., s=[], ..., ln=en, ln+1=en+1) *)
      | Ftuple of (string * exp) list * string * (string * exp) list
	(* ! [] *)
(*      | Fexp *)
	(* Inj(s,_,[]) *)
      | Finj of string * typ
	(* #s [] *)
      | Flproj of string
	(* lett (l1=v1, ... ln=vn) = [] in exp *)
      | Flett of (string * string) list * exp
	(* case [] of ... *)
      | Fcase of string * (string * exp) list
	(* Send [] exp *)
      | F1send of exp
        (* Send v [] *)
      | F2send of exp
	(* Recv [] *)
      | Frecv
	(* Close [] *)
      | Fclose
	(* use s as [] in e *)
      | Fuse of string * exp
	(* Spawn [] *)
      | Fspawn
        (* Print [] *)
      | Fprint

end