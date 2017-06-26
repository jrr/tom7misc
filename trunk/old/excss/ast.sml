
(* datatype for CFL, the Common Functional Language *)

structure AST =
struct

	 type var = string

	 datatype exp =
		  INC of exp
		| ZERO
		| NATCASE of exp * exp * var * exp

		| App of exp * exp
		| Fun of var * var * exp

		| Let of dec list * exp

		| LISTCASE of exp * var * exp * var * var * exp
		| CONS of exp * exp
		| NIL

		| Error

	 and dec = 
		  Val of var * exp


end