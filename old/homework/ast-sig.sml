
(* this is the AST for a homework document *)

signature AST =
sig

    datatype base = Hex | Dec

    datatype code =
	Join of code list
      | Text of string
      | Typefile of string
      | Var of string
      | Numrange of base * int * int
      | Oneshot of string * code
      | Assign of string * code
      | If of cond * code
      | Unset of string
      | Cap of code
      | Capwords of code
      | Low of code
      | An of code

    and cond =
	Not of cond
      | Rep
      | Var of string

    (* an option is some code and a bool flag saying if it is unlimited *)
    type optn = code * bool

    (* a family is a bool indicating whether or not it is 'unique' and a list
       of options. *)
    type family = bool * optn list

	
    type file =
	{ deps : string list,
	  code : (string, family) map }
end