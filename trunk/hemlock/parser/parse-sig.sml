
signature PARSE =
sig
    (* expression parser *)
    val exp : (string * (int * Parsing.associativity)) list -> 
	           (EL.exp_ * Pos.pos,Tokens.token) Parsing.parser

end