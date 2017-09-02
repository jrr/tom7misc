
(* TODO: This (syntactically ill-formed) program produces
   an unhandled "Subscript" exception in mlton 20100608. *)
exception Segments of string
datatype gatenum = Start | End | Num of int

fun split (SOME start, _, _) ((Start, _) :: _) =
   raise Segments ("Duplicate starts in " ^ name)
| split (NONE, nn, ee) ((Start, coords) :: rest) =
   split (SOME coords, nn, ee) rest
| split (_, _, SOME e) ((End, _) :: _) =
   raise Segments ("Duplicate ends in " ^ name)
| split (ss, nn, NONE) ((End, coords) :: rest) =
   split (ss, nn, SOME coords) rest
| split (ss, nn, ee) = ((Num n, coords) :: rest) =
   split (ss, (n, coords) :: nn, ee) rest
| split (SOME s, nn, SOME e) nil = (s, nn, e)
| split (NONE, _, _) nil =
   raise Segments ("Missing start in " ^ name)
| split (_, _, NONE) nil =
   raise Segments ("Missing end in " ^ name)

val (sp, np, ep) = split (NONE, nil, NONE) numbered
