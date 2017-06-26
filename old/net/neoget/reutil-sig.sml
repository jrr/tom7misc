

(* 


 Note: This code does not work any more.
 Use the one in sml-lib/util/re*


*)


signature REUTIL =
sig

  structure R : REGEXP

  (* find regexp matchstring 
     finds the first match in matchstring for regexp.
     (properly staged) *)
     
  val find : string -> string -> (int -> string) option

  (* finds all of them *)
  val findall : string -> string -> (int -> string) list

  (* true if any match *)
  val ismatch : string -> string -> bool

end