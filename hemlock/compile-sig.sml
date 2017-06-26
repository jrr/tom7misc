
(* Interface to the Hemlock compiler. *)

signature COMPILE =
sig

    (* compile source progname
       
       takes source file and produces binary. 
       progname should not be a path, rather, a base filename.
       this name is used for the name of the cordcode (prog.tar.gz)
       and client (prog)
       *)
    val compile : string -> string -> unit

    (* interpret post closure- and allocation-conversion. Just for
       debugging. *)
    val interpret : string -> unit

    (* for the interactive loop, make the compiler quieter *)
    val quiet : unit -> unit
    val skipassemble : unit -> unit
end
