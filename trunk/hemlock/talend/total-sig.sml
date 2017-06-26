
(* That's "to TAL," not "total." *)

signature TOTAL =
sig

    exception ToTAL of string

    val write : RTL.program -> string -> unit

end