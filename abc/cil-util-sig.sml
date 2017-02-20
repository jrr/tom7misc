signature CILUTIL =
sig

  structure BC :
  sig
    type 'a blockcollector
    type label = string
    val empty : unit -> 'a blockcollector
    (* Generated labels are distinct, even across different
       blockcollector instances or if the collector is extracted. *)
    val genlabel : string -> label
    val label : string -> label
    val insert : 'a blockcollector * label * 'a -> unit

    (* Empties the block collector, returning its contents. No
       label appears more than once. *)
    val extract : 'a blockcollector -> (label * 'a) list
    (* XXX tolist or whatever *)
  end

  val newlocal : string -> string
  val genvar : string -> string

end