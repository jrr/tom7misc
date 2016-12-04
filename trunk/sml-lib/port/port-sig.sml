signature PORT =
sig

  (* Returns a list of strings describing the exception's history (or maybe nothing). *)
  val exnhistory : exn -> string list

  (* If this returns true, the two arguments are the same object. If false, they may or
     may not be equal. *)
  val fast_eq : 'a * 'a -> bool

  (* The default top-level exception handler. This may be the compiler's default if it
     is accessible, or may simply raise the exception. *)
  val defaultTopLevelHandler : exn -> 'a

  (* Set the top-level exception handler if possible. May do nothing. *)
  val setTopLevelHandler : (exn -> unit) -> unit

  (* Try to compact the heap. Semantically a no-op, and may literally do nothing on
     platforms that do not support it. *)
  val shareAll : unit -> unit

  (* Estimate the current memory usage, for debug messages. May produce nonsensical
     values on platforms where it is not supported. *)
  val memorySize : unit -> int

end