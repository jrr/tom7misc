(* Manipulation of 64k segments. Convenient and paranoid. *)
signature SEGMENT =
sig
  (* Out-of-bounds writes and writes to locked regions throw
     an exception. *)
  exception Segment of string
  (* Always 64kb. *)
  type segment
  val empty : unit -> segment

  (* set_repeating seg start len str
     Fill with copies of the string over and over. *)
  val set_repeating_string : segment -> int -> int -> string -> unit

  (* set_range seg start len f
     call f 0, f 1, ... f (len - 1) to compute the bytes
     to populate the segment, starting at start. *)
  val set_range : segment -> int -> int -> (int -> Word8.word) -> unit

  (* set_range seg start str
     Write the string to the segment at the start location. *)
  val set_string : segment -> int -> string -> unit

  (* set_vec seg start vec *)
  val set_vec : segment -> int -> Word8Vector.vector -> unit

  (* set_idx seg idx value *)
  val set_idx : segment -> int -> Word8.word -> unit

  (* lock_range seg start len
     Locks the range so that an exception is raised if a write
     (or lock) to that region is attempted. *)
  val lock_range : segment -> int -> int -> unit
  val lock_idx : segment -> int -> unit
  (* Allows unlocking even if the region is not locked. *)
  val unlock_idx : segment -> int -> unit

  (* range_unlocked seg start len
     Returns true if the entire range is unlocked. *)
  val range_unlocked : segment -> int -> int ->bool

  (* Need not all be initialized. *)
  val extract : segment -> Word8Vector.vector
end
