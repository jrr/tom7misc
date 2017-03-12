structure Segment :> SEGMENT =
struct
  infixr 9 `
  fun a ` b = a b

  exception Segment of string
  datatype segment =
    S of { bytes : Word8Array.array,
           locked : bool array }
  fun empty () =
    S { bytes = Word8Array.array (65536, Word8.fromInt ` ord #"_"),
        locked = Array.array (65536, false) }

  (* just rename to set_idx? *)
  fun update (S { bytes, locked }) idx byte =
    if Array.sub (locked, idx)
    then raise Segment ("write to locked idx " ^ Int.toString idx)
    else Word8Array.update (bytes, idx, byte)
  fun set_idx seg idx byte = update seg idx byte

  fun set_range seg start len f =
    if start < 0 orelse start + len > 65536 orelse len < 0
    then raise Segment "set_range bad start/len"
    else
      Util.for 0 (len - 1)
      (fn i =>
       let val b = f i
       in update seg (start + i) b
       end)

  fun set_vec seg start vec =
    Util.for 0 (Word8Vector.length vec - 1)
    (fn i =>
     update seg (start + i) (Word8Vector.sub (vec, i)))

  fun set_string seg start str =
    Util.for 0 (size str - 1)
    (fn i =>
     update seg (start + i) (Word8.fromInt (ord (String.sub (str, i)))))

  fun set_repeating_string seg start len str =
    set_range seg start len
    (fn i => Word8.fromInt (ord (String.sub (str, i mod size str))))

  fun lock_range (S { locked, ... }) start len =
    Util.for 0 (len - 1)
    (fn i =>
     if Array.sub (locked, start + i)
     then raise Segment ("tried to lock already-locked " ^
                         Int.toString (start + i))
     else Array.update (locked, start + i, true))

  fun lock_idx (S { locked, ... }) idx =
    if Array.sub (locked, idx)
    then raise Segment ("tried to lock_idx already-locked " ^
                        Int.toString idx)
    else Array.update (locked, idx, true)

  fun range_unlocked _ _ 0 = true
    | range_unlocked (seg as S { locked, ... }) start len =
    if Array.sub (locked, start)
    then false
    else range_unlocked seg (start + 1) (len - 1)

  fun unlock_idx (S { locked, ... }) idx =
    Array.update (locked, idx, false)

  fun extract (S { bytes, ... }) =
    Word8Vector.tabulate (65536, fn i => Word8Array.sub (bytes, i))
end
