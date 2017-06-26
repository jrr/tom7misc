
val states = map (fn s => "f:/emu/gb/rew-gbsave/" ^ s ^ ".st0")
  [ "0",
    "1",
    "2",
    "3" ]

exception Bad of string

val _ =
  case Haxor.equals [ 0w6, 0w6, 0w5, 0w5 ] states of
    [(loc,_)] =>
        (* exactly one match. change to 99: *)
        FileStuff.setbyte (hd states) loc 0w99
  | _ => raise Bad "multiple (or zero) locations!"
