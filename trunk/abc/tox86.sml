structure ToX86 :> TOX86 =
struct

  exception ToX86 of string

  val INIT_SP = Word16.fromInt 0x7e7e
  val INIT_IP = Word16.fromInt 0x2020

  fun tox86 asm = raise ToX86 "unimplemented: tox86"

end