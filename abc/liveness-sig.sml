signature LIVENESS =
sig

  exception Liveness of string

  structure TS : ORD_SET where type Key.ord_key = ASM.named_tmp

  val liveness : ASM.named_block list ->
    { cmds : ASM.named_cmd Array.array,
      (* XXX Need some way to carve it back into blocks... *)
      inlive : TS.set Array.array,
      outlive : TS.set Array.array }

end