
val _ = 
let
  val s = INetSock.UDP.socket ()
    
(*  val _ = UnixSocket.bind (s, INetSock.any 1111) *)

  fun delimit s nil = ""
    | delimit s (h::t) =
	foldl (fn (a, b) => b ^ s ^ a) h t

  val addr = "1.0.0.127"

  val msg = delimit " " (CommandLine.arguments ())


  val _ = UnixSocket.sendVecTo (s, INetSock.toAddr (valOf (NetHostDB.fromString addr), 1111),
				{buf=SockUtil.stov msg,
				 i=0, sz=NONE})

  val _ = print "sent.\n"
in

  ()

end handle UnixSocket.SockError s => print ("SockError: " ^ s ^ "\n");
