
val _ = 
let
  val s = INetSock.UDP.socket ()
    
  val _ = UnixSocket.bind (s, INetSock.any 1111)

  val _ = while true do 
      let 
	  val (v, a) = UnixSocket.recvVecFrom (s, 1024)
      in
	  print (SockUtil.vtos v);
	  print "\n"
      end

in

  ()

end handle UnixSocket.SockError s => print ("SockError: " ^ s ^ "\n");
