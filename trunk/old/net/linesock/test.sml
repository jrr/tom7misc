
structure Test =
struct

    structure LS = LineSockFn(val maxline = 4096 structure K = IntKey)
    open LS

    val ctr = ref 0
    fun ++ (r as ref c) = (r := c + 1; c + 1)

    val _ =
	case CommandLine.arguments () of
	    ["server"] =>
		let
		    val s = INetSock.TCP.socket()
		    val _ = UnixSocket.bind (s, INetSock.any 3491)
		    val _ = UnixSocket.listen (s, 50)
			
		    fun loop sox =
			case wait (SOME s, sox, NONE) of
			    Closed k =>
				let in
				    print (Int.toString k ^ ": closed\n");
				    loop (List.filter (fn l => getkey l <> k) sox)
				end
			  | Line (k, s) => 
				let in
				    print (Int.toString k ^ ": [" ^ s ^ "]\n");
				    loop sox
				end
			  | Accept (s,a) => 
				let 
				    val nc = ++ ctr
				in
				    print (Int.toString nc ^ ": accepted connection\n");
				    loop (lsock ("", s, nc) :: sox)
				end
			  | Timeout => loop sox
		in
		    loop nil
		    
		end

end