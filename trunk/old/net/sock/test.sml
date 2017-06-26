
fun K x y = x
infixr ` fun a ` b = a b

val _ = 
(case CommandLine.arguments () of
     ["select"] =>
	 let
	     val n = 10
	     val s = INetSock.TCP.socket()
	     val _ = UnixSocket.bind (s, INetSock.any 3491)
	     val _ = UnixSocket.listen (s, 50)

	     fun loop nil = loop [#1 ` UnixSocket.accept ` s]
	       | loop l =
		 let 
		     val _ = print ("tr: " ^ Int.toString (length l + 1) ^ "\n")

		     val {rds, wrs, exs} =  UnixSocket.select {rds = UnixSocket.sockDesc s :: 
							             (map UnixSocket.sockDesc l; nil), 
							       wrs = nil, 
							       exs = map UnixSocket.sockDesc l, 
							       timeout = SOME ` Time.fromMilliseconds ` 5400}
		     val _ = print ("rd: " ^ Int.toString (length rds) ^
				   " ex: " ^ Int.toString (length exs) ^ "\n")

		     fun inlist nil _ = false
		       | inlist (h::t) i = UnixSocket.sameDesc (UnixSocket.sockDesc i, h) orelse inlist t i

		     val (dead, l) = List.partition (fn c => inlist exs c) l
		     val _ = app (fn c => print "Got exn!\n") dead

		     val _ = app (fn c =>
				  let
				      val _ = print "reading!\n"
				      val rd = SockUtil.vtos ` UnixSocket.recvVec (c, 1024)
				  in
				      app (fn s => ignore ` UnixSocket.sendVec (s, {buf=SockUtil.stov rd,i=0,sz=NONE})) l
				  end) ` List.filter (fn c => inlist rds c) l

		 in
		     loop 
		     (if inlist rds s then
			  let in
			      print "Going to accept!\n";
			      (#1 ` UnixSocket.accept ` s)::l
			  end
		      else l)
		 end
	 in
	     loop nil
	 end
   | ["send"] => 
	 let
	     val me = Word32.toString ` Posix.Process.pidToWord ` Posix.ProcEnv.getpid ()

	     open UnixSocket SockUtil
	     val s = INetSock.TCP.socket ()
	     val addr = "1.0.0.127"
	     val _ = connect (s, INetSock.toAddr (valOf (NetHostDB.fromString addr), 1111))
		 
	     fun loop () =
		 let in
		     sendVec (s, {buf=stov ("HELLO I AM " ^ me ^ "!"), i=0, sz=NONE});
		     (case select {rds = [sockDesc s],
				   wrs = nil,
				   exs = [sockDesc s],
				   timeout = SOME ` Time.fromMilliseconds ` 100} of
			  {rds=_, wrs=_, exs=_::_} =>
			      let in
				  print me;
				  print ": exn\n";
				  loop ()
			      end
			| {rds=_::_, wrs=_, exs=_} =>
			      let
				  val v = recvVec (s, 1024)
			      in
				  print ("[" ^ me ^ "]: " ^ SockUtil.vtos v ^ "\n");
				  loop ()
			      end
			| _ => let in
			      Posix.Process.sleep (Time.fromMilliseconds 800);
			      loop ()
			       end)
		 end
	 in
	     loop ()
	 end
   | ["sailor"] =>
	 let
	     val s = INetSock.TCP.socket ()
		 
	     val b = UnixSocket.bind (s, INetSock.any 3491)
		 
	     val c = UnixSocket.listen (s, 50)
		 
	     val (d, a) = UnixSocket.accept s
		 
	     val e = UnixSocket.sendVec 
		 (d, { buf=Word8Vector.fromList (map (Word8.fromInt o ord) (explode "hello sailor\r\n")), i=0, sz=NONE })
		 
	     val _ = while true do UnixSocket.sendVec (d, { buf= UnixSocket.recvVec (d, 256), i=0, sz=NONE })
		 
	 in
	     
	     ()
	     
	 end 

  | _ => let in
	 print "need command line argument (see test.sml)\n"
	  end
) handle UnixSocket.SockError s => print ("SockError: " ^ s ^ "\n");

val _ = print "Oops.\n"