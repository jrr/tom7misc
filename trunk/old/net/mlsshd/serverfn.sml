
functor ServerFn(A : SERVERARG) :> SERVER =
struct

  val verbose = 
      Params.flag false
                  (SOME
                   ("-verbose",
                    "Announce everything that happens"))
                  "verbose"
    
  val inetd = 
      Params.flag false
      (SOME
       ("-inetd",
	"Run as an inetd app (stdin/stdout)"))
      "inetd"

  exception Exit

  fun nameof a = "XXX-unknown.org"

  (* control flow starts here *)
  fun run () =
      if !inetd then
        let
	    val _ = A.init ()
	    val _ = A.beforeloop ()
	    val host = "XXX-gethost.org"

        in
	    print ("inetd mode not supported yet...!\n");
	    raise Exit
	end
      else 
    let
      val _ = MLton.Syslog.log (MLton.Syslog.NOTICE, (A.name ^ " started."))
      val _ = A.init ()
      val port = A.port ()
      val listener = INetSock.TCP.socket ()
      val _ = 
	  let in 
	      UnixSocket.bind (listener, INetSock.any port);
	      UnixSocket.listen (listener, 20)
	  end handle _ => 
	      let 
		  val msg = "Can't listen at port " ^ Int.toString port ^ "."
	      in
		  MLton.Syslog.log (MLton.Syslog.ERR, msg);
		  print (msg ^ "\n");
		  raise Exit
	      end

      val _ = print "Listening...\n"

      val _ = A.beforeloop ()

      fun loop pids =
       let 
         (* need to call wait as many times as we can
            to avoid zombie leaks. *)
         fun wait ps = 
	     (case Posix.Process.waitpid_nh 
		  (Posix.Process.W_ANY_CHILD, nil) of
		  NONE => ps
		| SOME (pid,_) => 
		      List.filter (fn p => p <> pid)
                       (wait ps)) handle _ => ps

         (* wait on zombies to get rid of them *)
         val pids = wait pids

	 val _ = print "Accept...\n"

         val (sock, addr) = UnixSocket.accept listener

	 val _ = print "Accpted!\n"

	 val nadd = nameof addr
	 val p = 999 (* FIXME: get the actual port *)

	 val _ = if !verbose then print ("Connection from: " ^ nadd ^ ".\n") else ()

        in
          case Posix.Process.fork () of
            NONE => 
              let in
		MLton.Syslog.openlog (A.name, [MLton.Syslog.PID], MLton.Syslog.DAEMON);
                A.process_connection (nadd, addr, p, sock)
              end 
          | SOME pid => 
              let in
		(* XXX do I close socket here? *)
                loop (pid :: pids)
              end
        end
    in
      loop nil
    end


end
