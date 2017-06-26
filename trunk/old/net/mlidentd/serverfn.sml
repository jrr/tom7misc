
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

  open MLton.Socket
    
  fun nameof a = 
      case Host.getByAddress a of
	  NONE => SockUtil.ipstring a
        | SOME {name} => name

  (* control flow starts here *)
  fun run () =
      if !inetd then
        let
	    val _ = A.init ()
	    val _ = A.beforeloop ()
	    val host = 
		case (SockUtil.getpeername TextIO.stdOut) of
		    NONE => 0w0
		  | SOME a => a
        in
          MLton.Syslog.openlog (A.name, [MLton.Syslog.PID], MLton.Syslog.DAEMON);
          A.process_connection (nameof host,
                                host,
                                0, (* XXX *)
                                TextIO.stdIn, 
                                TextIO.stdOut)
        end handle SockUtil.SockUtil s =>
          let val msg = 
            "With -inetd, stdin must be a socket. getpeername: " ^ s
          in
	      MLton.Syslog.log (MLton.Syslog.ERR, msg);
            print (msg ^ "\n");
            raise Exit
          end
      else 
    let
      val _ = MLton.Syslog.log (MLton.Syslog.NOTICE, (A.name ^ " started."))
      val initial_state = A.init ()
      val port = A.port ()
      val listener = (listenAt port)
        handle _ => 
          let 
	      val msg = "Can't listen at port " ^ Int.toString port ^ "."
	  in
	      MLton.Syslog.log (MLton.Syslog.ERR, msg);
	      print (msg ^ "\n");
	      raise Exit
          end

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

         val (add, p, ins, outs) = accept listener
	 val nadd = nameof add
	 val _ = if !verbose then print ("Connection from: " ^ nadd ^ ".\n") else ()

        in
          case Posix.Process.fork () of
            NONE => 
              let in
		MLton.Syslog.openlog (A.name, [MLton.Syslog.PID], MLton.Syslog.DAEMON);
                A.process_connection (nadd, add, p, ins, outs)
              end 
          | SOME pid => 
              let in
                TextIO.closeIn ins;
                TextIO.closeOut outs;
                loop (pid :: pids)
              end
        end
    in
      loop nil
    end


end
