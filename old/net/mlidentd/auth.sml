

structure Ident :> SERVERARG =
struct

    exception Exit

    val passwd_file = 
      Params.param "/etc/passwd" 
           (SOME 
            ("-passwd", 
             "The passwd file")) 
            "passwd_file"

    val verbose = 
      Params.flag false
           (SOME
            ("-verbose",
             "Announce everything that happens"))
           "verbose"

    val drop_privs =
	Params.flag true
	   (SOME
	    ("-drop-privs",
	     "Setuid to an unprivileged account"))
	   "drop_privs"
	   
    val account =
	Params.param "nobody"
	   (SOME
	    ("-account",
	     "Account to run as (if drop-privs set)"))
	   "account"

    val listenport = 
        Params.param "113"
           (SOME
            ("-port",
             "Port on which to listen (when in standalone mode)"))
           "listenport"

    val tcp =
        Params.param "/proc/net/tcp"
           (SOME
            ("-tcp",
             "The file from which to read TCP info"))
           "tcp"

    val log_info =
        Params.flag true
           (SOME 
            ("-log-info",
             "Log INFO level messages to the syslog"))
           "log_info"

    fun info s =
        let in
            if !verbose then
                print ("INFO: " ^ s ^ "\n")
            else ();
            if !log_info then
                MLton.Syslog.log (MLton.Syslog.INFO, s)
            else ()
        end

    fun beforeloop () =
	if (!drop_privs) then
	    case Passwd.lookup (!account) of
		NONE => 
		    let in
			print ("Can't switch to user '" ^ (!account) ^ "'!\n");
			raise Exit
		    end
	      | SOME {uid, ...} => 
		    Posix.ProcEnv.setuid (Posix.ProcEnv.wordToUid 
					  (Word32.fromInt uid))
	else ()

    fun port () = Option.getOpt (Int.fromString (!listenport), 113)

    fun init () =
      ignore
      let in
        Passwd.readdb (!passwd_file)
      end
	
    fun isws (#" ") = true
      | isws (#"\r") = true
      | isws (#"\n") = true
      | isws (#"\t") = true
      | isws _ = false

    fun is c d = c = d
	
(* format of /proc/net/tcp:

  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode       
                        
   0: 0BC20280:0016 348D1A26:9C7F 01 00000014:00000000 01:0000001C 00000000     0        0 11842482    
   ...
   
 *)

    fun tcpline s =
      case String.tokens isws s of
        [_, local_address, rem_address, _, _, _, _, uid, _, _] =>
          (case String.tokens (is #":") local_address of
             [la, lp] => 
               (case String.tokens (is #":") rem_address of
                  [ra, rp] => SOME (la, lp, ra, rp, uid)
                | _ => NONE)
           | _ => NONE)
      | _ => NONE

    fun getuid (sp, cp, host) =
      let
        val f = TextIO.openIn (!tcp)
        val hsp = StringUtil.word16tohex sp
        val hcp = StringUtil.word16tohex cp
        val hhost = StringUtil.wordtohex_le ( host)

        fun findconn () =
          case TextIO.inputLine f of
            ""   => NONE
          | "\n" => findconn ()
          | s    => 
              case tcpline s of
                NONE => findconn ()
              | SOME (la, lp, ra, rp, uid) =>
                  if (lp = hsp andalso
                      rp = hcp andalso
                      ra = hhost) then SOME (Option.getOpt
                                             (Int.fromString uid, ~1))
                  else findconn ()
        val r = findconn ()
      in
        TextIO.closeIn f;
        r
      end handle _ => NONE

    (* build a response to a query *)
    fun parse host_addr x = 
      case String.tokens (is #",") (StringUtil.filter (not o isws) x) of
        [n1, n2] => 
          let
            val serverport = Option.getOpt (Int.fromString n1, 0)
            val clientport = Option.getOpt (Int.fromString n2, 0)
          in
            Int.toString serverport ^ "," ^ 
            Int.toString clientport ^ ":" ^
            (case getuid (Word.fromInt serverport, 
                          Word.fromInt clientport, 
                          host_addr) of
               NONE => "ERROR:NO-USER"
             | SOME u => case Passwd.lookupuid u of
                 NONE => "ERROR:NO-USER"
               | SOME {login,...} =>
                   "USERID:UNIX:" ^ login)
          end
      | _ => "0,0:ERROR:UNKNOWN-ERROR"

    fun process_connection (hostname, host_addr, port, ins, outs) =
	let
	    val input = TextIO.inputLine ins
	    val input = 
              StringUtil.filter (fn #"\r" => false | 
                                    #"\n" => false | 
                                        _ => true) input
	    val input = StringUtil.harden 
                          (StringUtil.inlist [#",", #" "]) #"+" 256 input
	    val _ = info (hostname ^ ": " ^ input)
	in
	    TextIO.output (outs, (parse host_addr input) ^ "\r\n");
	    TextIO.flushOut outs;
	    TextIO.closeIn ins;
	    TextIO.closeOut outs
	end

    val name = "mlidentd"

end
