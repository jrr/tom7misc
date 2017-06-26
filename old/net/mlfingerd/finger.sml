

structure Finger :> SERVERARG =
struct

    exception Exit

    val listenport = 
      Params.param "79" 
                   (SOME ("-port", "The port on which to listen")) 
                   "listenport"

    val passwd_file = 
      Params.param "/etc/passwd" 
                   (SOME ("-passwd", "The passwd file")) 
                   "passwd_file"

    val include_plan =
      Params.flag true 
                  (SOME 
                   ("-include-plan", 
                    "Include user-defined .plan files"))
                  "include_plan"

    val plan_file =
      Params.param ".plan"
                   (SOME ("-planfile",
                          "Name of plan file (off user's home dir)"))
                   "plan_file"

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

    val log_info =
        Params.flag true
                   (SOME ("-log-info",
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

    fun port () = Option.getOpt (Int.fromString (!listenport), 79)

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

        
    val splitword = String.tokens isws 

    fun information ext user =
      case Passwd.lookup user of
        NONE => "Unknown user."
      | SOME {login, uid, gid, name, home, shell} =>
        let val stuff = 
       StringUtil.table 75 [["login: " ^ login, "uid:   " ^ Int.toString uid],
                            ["name:  " ^ name,  "gid:   " ^ Int.toString gid],
                            ["home:  " ^ home,  "shell: " ^ shell]]
            val plan =
                if !include_plan then
                   let (* val _ = print (home ^ (!plan_file) ^ "\n") *)
                       val f = TextIO.openIn (home ^ "/" ^ (!plan_file))
                       val ptext = TextIO.inputAll f
                   in
                       TextIO.closeIn f;
                       ptext
                   end handle _ => "" (* can't read *)
                else ""
        in
            stuff ^ "\n" ^ plan ^ "\n"
        end

    (* build a response to a query *)
    fun parse x = 
        let val cl = explode x
        in
            if (List.all isws cl) then
                "User listing not supported."
            else if (List.exists (fn c => c = #"@") cl) then
                "Forwarding not supported."
            else 
              case splitword x of
                ["/W", user] => "Extended information for user '" ^ user ^ 
                                "':\r\n" ^ (information true user)
              | [user] => "Information for user '" ^ user ^ "':\r\n" ^
                                (information false user)
              | _ => "Error."
        end

    fun process_connection (host, _, port, ins, outs) =
        case TextIO.inputLine ins of
            SOME input =>
               let
                   val input = StringUtil.filter (fn #"\r" => false | #"\n" => false | _ => true) input
                       
                   (* if your usernames contain any funny characters, add them here: *)
                   val input = StringUtil.harden 
                       (StringUtil.inlist [#"@", #".", #"_", #"-"]) #"+" 256 input
                   val _ = info (host ^ ": " ^ input)
               in
                   TextIO.output (outs, (parse input) ^ "\r\n");
                   TextIO.flushOut outs;
                   TextIO.closeIn ins;
                   TextIO.closeOut outs
               end
          | NONE => (* timeout *)
               let in
                   info (host ^ " timed out.");
                   TextIO.closeIn ins;
                   TextIO.closeOut outs
               end handle _ => ()

    val name = "mlfingerd"

end
