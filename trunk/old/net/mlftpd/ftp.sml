
(* Core of ML FTP server. (Passed to ServerFn to create the server.)

   Written by Tom 7 in June 2001. Distributed under the terms of the
   GPL; see the file COPYING for further information.
*)

structure FtpArg :> SERVERARG =
struct

    exception Exit
    exception Err of string
    
    structure FS = Posix.FileSys
    structure S = MLton.Socket

    val version = "1.71"

    (* XXX this is probably bogus *)
    val workaround_deadlock =
      Params.flag false
                   (SOME ("-workaround-deadlock", 
                          "Work around PORT deadlock in win2k CLI client (?)"))
                   "workaround_deadlock"

    val site =
      Params.param "localhost"
                   (SOME ("-site", "Hostname for this site (can be anything)"))
                   "site"

    val passwd_file = 
      Params.param "/etc/passwd" 
                   (SOME ("-passwd", "The passwd file")) 
                   "passwd_file"

    val group_file =
      Params.param "/etc/group"
                   (SOME ("-group", "The group file"))
                   "group_file"

    val admin_email =
      Params.param "root@localhost"
                   (SOME ("-admin",
                          "email address of administrator"))
                   "admin_email"

    val verbose = 
      Params.flag false
                  (SOME
                   ("-verbose",
                    "Announce everything that happens"))
                  "verbose"

    val server_port =
      Params.param "port"
                   (SOME ("-port",
                          "Port on which to listen for connections"))
                   "server_port"

    val localhost =
      Params.param "localhost"
                   (SOME ("-localhost",
                          "IP of local host (names not yet supported)"))
                   "localhost"

    val log_info =
        Params.flag true
                   (SOME ("-log-info",
                          "Log INFO level messages to the syslog"))
                   "log_info"

    val log_notice =
        Params.flag true
                   (SOME ("-log-notice",
                          "Log NOTICE level messages to the syslog"))
                   "log_notice"

    val lscompat =
        Params.flag true
                (SOME ("-lscompat",
                       "Support non-RFC but de facto LIST and NLST behavior"))
                "lscompat"

    fun beforeloop () = ()

    fun port () = Option.getOpt (Int.fromString(!server_port), 21)

    val localip = ref "0.0.0.0"

    (* compatibility with old basis *)
    fun old_inputline s =
        case TextIO.inputLine s of
            NONE => ""
          | SOME s => s

    fun notice s =
        let in
            if !verbose then
                print ("NOTICE: " ^ s ^ "\n")
            else ();
            if !log_notice then
                MLton.Syslog.log (MLton.Syslog.NOTICE, s)
            else ()
        end
            
    fun info s =
        let in
            if !verbose then
                print ("INFO: " ^ s ^ "\n")
            else ();
            if !log_info then
                MLton.Syslog.log (MLton.Syslog.INFO, s)
            else ()
        end

    fun init () =
      ignore
      let in
          MLton.Random.srand 
             (case MLton.Random.seed () of
                  SOME s => s
                | NONE => (* not a big deal; only used for store unique *)
                   (case MLton.Random.useed () of
                        SOME s => s
                      | NONE => 
                    Word.fromInt
                    (LargeInt.toInt
                     (LargeInt.mod((Time.toMicroseconds (Time.now ())),
                                   LargeInt.fromInt 0x3FFFFFFE))))); 

        (* XXX also try environment variable HOSTNAME *)
        (* XXX is there a better way to do this? *)
        if !localhost = "localhost" then
          let in
            localhost := StringUtil.filter (fn #"." => true
                                             | #"-" => true
                                             | c => Char.isAlphaNum c)
                            (StringUtil.readfile "/etc/HOSTNAME")
          end
        else ();

          (* if localhost isn't an IP address, resolve it and
             put it in the localip param. *)
          if Char.isDigit (CharVector.sub(!localhost, size (!localhost) -1))
            then localip := !localhost
          else
        case SockUtil.getipbyhost (!localhost) of
          NONE => print ("Couldn't resolve " ^ !localhost ^ 
                         " -- PASV will not work\n")
        | SOME b => 
            let 
              val a = SockUtil.ipstring b
            in
              if !verbose then print ("Resolved " ^ !localhost ^ 
                                      " to " ^ a ^ "\n")
              else ();
                localip := a
            end;

        Passwd.readdb (!passwd_file);
        Group.readdb  (!group_file)
      end

    fun timefmt t =
      let
        val d = Date.fromTimeLocal t
      in
        String.substring(Date.toString d, 4, 12)
      end
        
    fun isws (#" ") = true
      | isws (#"\r") = true
      | isws (#"\n") = true
      | isws (#"\t") = true
      | isws _ = false

    fun sleep n =
        Posix.Process.sleep (Time.fromSeconds n)

    val splitword = String.tokens isws 

    fun split s =
        case splitword s of
            nil => ("", "")
          | [cmd] => (StringUtil.ucase cmd, "")
          | a::b::rest => (StringUtil.ucase a, 
                           foldl (fn (e, r) => r ^ " " ^ e) b rest)

    fun cloop host port ins outs user uid gid name home =
        let
            val who = user ^ "/" ^ host

            fun say s = 
                let in
                    if !verbose then print (who ^ " <- " ^ s ^ "\n")
                    else ();
                    TextIO.output (outs, s ^ "\r\n");
                    TextIO.flushOut outs
                end

                
            fun pathto f = 
                FSUtil.dirplus (FS.getcwd ()) f

            (* switch to this user *)
            val _ = 
                if ProcUtil.becomeuser (user, uid, gid)
                then ()
                else
                    let in
                        say("530 Internal Error: Can't setuid");
                        raise Exit
                    end

            (* start in the user's home directory *)
            val _ = FS.chdir home
                
       val helpdata =
           [("USER", true, "USER <string> - use to log in"),
            ("CWD",  true, "CWD <dir> - change working directory"),
            ("PWD",  true, "PWD - print the current directory"),
            ("NOOP", true, "NOOP - do nothing"),
            ("CDUP", true, "CDUP - same as CWD .."),
            ("PASS", true, "PASS <string> - use to log in"),
            ("HELP", true, "HELP or HELP command - get minimal help"),
            ("RNTO", true, "RNTO <file> - rename a file (after RNFR)"),
            ("RNFR", true, "RNFR <file> - rename a file (before RNTO)"),
            ("DELE", true, "DELE <file> - delete a file"),
            ("ALLO", true, "ALLO <n> [ R <m> ] - allocate space"),
            ("STRU", true, "STRU F or STRU R or STRU P - change structure"),
            ("MODE", true, "MODE S or MODE B or MODE C - change mode"),
            ("TYPE", true, "TYPE I or TYPE A - change file transfer type"),
            ("SYST", true, "SYST - return server system type"),
            ("MKD",  true, "MKD <dir> - make a directory"),
            ("RMD",  true, "RMD <dir> - remove empty directory"),
            ("PASV", true, "PASV - set passive mode (recommended)"),
            ("RETR", true, "RETR <file> - retrieve a file"),
            ("QUIT", true, "QUIT - close the connection and log off"),
            ("STOR", true, "STOR <file> - store a file (destructively)"),
            ("PORT", true, "PORT h1,h2,h3,h4,p1,p2 - change data port"),
            ("NLST", true, "NLST or NLST <path> - list just files"),
            ("LIST", true, "LIST or LIST <path> - directory listing"),
            ("STOU", true, "STOU - store unique (new file)"),
            ("REST", true, "REST <bytes> - retry next STOR or RETR at point"),
            ("APPE", true, "APPE <file> - write to the end of a file"),

            (* de facto extensions, mapped on to the real RFC commands.
               why do some clients use these, still? *)
            ("XPWD", true, "alias for PWD"),
            ("XCWD", true, "alias for CWD"),                
            ("XMKD", true, "alias for MKD"),
            ("XCUP", true, "alias for CDUP"),
            ("XRMD", true, "alias for RMD"),

            (* should implement these *)
            ("STAT", false, ""),
            ("ABOR", false, ""),
            
            (* might implement these *)
            ("REIN", false, "REIN - reinitialize"),
            ("SITE", false, "SITE CMD <args> - site-specific commands"),
            
            (* what are these? *)
            ("MLFL", false, "?"),
            ("SIZE", false, "?"),
            ("MDTM", false, "?"),
            
            (* won't implement these *)
            ("MRCP", false, "?"),
            ("MRSQ", false, "?"),
            ("MSOM", false, "?"),
            ("MSND", false, "?"),
            ("MAIL", false, "?"),
            ("MSAM", false, "?"),
            ("SMNT", false, "SMNT <pathname> - structure mount"),
            ("ACCT", false, "ACCT <string> - change accounts")]

       fun remap ("XPWD",c) = ("PWD",c)
         | remap ("XCWD",c) = ("CWD",c)
         | remap ("XMKD",c) = ("MKD",c)
         | remap ("XCUP",c) = ("CDUP",c)
         | remap ("XRMD",c) = ("RMD",c)
         | remap (s,c) = (s,c)

            fun help "" =
                let 
                    fun seven (a::b::c::d::e::f::g::rest) = 
                              ["",a,b,c,d,e,f,g]::seven rest
                      | seven nil = nil
                      | seven l = ["" :: l @ List.tabulate 
                                   (7 - length l, fn _ => "")]
                    fun filt (s, false, _) = s ^ "*"
                      | filt (s, _, _) = s
                in
                    say "214-The following commands are recognized.";
                    say "    * Means unimplemented.";
                    say (String.translate (fn #"\n" => "\r\n" | c => str c)
                         (StringUtil.hardtable 
                          (List.tabulate (8, fn _ => 6))
                          (seven (map filt helpdata))));
                    say ("214 Direct comments to " ^ !admin_email ^ ".")
                end
              | help s =
                let
                    fun fnd ((n,b,d)::rest) =
                        if n = s then
                            if b then
                                say("214 " ^ d)
                            else say ("214 " ^ d ^ " (unimplemented)")
                        else fnd rest
                      | fnd nil =
                            say ("502 Unknown command " ^ s ^ ".")
                in
                    fnd helpdata
                end


            fun ascii_up #"\n" = "\r\n"
              | ascii_up c = str c

            fun ascii_dn #"\r" = ""
              | ascii_dn c = str c

            datatype transfer_type = ASCII | IMAGE

            datatype conn_type =
                Connected of TextIO.outstream * TextIO.instream
              | Listening of S.t * S.Port.t
              | NoConnection

            val closeout = Util.doboth S.shutdownWrite TextIO.closeOut
            val closein = Util.doboth S.shutdownRead TextIO.closeIn

            fun discardconn NoConnection = ()
              (* XXX - How do I discard server sockets? *)
              | discardconn (Listening (t, p)) = ()
              | discardconn (Connected (a, b)) =
                             let in
                               closeout a;
                               closein b
                             end handle _ => ()

            fun makedata f fail say conn =
              case conn of
                NoConnection => 
                  let in
                    say "425 Unable to build data connection.";
                    fail ()
                  end
              | Listening (t, p) => 
                  (let
                     val (_, _, ii, oo) = S.accept t
                   in
                     f (ii, oo)
                   end handle _ =>
                     let in
                       say "425 Unable to build data connection.";
                       fail ()
                     end)
              | Connected (a, b) => f (b, a)

            fun dash_al oo { dir, lnk, mode,
                             nlink, mtime, size, uid,
                             gid, name } =
                let 
                    fun uidname u =
                        let val ui = Word32.toIntX (Posix.ProcEnv.uidToWord u)
                        in case Passwd.lookupuid ui of
                            NONE => Int.toString ui
                          | SOME {login,...} => login
                        end
                    fun gidname g =
                        let val gi = Word32.toIntX (Posix.ProcEnv.gidToWord g)
                        in case Group.lookupgid gi of
                            NONE => Int.toString gi
                          | SOME {name, ...} => name
                        end
                in
                TextIO.output
                (oo,
                 StringUtil.delimit " "
                 (ListPair.map
                  (Util.uncurry StringUtil.pad)
                  ([9,~3,8,8,~7,12],
                   [(if dir then "d" else
                         if lnk then "l" else
                             "-") ^
                    FSUtil.modestring mode,
                    Int.toString nlink,
                    uidname uid,
                    gidname gid,
                    Int64.toString size,
                    timefmt mtime])) ^ 
                 " " ^ name ^ "\r\n");
                 TextIO.flushOut oo
                end
            (* handle -al compatibility with many clients,
               if the lscompat flag is set. *)
            fun parsels d =
                if !lscompat then
                    (case String.tokens (fn #" " => true | _ => false) d of
                         nil => (".", false)
                       | ["-al"] => (".", true)
                       | ["-al", dd] => (dd, true)
                       | _ => (d, false))
                else (if d = "" then (".", false)
                      else (d, false))
                
      fun c {rn, typ, conn, seek} =
          let
              val input = old_inputline ins
              val _ = 
                  if !verbose then
                      print (host ^ ": " ^ input ^ "\n")
                  else ()

              (* create a file, truncate to n bytes if it already exists. *)
              fun truncreate n f =
                  let
                      val tf = FS.openf (f,
                                         FS.O_RDWR,
                                         FS.O.flags[FS.O.noctty])
                  in
                      FS.ftruncate (tf, n);
                      Posix.IO.close tf
                  end handle _ => ()


              (* used by STOR and STOU and APPE *)
              (* Here I want to support uploading over files
                 with multiple links. (I used to just delete the
                 old file.) If the user wants to affect only this
                 link, he should DELE the file first. *)

              fun storfn startat f =
                  (let
                       val _ = truncreate startat f

                       val l = TextIO.openAppend f
                       fun fail () = TextIO.closeOut l
                       fun sendit (ii, oo) =
                           let 
                               val (ss, ff) =
                                   case typ of
                                       ASCII => ("ASCII",
                                                 SockUtil.sendfilefilt ascii_dn)
                                     | IMAGE => ("IMAGE",
                                                 SockUtil.sendfile)
                           in
                               say ("150 Opening " ^ ss ^
                                    " mode connection for " ^ f ^ ".");
                               info (who ^ " STORE " ^ pathto f ^ 
                                     " (begin)");
                               ff (l, ii);
                               let in
                                   closeout oo;
                                   closein ii
                               end handle _ => ();
                               TextIO.closeOut(l);
                               info (who ^ " store complete.");
                               say "226 Transfer complete."
                           end
                   in
                       makedata sendit fail say conn
                   end handle _ =>
                       say "550 Bad file name or access denied.")
          in
              (input <> "") orelse raise Exit;
              (case remap (split input) of
                   ("HELP", s) => help (StringUtil.ucase s)
                 | ("PWD", _) => say ("257 \"" ^ 
                                      FS.getcwd () ^ 
                                      "\" is current directory.")
                 | ("RETR", f) => 
                        (let
                           val st = FS.stat f
                           val len = FS.ST.size st

                           (* don't let them download directories and stuff *)
                           val _ = (FS.ST.isReg st) 
                               orelse raise Err "Not a regular file."
                           
                           val l = TextIO.openIn f


                           val _ = case seek of 
                               NONE => ()
                             | SOME n => FSUtil.skipi n l

                           fun fail () = TextIO.closeIn l
                           fun sendit (ii, oo) =
                             let 
                               val (ss, ff) =
                                 case typ of
                                   ASCII => ("ASCII", 
                                             SockUtil.sendfilefilt ascii_up)
                                 | IMAGE => ("IMAGE",
                                             SockUtil.sendfile)
                             in
                                 say ("150 Opening " ^ ss ^
                                      " mode connection for " ^ f ^
                                      " (" ^ Int64.toString len ^ " bytes).");
                                 info (who ^ " RETR " ^ pathto f ^ 
                                       " (begin)");
                                 ff (oo, l);
                                 closeout oo;
                                 closein ii;
                                 TextIO.closeIn(l);
                                 info (who ^ " retrieve complete.");
                                 say "226 Transfer complete."
                             end
                         in
                           makedata sendit fail say conn
                         end handle 
                               Err s => say ("550 " ^ s)
                             | FSUtil.Seek => 
                                   say "550 Previous REST too big for file."
                             | _ => say "550 File not found or access denied.")
                 | ("APPE", f) =>
                             (let 
                                  val s = (FSUtil.filesize64 f) handle _ => 0
                              in
                                  storfn s f
                              end)
                 | ("STOR", f) => 
                             (let in
                                  storfn (Int64.fromInt (getOpt (seek, 0))) f
                              end handle Err s => say ("550 " ^ s))
                 | ("STOU", _) =>
                             let
                                 val base = 
                                     StringUtil.harden (StringUtil.charspec
                                                        "-A-Za-z0-9") #"_" 32 
                                                         (user ^ "-" ^ host)
                                 fun finduniq () =
                                     let
                                         val file = base ^ "-" ^
                                             MLton.Random.alphaNumString 8
                                     in
                                         let in
                                             Posix.FileSys.stat file;
                                             finduniq ()
                                         end handle _ => storfn 0 file
                                     end 
                             in
                                 finduniq ()
                             end
                 | ("PORT", p) => 
                           (case String.tokens (fn #"," => true
                                                |  #" " => true
                                                |     _ => false) p of
                             [h1,h2,h3,h4,p1,p2] =>
                               (let
                                  val _ = discardconn conn
                                  val host = h1 ^ "." ^ h2 ^ "." ^ 
                                             h3 ^ "." ^ h4
                                  fun i s = Option.getOpt(Int.fromString s, 0)
                                  val port = i p1 * 256 + i p2
                                  val _ = if !verbose then 
                                      print 
                                      ("Trying to establish data connection with " ^
                                       host ^ ":" ^ Int.toString port ^ "...\n")
                                          else ()
                                  val _ = if !workaround_deadlock
                                          then say "200 PORT command successful (provisionally)"
                                          else ()
                                  val (ii, oo) = S.connect (host, port)
                                  val _ = if !verbose then 
                                          print ("Success.\n")
                                          else ()
                                in
                                  if not (!workaround_deadlock) 
                                  then say "200 PORT command successful"
                                  else ();
                                  c { rn=NONE, typ=typ, seek=NONE,
                                      conn=Connected(oo,ii) }
                                end handle _ =>
                                  say ("500 Can't connect to " ^ host ^
                                       ":" ^ Int.toString port ^ "."))
                           | _ => say "500 Unable to parse PORT command.")
                 | ("PASV", _) =>
                              let 
                                fun report port =
                                  say ("227 Entering Passive Mode (" ^
                                       String.map (fn #"." => #"," | c => c) 
                                          (!localip) ^ 
                                       "," ^ Int.toString (port div 256) ^
                                       "," ^ Int.toString (port mod 256) ^
                                       ")")
                              in
                                case conn of
                                  Listening (ss, pp) => report pp
                                     | _ => 
                                    let 
                                      val (port, sock) = S.listen ()
                                    in
                                      discardconn conn;
                                      report port;
                                      c {rn=NONE, typ=typ, seek=NONE,
                                         conn=Listening(sock, port)}
                                    end handle _ =>
                                      say "550 Can't bind any socket!"
                              end
                 | ("NLST", dir') => 
                       let 
                         val (dir, full) = parsels dir'
                         fun fail () = ()
                         fun sendit (ii, oo) =
                           let
                               (* if lscompat is set, full might tell
                                  us to do a full listing anyway
                                  (NLST -al), so act like LIST. *)
                               val wrt = if full
                                         then dash_al oo
                                         else 
                                             (fn { dir, lnk, mode,
                                                   nlink, mtime, size, uid,
                                                   gid, name } =>
                                                let in
                                              TextIO.output 
                                              (oo, name ^ "\r\n");
                                                TextIO.flushOut oo
                                                end)
                           in
                             say ("150 Opening ASCII mode " ^
                                  "connection for /bin/ls.");

                             let in
                               if !lscompat then
                                 FSUtil.ls wrt dir
                               else FSUtil.dirapp wrt dir;
                               say "226 Transfer complete."
                             end handle _ => 
                               say "500 No such directory or no permission.";

                             let in
                                 closeout oo;
                                 closein ii
                             end handle e => ()

                           end handle _ =>
                               say "500 NLST: error."
                       in
                           makedata sendit fail say conn
                       end
                     
                 | ("LIST", dir') => 
                       let 
                           val (dir, _) = parsels dir'
                         fun fail () = ()
                         fun sendit (ii, oo) =
                           let
                               val wrt = dash_al oo
                           in
                             say ("150 Opening ASCII mode " ^
                                  "connection for /bin/ls.");

                             let in
                               (if !lscompat then FSUtil.ls
                                else FSUtil.dirapp) wrt dir;
                               say "226 Transfer complete."
                             end handle _ => 
                               say "500 No such directory or no permission.";

                             let in
                                 closeout oo;
                                 closein ii
                             end handle e => ()

                           end handle _ =>
                               say "500 LIST: error."
                       in
                           makedata sendit fail say conn
                       end
                 | ("CWD", dir) => 
                       (let in
                            FS.chdir dir;
                            say "250 CWD command successful."
                        end handle _ => 
                            say ("550 " ^ dir ^ 
                                 ": No such directory or no permission."))
                 | ("REST", ns) => 
                       (let 
                            val n = Option.getOpt(Int.fromString ns, 0)
                        in
                            say ("350 Restarting at " ^ Int.toString n ^ 
                                 " bytes. Next: RETR or STOR.");
                            c {rn=NONE, typ=typ, conn=conn, 
                               seek=SOME n}
                        end)
                 | ("DELE", f) =>
                       (let in
                            FS.unlink f;
                            info (who ^ " DELE " ^ pathto f);
                            say "250 DELE command successful."
                        end handle _ => 
                            say ("550 " ^ f ^ 
                                 ": No such file or no permission."))
                 | ("MKD", d) => 
                       (let in
                            (* create with rwx for user *)
                            FS.mkdir (d, FS.S.flags [FS.S.irwxu]);
                            info (who ^ " MKD " ^ pathto d);
                            say "250 MKD command successful."
                        end handle _ =>
                            say ("550 Failed to create directory " ^ d ^ "."))
                 | ("RMD", d) => 
                       (let in
                            FS.rmdir d;
                            info (who ^ " RMD " ^ pathto d);
                            say "250 RMD command successful."
                        end handle _ =>
                            say ("550 Failed to remove directory " ^ d ^ "."))
                 | ("RNTO", f) => 
                     (case rn of
                       NONE => say "503 Bad sequence of commands. (RNFR first)"
                     | SOME s =>
                           let in
                               FS.rename {new=f, old=s};
                               info (who ^ " REN " ^ pathto s ^ " to " ^ 
                                     pathto f);
                               say "250 RNTO command successful."
                           end 
                       handle _ => say "550 rename: Operation not permitted.")
                 | ("RNFR", f) => 
                       (let in
                            FS.stat f;
                            say 
                              "350 File exists, ready for destination name";
                              c {rn=SOME f, typ=typ, conn=conn, seek=NONE}
                        end handle _ => say ("550 " ^ f ^ 
                                             ": No such file or directory."))
                 | ("TYPE", t) => 
                      (case StringUtil.ucase t of
                           "A" => let in
                                      say "200 Type set to A.";
                                      c {rn=NONE, typ=ASCII, conn=conn, 
                                         seek=NONE}
                                  end
                         | "I" => let in
                                      say "200 Type set to I.";
                                      c {rn=NONE, typ=IMAGE, conn=conn,
                                         seek=NONE}
                                  end
                         |  t  => say ("504 Type " ^ t ^ " not implemented."))
                 | ("SYST", _) => say "215 UNIX Type: L8"
                 | ("QUIT", _) =>
                         let in
                           info (who ^ " quit gracefully.");
                           say ("221-Thank you for using ML FTP at " ^
                                !localhost ^ ".");
                           say "221 Bye-bye!";
                           raise Exit
                         end
                 | ("ALLO", _) => say "202 ALLO command always successful."
                 | ("NOOP", _) => say "200 NOOP command successful."
                 | ("USER", _) => say "530 Already logged in."
                 | ("PASS", _) => say "530 Already logged in."

                 (* STRUs are largely obsolete *)
                 | ("STRU", c) => 
                       (case StringUtil.ucase c of
                            "F" => say "200 STRU command successful."
                          | _   => say "504 Unimplemented STRU type.")

                 (* MODEs too. *)
                 | ("MODE", c) => 
                       (case StringUtil.ucase c of
                            "S" => say "200 MODE command successful."
                          | _   => say "504 Unimplemented transfer MODE.")

                 (* Can't fail.. *)
                 | ("CDUP", _) => 
                       let in
                           FS.chdir "..";
                           say "250 CWD command successful."
                       end
                 | (cmd, _) => 
                       let fun fnd ((s, false, _)::r) =
                           if s = cmd then
                               say("532 '" ^ cmd ^ "': not implemented.")
                           else fnd r
                             | fnd (h::t) = fnd t
                             | fnd nil = 
                               say("500 '" ^ cmd ^ 
                                   "': command not understood.")
                       in fnd helpdata
                       end);
              c {rn = NONE, typ=typ, conn = conn, seek=NONE}
          end

        in
            c {rn = NONE, typ = ASCII, conn = NoConnection, seek=NONE}
        end handle Exit => ()


    fun process_connection (host, _, port, ins, outs) =
        let
            fun say s = 
                let in
                    TextIO.output (outs, s ^ "\r\n");
                    TextIO.flushOut outs
                end

            val _ = say ("220 " ^ !site ^ " FTP server (ML ftpd version " ^ 
                                   version ^ ").")

            (* untrusted users should not be able to write arbitrary
               characters to syslog. *)
            val prot = StringUtil.harden 
                           (StringUtil.inlist [#" ", #".", #"-", #"_"]) #"+" 256

            fun getuser () =
                case split (old_inputline ins) of
                    ("USER", uname) => 
                        let in
                            say("331 Password required for " ^ uname ^ ".");
                            getpass uname
                        end
                  | _ => let in
                            say("530 Please login with USER and PASS.");
                            getuser ()
                         end

            and getpass user =
                case split (old_inputline ins) of
                    ("PASS", pass) =>
                        (case Auth.check (user,pass) of
                             Util.B reason => 
                                 let in
                                     notice ("Login for user '" ^ 
                                             prot user ^ "' failed for " ^
                                             prot host ^ ": " ^ reason ^ ".");
                                     sleep(1);
                                     say("530 Login incorrect.");
                                     getuser ()
                                 end
                           | Util.A {uid,name,home,gid} => 
                                 let in
                                     notice ("Login as '" ^ prot user ^
                                             "' from " ^ prot host ^ ".");
                                     say("230 User " ^ user ^ " (" 
                                         ^ name ^ 
                                         ") logged in.");
                                     
                                     cloop host port ins outs 
                                     user uid gid name home
                                 end)
                  | _ => let in
                             say("331 Password required for " ^ user ^ ".");
                             getpass user
                         end

        in
            getuser ()
        end

    (* for syslog *)
    val name = "mlftpd"

end
