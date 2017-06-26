
(* 
http://tinylogin.busybox.net/cgi-bin/cvsweb/tinylogin/su.c?rev=1.2&content-type=text/vnd.viewcvs-markup
http://tinylogin.busybox.net/cgi-bin/cvsweb/tinylogin/libbb/change_identity.c?rev=1.1&content-type=text/vnd.viewcvs-markup
http://tinylogin.busybox.net/cgi-bin/cvsweb/tinylogin/pwd_grp/initgroups.c?rev=1.6&content-type=text/vnd.viewcvs-markup
*)

structure ProcUtil =
struct

    fun becomeuser (name, uid, gid) =
        let 

            val memberships = Group.memberships name

(*
	val 	_ = print ("user " ^ name ^ " gid " ^ Int.toString gid ^
			   " uid " ^ Int.toString uid ^ "\n")
	    val _ = print "Member of ..."
	    val _ = app (fn {name, ...} =>
			 print ("   " ^ name ^ "\n")) memberships
*)

            val mems = map (fn {gid, ...} =>
                            Word32.fromInt gid) memberships

            val setgroups = _ffi "setgroups" : int * Word32.word array -> int ;

            val didset = 
                0 = setgroups (length memberships, Array.fromList mems)

            val uid = 
                Posix.ProcEnv.wordToUid (Word32.fromInt uid)
            val gid =
                Posix.ProcEnv.wordToGid (Word32.fromInt gid)
        in
            Posix.ProcEnv.setgid gid;
            Posix.ProcEnv.setuid uid;
            
            didset andalso
            Posix.ProcEnv.getuid () = uid andalso
            Posix.ProcEnv.getgid () = gid
        end

end
