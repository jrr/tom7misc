
(* mlsshd, Copyright (c) 2001 Tom Murphy VII.
   
   Licensed for your use under the Gnu Public License, Version 2 or later.
   http://www.gnu.org/copyleft/gpl.html

*)

structure Server = ServerFn(SSH) :> SERVER


val _ = (case Params.docommandline () of
              nil => Server.run ()
            | _ => 
                let in
                  print "This program takes no arguments.\n";
                  print (Params.usage());
                  print "\n"
                end)
        handle Params.BadOption s => print s
	     | UnixSocket.SockError s => print ("Socket error: " ^ s ^ "\n")
	     | SSHProt.SSHProt s => print ("SSH protocol error: " ^ s ^ "\n")
