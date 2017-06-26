
(* stubs for type-checking in SML/NJ.
   this should be in sources.cm, but not mlftpd.cm.
 *)

signature MLTON_SOCKET = 
sig
      structure Address:
         sig
            type t = word
         end
      structure Host:
         sig
            type t = {name: string}

            val getByAddress: Address.t -> t option
            val getByName: string -> t option
         end
      structure Port:
         sig
            type t = int
         end

      type t

      val accept: t -> Address.t * Port.t * TextIO.instream * TextIO.outstream
      val connect: string * Port.t -> TextIO.instream * TextIO.outstream
      val listen: unit -> Port.t * t
      val listenAt: Port.t -> t

      val shutdownRead: TextIO.instream -> unit
      val shutdownWrite: TextIO.outstream -> unit
end

signature MLTON_SYSLOG =
sig

    type openflag
	
    val CONS     : openflag
    val NDELAY   : openflag
    val PERROR   : openflag
    val PID      : openflag

    type facility

    val AUTHPRIV : facility
    val CRON     : facility
    val DAEMON   : facility
    val KERN     : facility
    val LOCAL0   : facility
    val LOCAL1   : facility
    val LOCAL2   : facility
    val LOCAL3   : facility
    val LOCAL4   : facility
    val LOCAL5   : facility
    val LOCAL6   : facility
    val LOCAL7   : facility
    val LPR      : facility
    val MAIL     : facility
    val NEWS     : facility
    val SYSLOG   : facility
    val USER     : facility
    val UUCP     : facility

    type loglevel

    val EMERG    : loglevel
    val ALERT    : loglevel
    val CRIT     : loglevel
    val ERR      : loglevel
    val WARNING  : loglevel
    val NOTICE   : loglevel
    val INFO     : loglevel
    val DEBUG    : loglevel

    (* calling this is optional but recommended *)
    val openlog : string * openflag list * facility -> unit

    (* also optional *)
    val closelog : unit -> unit

    (* log a message at a particular loglevel. *)
    val log : loglevel * string -> unit

end


signature MLTON =
sig
    structure Socket : MLTON_SOCKET
    structure Syslog : MLTON_SYSLOG
    structure TextIO :
	sig
	    val outFd : TextIO.outstream -> Posix.FileSys.file_desc
	    val inFd  : TextIO.instream  -> Posix.FileSys.file_desc
	end
    structure Random :
      sig
        val alphaNumString: int -> string
        val rand: unit -> word
        val seed: unit -> word
        val srand: word -> unit
        val useed: unit -> word
      end
end

structure MLton :> MLTON =
struct

    exception Unimplemented

    structure TextIO =
	struct
	    fun outFd _ = raise Unimplemented
	    fun inFd _ = raise Unimplemented

	end

    structure Random =
    struct
      fun rand _ = 0w0
      fun seed _ = 0w0
      fun srand _ = ()
      fun useed _ = 0w0
      fun alphaNumString _ = raise Unimplemented
    end

    structure Socket : MLTON_SOCKET =
	struct
	   structure Address =
	       struct 
		   type t = word
	       end

	   structure Host =
	       struct
		   type t = { name : string }
		       
		   fun getByAddress a = NONE
		   fun getByName a = NONE
	       end
	   
	   structure Port =
	       struct
		   type t = int
	       end

	   type t = unit

	   fun accept t = raise Unimplemented
	   fun connect x = raise Unimplemented
	   fun listen () = raise Unimplemented
	   fun listenAt x = raise Unimplemented

	   fun shutdownRead x = raise Unimplemented
	   fun shutdownWrite x = raise Unimplemented
	end

    structure Syslog :> MLTON_SYSLOG =
	struct

    type openflag = unit
	
    val CONS     : openflag = ()
    val NDELAY   : openflag = ()
    val PERROR   : openflag = ()
    val PID      : openflag = ()

    type facility = unit

    val AUTHPRIV : facility = ()
    val CRON     : facility = ()
    val DAEMON   : facility = ()
    val KERN     : facility = ()
    val LOCAL0   : facility = ()
    val LOCAL1   : facility = ()
    val LOCAL2   : facility = ()
    val LOCAL3   : facility = ()
    val LOCAL4   : facility = ()
    val LOCAL5   : facility = ()
    val LOCAL6   : facility = ()
    val LOCAL7   : facility = ()
    val LPR      : facility = ()
    val MAIL     : facility = ()
    val NEWS     : facility = ()
    val SYSLOG   : facility = ()
    val USER     : facility = ()
    val UUCP     : facility = ()

    type loglevel = unit

    val EMERG    : loglevel = ()
    val ALERT    : loglevel = ()
    val CRIT     : loglevel = ()
    val ERR      : loglevel = ()
    val WARNING  : loglevel = ()
    val NOTICE   : loglevel = ()
    val INFO     : loglevel = ()
    val DEBUG    : loglevel = ()

    fun openlog _ = ()

    fun closelog _ = ()

    fun log _ = ()

	end
end
