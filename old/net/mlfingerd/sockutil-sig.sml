
signature SOCKUTIL =
sig

  exception SockUtil of string

  (* word to "128.2.1.2" *)
  val ipstring : MLton.Socket.Address.t -> string

  (* DNS lookup on a host *)
  val getipbyhost : string -> MLton.Socket.Address.t option

  (* copies the outstream (until EOF) to the instream *)
  val sendfile : TextIO.outstream * TextIO.instream -> unit

  (* as sendfile, but applies the function like String.translate *)
  val sendfilefilt : (char -> string) -> 
                 TextIO.outstream * TextIO.instream -> unit

  (* get the name of a connected Socket.
     On a non-socket outstream, raises SockUtil. *)
                    
  val getpeername : TextIO.outstream -> MLton.Socket.Address.t option
      
  (* can have exceptions on both in and out streams with select. *)
  datatype iostream = 
      IN of TextIO.instream
    | OUT of TextIO.outstream

(*
  (* select (readfds, writefds, exceptfds, timeout_sec, timeout_usec)

   See the man page for "select(2)" for more info.

   *)
  val select : TextIO.instream list * TextIO.outstream list * iostream list * int * int ->
               TextIO.instream list * TextIO.outstream list * iostream list

  (* readline (socket, sec, usec) 

     read a line from a socket, with a timeout.
   *)
  val readline : TextIO.instream * int * int -> string option
*)
    
end
