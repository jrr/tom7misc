
signature SERVERARG =
sig

  (* port on which this server listens *)
  val port : unit -> int

  (* once I've opened the port, but before I start accepting
     connections, run this. (to drop privileges, for instance) *)
  val beforeloop : unit -> unit

  (* process_communication 
           state hostname hostaddr port instream outstream 
     
     called once for each connection.

     must close the socket (streams) when done.
     *)
  val process_connection : string * INetSock.inet UnixSocket.sock_addr * int *
                           (INetSock.inet, UnixSocket.active UnixSocket.stream) UnixSocket.sock
                           -> unit

  val init : unit -> unit

  (* short name of daemon (for logging) *)
  val name : string

end
