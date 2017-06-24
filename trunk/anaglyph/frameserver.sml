val () = print "BYE\n"

structure FrameServer =
struct

  datatype client =
    C of { socket : (INetSock.inet, Socket.active Socket.stream) Socket.sock }

  val clients : client list ref = ref nil

  (* Returns the sockets that are ready for reading.
     For the server, this means it can accept a new connection. *)
  fun readable server =
    let
      val { rds, exs = _, wrs = _ } =
        Socket.select { rds = Socket.sockDesc server ::
                        map (fn C { socket, ... } => Socket.sockDesc socket) (!clients),
                        wrs = [], exs = [], timeout = NONE }

      val server_ready = ref false
      val clients_ready = ref nil
    in
      app
      (fn desc =>
       if Socket.sameDesc (Socket.sockDesc server, desc)
       then server_ready := true
       else
         case List.find (fn (C { socket, ... }) =>
                         Socket.sameDesc (Socket.sockDesc socket, desc)) (!clients) of
           NONE => () (* warning? *)
         | SOME client => clients_ready := client :: !clients_ready) rds;
      { server_ready = !server_ready, clients_ready = !clients_ready }
    end

  fun loop server =
    let
      val { server_ready, clients_ready } = readable server
    in
      if server_ready
      then
        let val (sock, _) = Socket.accept server
        in
          print ("Got connection.\n");
          clients := C { socket = sock } :: !clients
        end
      else ();
      (* XXX process clients... *)
      loop server
    end

  fun go () =
    let
      val sock = INetSock.TCP.socket()
      val port = 8000
    in
      Socket.Ctl.setREUSEADDR (sock, true);
      Socket.bind(sock, INetSock.any port);
      Socket.listen(sock, 5);
      print ("Listening on port " ^ (Int.toString port) ^ "...\n");
      loop sock
    end
end


val () = FrameServer.go ()
