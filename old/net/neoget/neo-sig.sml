
signature NEO =
sig

    (* a session is a cookie (and other info) for telling the
       site who you are. *)
    type session


    (* init a session with the cookie *)
    val session : string -> session
    val cookie : session -> string

end
