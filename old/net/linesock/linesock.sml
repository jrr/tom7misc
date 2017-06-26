
(* XXX maxline not obeyed *)
functor LineSockFn(val maxline : int
		   structure K : KEY) :> LINESOCK where type key = K.key =
struct
    type key = K.key

    structure US = UnixSocket

    datatype state = OPEN | CLOSED | REPORT

    type 'a lsock = string list ref * string ref * state ref * 
	('a, US.active US.stream) US.sock * US.sock_desc * key

    fun filter f = implode o (List.filter f) o explode

    fun lsock (inc, s, k) =
	let 
	    val inc = filter (fn c => c <> #"\r") inc
	    (* ok. fields can't return nil *)
	    val (rest::lines) = rev (String.fields (fn c => c = #"\n") inc)
	in
	    (ref (rev lines), ref rest, ref OPEN, s, US.sockDesc s, k)
	end

    fun getkey (_, _, _, _, _, k) = k

    datatype 'a event =
	Closed of key
      | Line of key * string
      | Accept of ('a, UnixSocket.active UnixSocket.stream) UnixSocket.sock * 'a UnixSocket.sock_addr
      | Timeout

    fun exevent nil = NONE
      | exevent ((r as ref (h::t), _, _, _, _, k)::_) = SOME (r := t; Line (k, h))
      | exevent ((ref nil, _, r as ref REPORT, _, _, k)::_) = SOME (r := CLOSED; Closed k)
      | exevent ((ref nil, _, _, _, _, _)::t) = exevent t

    fun wait (server, socks, timeout) =
	let val socks = List.filter (fn (_, _, ref b, _, _, _) => b <> CLOSED) socks
	in
	    case exevent socks of
		SOME e => e
	      | NONE => 
		    let
			val sss = map (fn (_, _, _, _, sd, _) => sd) socks
			val timeout = Option.map (fn (s, us) => 
						  Time.+(Time.fromMicroseconds (Int32.fromInt us),
							 Time.fromSeconds (Int32.fromInt s))) timeout
			fun inset l s = List.exists (fn c => US.sameDesc(c, s)) l

			(* find newlines in rc. Tack onto the end of st, update sofar *)
			fun splits (st as ref strs,
				    sf as ref sofar,
				    rc) =
			    let
				val rc = filter (fn c => c <> #"\r") rc
				(* fields can't be nil *)
				val res = String.fields (fn c => c = #"\n") rc

				(* XXX: needlessly inefficient - use doubly-linked lists? *)
				fun push (r as ref l) n = r := (l @ [n])

				(* fields cannot return nil *)
				fun go [last] = 
				    let in
					(* no newline for the last one *)
					sf := !sf ^ last
				    end
				  | go (h::t) =
				    let in
					push st (!sf ^ h);
					sf := "";
					go t
				    end
			    in
				go res
			    end

			fun doreads rds exs (st as ref strs, sf as ref sofar, state, sok, sd, k) =
			    if inset exs sd then
				(* XXX... I don't know what exns come from, actually. *)
				state := REPORT
			    else if inset rds sd then
				(* ready for reading, or socket closed (if 0 bytes returned) *)
				let 
				    val rc = SockUtil.vtos (US.recvVec (sok, 1024))
				in
				    if size rc = 0 then
					state := REPORT
				    else
					splits (st, sf, rc)
				end
			    else ()
				    
		    in
			case server of
			    SOME svr => 
				let 
				    val svs = US.sockDesc svr
				    val {rds=rs, wrs=_, exs=es} = US.select {rds=svs::sss, wrs=nil, exs=sss, 
									     timeout=timeout}
				in
				    app (doreads rs es) socks;
				    if inset rs svs then
					(* always favor Accept events (because we cannot queue them) *)
					Accept (US.accept svr)
				    else case exevent socks of
					NONE => Timeout (* XXX should loop if not timed out yet *)
				      | SOME e => e
				end
			  | NONE =>
				let 
				    val {rds=rs, wrs=_, exs=es} = US.select {rds=sss, wrs=nil, exs=sss, 
									     timeout=timeout}
				in
				    app (doreads rs es) socks;
				    case exevent socks of
					NONE => Timeout (* XXX should loop if not timed out yet *)
				      | SOME e => e
				end
		    end
	end
end