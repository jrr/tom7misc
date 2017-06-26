
structure Hsock :> HSOCK =
struct

  type address = string
    
  type connection = TextIO.instream * TextIO.outstream

  exception Error

  fun address x = x

  fun connect a p = 
    (MLton.Socket.connect (a, p))
    handle _ => raise Error

  fun send (c : connection) s = 
      let in
	  TextIO.output (#2 c, s);
	  TextIO.flushOut (#2 c)
      end
    handle _ => raise Error


  fun ipall s =
      if TextIO.endOfStream s then
	  ""
      else let val v = TextIO.inputN (s, 4096)
	   in
(*	       print ("Got [" ^ v ^ "]\n"); *)
	       v ^ ipall s
	   end handle _ => ""

  val recvall : connection -> string = (ipall o #1)
    handle _ => raise Error
    
  fun close (ii,oo) = 
    let in
      MLton.Socket.shutdownRead ii;
      MLton.Socket.shutdownWrite oo;
      TextIO.closeIn ii;
      TextIO.closeOut oo
    end handle _ => ()

end
