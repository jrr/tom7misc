exception Error of string

open MLton.Socket
    
val data = "Hello there sailor\n"
    
val num = case CommandLine.arguments() of
    nil => 1
  | n::_ => valOf (Int.fromString n)

val (port, listener) = listen ()
    handle _ => 
	raise Error ("Can't listen...\n")

fun server () =
    let val (_, _, ins, outs) = accept listener
	fun s b = 
	    case TextIO.inputLine ins of
		"" => let in
		          Posix.Process.wait ();
			  print ("server processed " ^ Int.toString b ^
				 " bytes\n")
		      end
	      | i =>  let in 
		          TextIO.output(outs, i);
		          s (b + 19)
		      end
    in s 0
    end

fun client () =
    let
	val (ins, outs) = connect ("127.0.0.1", port)
	fun c 0 = let in
	              TextIO.closeOut outs;
		      TextIO.closeIn ins
		  end
	  | c n = let in
		      TextIO.output(outs, data);
		      TextIO.inputLine ins = data
		          orelse raise Error "Didn't receive the same data";
		      c (n - 1)
		  end
    in
	c num
    end

val _ = case Posix.Process.fork () of
    SOME pid => server ()
  | NONE => client ()
