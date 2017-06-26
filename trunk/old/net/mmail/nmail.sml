
(* The sequel to mmail *)

structure Nmail =
struct

    val version = "1.0"

    exception Nmail of string

    val from = Params.param "nobody@localhost" 
	                    (SOME("-from",
			 	  "Sets the 'from' address on emails"))
	                    "from"

    val subject = Params.param "No Subject"
	                       (SOME("-subject",
				     "Sets the subject of email")) "subject"

    val pid = Posix.Process.pidToWord (Posix.ProcEnv.getpid ())

    fun mkletter templ =
        let 
	    val t = StringUtil.readfile templ
	    val begin::tl = String.fields (StringUtil.ischar #"{") t

	    fun ret f name unsubkey id email =
		let
		    
		    fun proc nil = ()
		      | proc (h::tt) =
			(* h is some sort of command, split it: *)
			case String.fields (StringUtil.ischar #"}") h of
			    [cmd, rest] =>
				let in
				    (case cmd of
					 "name" => TextIO.output
					     (f, 
					      CharVector.map
					      (fn #"_" => #" "
					        | c => c) name)
				       | "id" => TextIO.output(f, id)
				       | "unsubkey" => TextIO.output(f, 
								     unsubkey)
				       | "email" => TextIO.output(f, email)
				       | _ => raise 
					     Nmail ("unknown command " ^ 
						    cmd));
				    TextIO.output(f, rest);
				    proc tt
				end
			  | _ => raise 
				Nmail "bad template file (mismatched {})!"
		in
		    TextIO.output(f, begin);
		    proc tl
		end
	in ret
	end

    fun loop mkl n =

	case TextIO.inputLine TextIO.stdIn of
	    "" => print ("Done. Sent " ^ Int.toString (n - 1) ^ " messages.\n")
	  | l => 
	    case String.fields (StringUtil.ischar #" ") 
		   (String.substring (l, 0, size l - 1)) of
		[name, email, id, unsubkey] =>
		    let
			val _ = Option.isSome (StringUtil.find "@" email)
			          orelse raise Nmail ("bad email " ^ email)
			val fname = name ^ "." ^ Word32.toString pid ^
			            ".tmp"
			val f = TextIO.openOut fname
			val cmd = ("sendmail \"-f" ^ !from ^ "\" \"" ^ 
				   email ^ "\" < " ^ fname)
		    in
			TextIO.output(f, 
				      "Subject: " ^ !subject ^ "\n" ^
				      "To: " ^ email ^ "\n" ^
				      "X-Mailer: nmail " ^ version ^ "\n" ^
				      "\n");
			mkl f name unsubkey id email;
			TextIO.closeOut f;
			print ("[" ^ Int.toString n ^ "] " ^ cmd ^ "\n");
			OS.Process.system cmd = 0
				   orelse raise Nmail "sendmail failed!";
			Posix.FileSys.unlink fname;
			loop mkl (n + 1)
		    end
	      | _ => raise Nmail ("bad line in template: " ^ l)

    fun main () =
	(case Params.docommandline () of
	     [templ] => loop (mkletter templ) 1
	   | _ => 
		 let in
		     print "Must supply a template file.\n";
		     print (Params.usage());
		     print "\n"
		 end)
	     handle Params.BadOption s => print (s ^ "\n")
		  | Nmail s => print (s ^ "\n")
end

val _ = Nmail.main ()
