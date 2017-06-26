
structure Main =
struct
    
    fun tc x =
	(Type.typecheck (Subst.rename x))
	handle (ex as (Type.TypeError s)) =>
	    let in
		print "Type Error: ";
		print s;
		print "\n";
		raise ex
	    end

    fun run x =
      let
        val x = Subst.rename x
	    val t = (Type.typecheck x)
	    val pr = Eval.begin [x]
	in
	    Eval.run pr
	end
    handle (ex as (Type.TypeError s)) =>
	let in
	    print "Type Error: ";
	    print s;
	    print "\n";
	    raise ex
	end
	 | (ex as (Eval.Wrong)) => 
	let in
	    print "Eval Error\n";
	    raise ex
	end


end