
structure Main =
struct

    exception Unimplemented

    fun int2bytes i =
	Word8Vector.fromList (map Word8.fromInt [(i div (65536*256)) mod 256,
						 (i div 65536) mod 256,
						 (i div 256) mod 256,
						 i mod 256])

    fun bytes2int (v : BinIO.vector) =
	Word8Vector.foldl (fn (a, b) => ((b * 256) + Word8.toInt a)) 0 v

    fun loadtex filename =
	let
	    val file = BinIO.openIn filename

	    val (width,height) = (bytes2int (BinIO.inputN (file, 4)),
				  bytes2int (BinIO.inputN (file, 4)))

	    val _ = print ("texture " ^ filename ^ ": " ^
			   Int.toString height ^ "x" ^
			   Int.toString width ^ "\n")		
	    val g = (fn _ => (Real.fromInt 
			      (Word8.toInt (getOpt(BinIO.input1 file, 
						   0w0)))
			      / 255.0))
	in
	    (Array2.tabulate Array2.RowMajor
	    (height, width*3, (fn _ => (g (), g(), g()))))
	      before (BinIO.closeIn file)
	end

    fun saveraw image filename =
	let
	    val file = BinIO.openOut filename
		
	    val (height, width) = Array2.dimensions image
		
	    val _ = (BinIO.output (file, int2bytes (width div 3)))
	    val _ = (BinIO.output (file, int2bytes height))

	    val _ = Array2.app
		Array2.RowMajor
		(fn byte => 
		 (BinIO.output1 (file, byte)))
		image
		
	    val _ = BinIO.closeOut file
		
	in
	    print ("saved : " ^ filename ^ "\n")
	end

    fun readgeom filename =
	let
	    val infile = TextIO.openIn filename
		
	    fun primfroms s =
		let
		    val ss = String.tokens (fn a => a = #" ") s
			
		    fun pfh ("T"::
			     x1::y1::z1::
			     x2::y2::z2::
			     x3::y3::z3::mat) = raise Unimplemented
		in
		    pfh ss
		end


	    (* each line corresponds to a primitive *)
	    fun rl () = case TextIO.inputLine infile of
		"" => nil
	      | s  => ((primfroms s) :: (rl ()))
		    
	    val prims = rl ()
		

	in
	    raise Unimplemented
	end

end