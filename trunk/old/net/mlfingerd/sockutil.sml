
(* Designed to be used with MLton only. 
   (In particular, assumes Word.word is 32 bits)
*)

structure SockUtil :> SOCKUTIL =
struct

  (* workaround for andb bug in MLton 20010706 *)
  fun mkbyte w = Word.mod (w, 0w256)

  exception SockUtil of string

  val dotnumeric = StringUtil.charspec ".0-9"

  fun getipbyhost s = 
      if StringUtil.all dotnumeric s then
	  (* IP address *)
	  case String.fields (StringUtil.ischar #".") s of
	      [a, b, c, d] =>
		  (let
		       fun oct n = 
                         Word.fromInt(let val v = valOf (Int.fromString n)
                                      in if v > 255 then raise SockUtil ""
                                         else v
                                      end)
		   in
		       SOME
		       (Word.<< (oct a, 0w24) +
			Word.<< (oct b, 0w16) +
			Word.<< (oct c, 0w8) +
			oct d)
		   end handle _ => NONE)
	    | _ => NONE (* bad IP address? *)
      else (* call DNS primitive *)
	  NONE
	  

  fun ipstring addr = 
      let
	  val a = mkbyte(Word.>> (addr, 0w24))
	  val b = mkbyte(Word.>> (addr, 0w16))
	  val c = mkbyte(Word.>> (addr, 0w8))
	  val d = mkbyte addr
      in
	  Int.toString (Word.toInt a) ^ "." ^
	  Int.toString (Word.toInt b) ^ "." ^
	  Int.toString (Word.toInt c) ^ "." ^
	  Int.toString (Word.toInt d)
      end

  fun getpeername os =
    let
      val fd = Posix.FileSys.fdToWord (MLton.TextIO.outFd os)
      val a = SockPrim.getpeername fd
    in
      SOME a
    end handle SockPrim.SockPrim s => NONE

  (* XXX should use C sendfile for best performance, perhaps? *)
  fun sendfile (oo, ii) =
      let in
        while not (TextIO.endOfStream ii) do
          TextIO.output (oo, TextIO.inputN (ii, 4096));
        TextIO.flushOut oo
      end

  fun sendfilefilt f (oo, ii) =
      let in
        while not (TextIO.endOfStream ii) do
          TextIO.output (oo, String.translate f (TextIO.inputN (ii, 4096)));
        TextIO.flushOut oo
      end

  datatype iostream = 
      IN of TextIO.instream
    | OUT of TextIO.outstream

(*
  fun select (ins, outs, exs, sec, usec) =
      let
	  fun ff f x = ((Posix.FileSys.fdToWord (f x)), x)
	  val ins = map (ff MLton.TextIO.inFd) ins
	  val outs = map (ff MLton.TextIO.outFd) outs
	  fun getfds (arg as (IN i)) = (Posix.FileSys.fdToWord 
                                        (MLton.TextIO.inFd i), arg)
	    | getfds (arg as (OUT t)) = (Posix.FileSys.fdToWord 
                                         (MLton.TextIO.outFd t), arg)
	  val exs = map getfds exs

	  val (ri, ro, re) = SockPrim.selectprim (map #1 ins, 
                                                  map #1 outs, 
                                                  map #1 exs, 
                                                  sec, usec);

      in
	  (map (valOf o (ListUtil.Alist.find op= ins)) ri,
	   map (valOf o (ListUtil.Alist.find op= outs)) ro,
	   map (valOf o (ListUtil.Alist.find op= exs)) re)
      end
*)
  (* FIXME - not yet implemented correctly.
     If the client sends one byte, and then blocks, this will block.
  *)
(*
  fun readline (socket, sec, usec) =
      case select ([socket], nil, [IN socket], sec, usec) of
	  (nil, nil, nil) => NONE
	| _ => SOME (TextIO.inputLine socket)
*)
end
