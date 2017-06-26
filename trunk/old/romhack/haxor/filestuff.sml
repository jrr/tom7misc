
structure FileStuff :> FILESTUFF =
struct

  structure V = Word8Vector

  type bytes = Word8Vector.vector

  (* this doesn't work in SML/NJ on win32 due to a basis library bug *)

  fun readfileX f =
    let
      val inf = BinIO.openIn f
      val dat = BinIO.inputAll inf
    in
      BinIO.closeIn inf;
      dat
    end

  fun readfile f =
    let
      val inf = BinIO.openIn f
      fun rd vs =
        let val v = BinIO.input inf
        in case V.length v of
           0 => V.concat (rev vs)
         | _ => rd (v :: vs)
        end
      val dat = rd nil
    in
      BinIO.closeIn inf;
      dat
    end

  fun writefile f out =
    let
      val ouf = BinIO.openOut f
      val output = BinIO.output (ouf, out)
    in
      BinIO.closeOut ouf
    end

  fun setbyte f i b =
    let
      fun modify (v : V.vector,
                  i : int,
                  a) : V.vector =
        V.mapi (fn (ii, aa) => if ii = i then a else aa) (v, 0, NONE)

      val dat = readfile f

      val out = modify (dat, i, b)
    in
      writefile f out
    end

end