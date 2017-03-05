structure BenchBytes =
struct

  fun main bytefiles =
    let
      fun one f =
        let val contents = StringUtil.readfile f
        in print (f ^ ": " ^ contents)
        end
    in
      app one bytefiles
    end

end

val () = Params.main
  "Usage:\n  benchbytes test1.bytes test2.bytes ..."
  BenchBytes.main
