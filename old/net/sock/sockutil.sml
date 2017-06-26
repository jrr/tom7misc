
structure SockUtil =
struct

    (* XXX perhaps implement as casts for MLton? *)

    exception Closed


    fun vtos v = CharVector.tabulate (Word8Vector.length v, fn x => chr (Word8.toInt (Word8Vector.sub (v, x))))
    fun stov v = Word8Vector.tabulate (size v, fn x => Word8.fromInt (ord (CharVector.sub (v, x))))

    (* send an entire vector *)
    fun sendveca sock v = 
        let
            val s = Word8Vector.length v
            fun go b =
                let val n = UnixSocket.sendVec (sock, {buf= v, i=b, sz=NONE})
                in
                    if n + b = s then ()
                    else go (b + n)
                end
        in
            go 0
        end

    (* receive exactly n bytes *)
    fun recva sock n =
        let
            fun go 0 = []
              | go m =
                let val v = UnixSocket.recvVec (sock, m)
                in 
                    Word8Vector.length v > 0 orelse raise Closed;
                    v :: (go (m - Word8Vector.length v))
                end
        in
            Word8Vector.concat (go n)
        end

    (* for situations where we need to read up to a newline, but not pull
       anything else out of the buffer. Returns a string since we're
       probably doing ASCII. Not very efficient.. *)
    fun readline sock =
        let
            fun oc () = 
                case chr (Word8.toInt (Word8Vector.sub (recva sock 1, 0))) of
                    #"\n" => nil
                  | c => c :: oc ()
        in
            implode (oc ())
        end

end