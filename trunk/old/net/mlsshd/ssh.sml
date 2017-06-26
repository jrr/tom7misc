
structure SSH :> SERVERARG =
struct

    open SSHProt

    val version = "0.01"
    (* no joking around here; this is actually used by the
       client *)
    val sshversion = "1.5"

    val server_port =
      Params.param "port"
                   (SOME ("-port",
                          "Port on which to listen for connections"))
                   "server_port"

    val verbose = 
        Params.flag false
                  (SOME
                   ("-verbose",
                    "Announce everything that happens"))
                  "verbose"

    fun init () = ()

    val SBITS = 257
    val HBITS = 513

    (* pretty bogus *)
    val ((hn, he), (_, hd)) = RSA.keygen (fn _ => "") HBITS
    val ((sn, se), (_, sd)) = RSA.keygen (fn _ => "") SBITS
        
    fun beforeloop () = ()

    fun port () = Params.asint 9999 server_port


    fun ntohexl n =
      if N.> (n, Number.n0) 
      then
         let 
           val n' = N.div (n, N.fromInt 16)
           val d  = N.mod (n, N.fromInt 16)
         in
           String.sub("0123456789ABCDEF", N.toInt d) ::
           ntohexl n'
         end
      else [#"0"]
        
    fun ntohex n = implode (rev (ntohexl n))

    fun ssh_rsadec (k : N.int) (n : N.int) (msg : N.int) : 
           CharVector.vector =
        let
          val _ = print ("[RSADEC] before: " ^ ntohex msg ^ "\n")
          val decoded = ntobytes (RSA.crypt k n msg)
          val _ = print ("[RSADEC] decrypted:\n" ^
                         StringUtil.hexdump decoded ^ "\n")

          val chunk as {char, vec, ...} = 
            Reader.fromvec (ntobytes (RSA.crypt k n msg))
        in
            (char () = #"\002") orelse 
            raise SSHProt "decrypted RSA message doesn't begin with 2";
                
            (* consume random padding until 0 *)
            while char () <> #"\000" do ();

            Reader.rest chunk
        end

    fun makenum bytes =
      let
        fun mknum (r as {char, ...}) acc =
          if Reader.eof r then acc
          else mknum r (N.+(N.*(acc, n256), N.fromInt(ord (char()))))
      in
        mknum (Reader.fromvec bytes) Number.n0
      end


    (* returns a pair of functions, one for encrypting
       and one for decrypting. Each takes a CharVector.vector
       that's a multiple of 8 bytes, and returns a vector of
       the same length 
       
       because these ciphers operate in CBC mode, the functions
       carry imperative state! *)

    (* (SSH) 3DES in CBC mode *)
    fun cipherfunc Des3 session =
      let
        val read = Reader.fromvec session

        fun getkey () =
          let
            val l = Reader.rbw32 read
            val r = Reader.rbw32 read
          in DES.key (l, r)
          end

        val k1 = getkey ()
        val k2 = getkey ()
        val k3 = getkey ()


        (* IVs for encryption *)
        val ev1 = ref (0w0 : Word32.word, 0w0 : Word32.word)
        val ev2 = ref (0w0 : Word32.word, 0w0 : Word32.word)
        val ev3 = ref (0w0 : Word32.word, 0w0 : Word32.word)

        (* IVs for decryption *)
        val dv1 = ref (0w0 : Word32.word, 0w0 : Word32.word)
        val dv2 = ref (0w0 : Word32.word, 0w0 : Word32.word)
        val dv3 = ref (0w0 : Word32.word, 0w0 : Word32.word)

        fun encrypt (bl, br) =
          let
            val _ = print ("encrypt this: " ^ Word32.toString bl ^
                           " / " ^ Word32.toString br ^ "\n")

            (* round one - encrypt *)
            val bl = Word32.xorb(bl, #1 (!ev1))
            val br = Word32.xorb(br, #2 (!ev1))
            val (bl, br) = DES.encrypt k1 (bl, br)
            val _ = ev1 := (bl, br)

            (* round two - decrypt *)
              
            val (pl, pr) = (bl, br)
            val (bl, br) = DES.decrypt k2 (bl, br)
            val bl = Word32.xorb(bl, #1 (!ev2))
            val br = Word32.xorb(br, #2 (!ev2))
            val _ = ev2 := (pl, pr)

            (* round two - encrypt *)
            val bl = Word32.xorb(bl, #1 (!ev3))
            val br = Word32.xorb(br, #2 (!ev3))
            val (bl, br) = DES.encrypt k3 (bl, br)
            val _ = ev3 := (bl, br)

            val _ = print ("  ... result: " ^ Word32.toString bl ^
                           " / " ^ Word32.toString br ^ "\n")
          in
            (bl, br)
          end

        (* in this direction, we XOR with the previous
           ciphertext after decryption *)
        fun decrypt (bl, br) =
          let
            (* round one - decrypt *)
            val (pl, pr) = (bl, br)
            val (bl, br) = DES.decrypt k3 (bl, br)
            val bl = Word32.xorb(bl, #1 (!dv1))
            val br = Word32.xorb(br, #2 (!dv1))
            val _ = dv1 := (pl, pr)

            (* round two - encrypt *)
            val bl = Word32.xorb(bl, #1 (!dv2))
            val br = Word32.xorb(br, #2 (!dv2))
            val (bl, br) = DES.encrypt k2 (bl, br)
            val _ = dv2 := (bl, br)

            (* round two - decrypt *)
            val (pl, pr) = (bl, br)
            val (bl, br) = DES.decrypt k1 (bl, br)
            val bl = Word32.xorb(bl, #1 (!dv3))
            val br = Word32.xorb(br, #2 (!dv3))
            val _ = dv3 := (pl, pr)
          in
            (bl, br)
          end

        fun several f start v =
            case CharVector.length v - start of
              0 => nil
            | n => if n < 8 
                   then raise SSHProt ("3DES: not multiple of 8")
                   else
                     let
                       val read as {seek, ...} = Reader.fromvec v
                       val _ = seek start
                       val l = Reader.rbw32 read
                       val r = Reader.rbw32 read
                       val (l, r) = f (l, r)
                     in
                       wtov l ^ wtov r :: several f (start + 8) v
                     end
      in
        { enc = CharVector.concat o several encrypt 0, 
          dec = CharVector.concat o several decrypt 0 }
      end
      | cipherfunc c _ = raise SSHProt ("cipher " ^ ciphertostring c ^
                                        " not supported")

    (* treats the tail of the string like all zeroes *)
    fun byte_xor s1 s2 =
      let
        fun bx nil l2 = l2
          | bx l1 nil = l1
          | bx (h1::t1) (h2::t2) =
          chr
          (Word8.toInt (Word8.xorb (Word8.fromInt (ord h1),
                                    Word8.fromInt (ord h2)))) ::
          bx t1 t2
      in
        implode (bx (explode s1) (explode s2))
      end

    
    (* start interactive mode.. actually, should be called
       after starting shell *)
    fun interactive sock (sc, rc) =
      let
      in
        send sock sc 
        (SS (Stdout "Hello welcome to my secure internet server\n\n"));

        send sock sc 
        (SS (Stdout "bye bye\n\n"));

        raise SSHProt "bye bye"
      end

    fun setup sock (sc, rc) =
      let 
        fun no () =
          let in
            send sock sc (SS Failure);
            setup sock (sc, rc)
          end
      in
        (* loop doing setup until we get into interactive mode *)
        case getpacket sock rc of
          (* XXX support these at some point in the future! *)
          RC (RequestCompression _) => no()
        | RC (RequestPTY _) => no ()
        | RC (RequestX11Forwarding _) => no ()
        | RC (RequestPortForwarding _) => no ()
        | RC (RequestAgentForwarding _) => no ()
        | RC (ExecShell) =>
            interactive sock (sc, rc)
        | _ => raise SSHProt "setup: unexpected client message"
      end

    (* authentication is first *)
    fun authenticate sock (cf as {enc, dec}) =
      let val sc = {cipher = enc, padzero = false}
          val rc = {cipher = dec, padzero = false}
      in
        case getpacket sock rc of
          RC (User user) =>
            let
              val _ = print ("Wants user " ^ user ^ "\n")
            in
              (* FIXME send failure and require password! *)
              send sock sc (SS Success);
              setup sock (sc, rc)
            end
        | _ => raise SSHProt "expected User message"
      end

    fun process_connection (host, addr, port, sock) =
        ignore
        let
            fun say s = SockUtil.sendveca sock (SockUtil.stov s)

            val _ = say ("SSH-" ^ sshversion ^ "-mlsshd-" ^ version ^ "\n")
            val clientversion = SockUtil.readline sock
            val _ = if !verbose 
                    then print ("Client: " ^ clientversion ^ "\n") 
                    else ()

            val plain = {cipher = fn x => x, padzero = true} 

            (* the random 'cookie' *)
            val nonce = CryptRand.vec 8
            val session_id = MD5.md5(ntobytes hn ^
                                     ntobytes sn ^
                                     nonce)
        in
            print "keys:\n";
            print ("he: " ^ IntInf.toString he ^ "\n");
            print ("hn: " ^ IntInf.toString hn ^ "\n");
            print "\n";
            print ("se: " ^ IntInf.toString se ^ "\n");
            print ("sn: " ^ IntInf.toString sn ^ "\n");

            send sock plain 
            (SS(Pubkey {cookie = nonce,
                        (* actually, this is wrong; SBITS and HBITS
                           are the sizes of the constituent primes *)
                        sbits = SBITS, sexp = se, smod = sn,
                        hbits = HBITS, hexp = he, hmod = hn,
                        protflags = nil, 
                        ciphers = [Des3],
                        auths = [Password]}));
            print "sent. waiting...\n";

            case getpacket sock plain of
                (RC (Sessionkey { cipher, cookie, key, protflags })) =>
                    let
                        val _ = cookie = nonce orelse raise SSHProt "client returned different cookie"
                        val _ = print ("encrypted session key: " ^ 
                                       ntohex key ^ "\n")

                        val _ = print ("requested cipher: " ^
                                       ciphertostring cipher ^ "\n")

                        (* since encrypted smaller, bigger,
                           we decode in the opposite order *)
                        (* XXX use size of keys; don't rely on values *)
                        val s = makenum(ssh_rsadec hd hn key)
                        val _ = print "dec 1 ok\n"
                        val sk = ssh_rsadec sd sn s

                        val _ = 
                          print ("unencrypted session key: " ^ 
                                 StringUtil.hexdump sk ^ "\n")

                        val sk = byte_xor session_id sk
                          
                        val _ =
                          print ("de-nonced session key: " ^
                                 StringUtil.hexdump sk ^ "\n")

                        val cf as { enc, dec } = 
                          cipherfunc cipher sk

                    in
                      send sock { cipher = enc, padzero = false }
                                (SS Success);

                      authenticate sock cf
                    end
              | _ => raise SSHProt "unexpected packet. wanted sessionkey.\n"
        end 


    (* for syslog *)
    val name = "mlsshd"




end