
(* XXX -- needs sealing since opened in SSH *)

structure SSHProt =
struct

    exception SSHProt of string

    structure N = IntInf

    datatype prot = 
        Screen_Number
      | Host_in_Fwd_Open

    val protvalues = [(Screen_Number, 0), (Host_in_Fwd_Open, 1)]

    (* Note: probably good to only support 3DES and perhaps Blowfish.
       RC4 is known to be flawed, IDEA is patented and flawed,
       and None is obviously weak! *)
    (* Blowfish is not in the RFC; value from openssh *)
    datatype cipher =
        None | Idea | Des | Des3 | Rc4 | Blowfish

    val MAX_PACKET = 262144

    val ciphervalues = [(None, 0), (Idea, 1), (Des, 2), (Des3, 3), (Rc4, 5),
                        (Blowfish, 6)]

    fun ciphertostring None = "None"
      | ciphertostring Idea = "IDEA"
      | ciphertostring Des = "DES"
      | ciphertostring Des3 = "3DES"
      | ciphertostring Rc4 = "RC4"
      | ciphertostring Blowfish = "Blowfish"

    (* Should not support Rhosts *)
    datatype auth =
        Rhosts | Rsa | Password | Rhosts_Rsa

    val authvalues = [(Rhosts, 1), (Rsa, 2), (Password, 3), (Rhosts_Rsa, 4)]

    datatype msg =
        Disconnect of string

    datatype unsupported = UNSUPPORTED

    datatype cmsg =
        Sessionkey of
        {cipher : cipher,
         cookie : CharVector.vector,
         key : IntInf.int,
         protflags : prot list}
      | User of string
      | RequestCompression of unsupported
      | RequestPTY of unsupported
      | RequestX11Forwarding of unsupported
      | RequestPortForwarding of unsupported
      | RequestAgentForwarding of unsupported
      | ExecShell
      | ExecCmd of string
      | Stdin

    datatype smsg =
        Pubkey of
        {cookie : CharVector.vector,
         sbits : int,
         sexp : IntInf.int,
         smod : IntInf.int,
         hbits : int,
         hexp : IntInf.int,
         hmod : IntInf.int,
         protflags : prot list,
         ciphers : cipher list,
         auths : auth list}
      | Success
      | Failure
      | Stdout of string

    datatype sendable =
        SS of smsg
      | SM of msg

    datatype recvable =
        RC of cmsg
      | RM of msg

    val n256 = N.fromInt 256

    fun itov i =
        String.implode [chr ((i div (256*256*256)) mod 256),
                        chr ((i div (256*256)) mod 256),
                        chr ((i div  256) mod 256),
                        chr (i mod 256)] handle _ => raise SSHProt (print "exn at 1083"; print "\n"; "exn at 1083")

    fun i16tov i =
        String.implode [chr ((i div 256) mod 256),
                        chr (i mod 256)] handle _ => raise SSHProt (print "exn at 1206"; print "\n"; "exn at 1206")

    fun wtov (i : Word32.word) =
        CharVector.fromList [chr (Word32.toInt 
                                  (Word32.andb(Word32.>>(i, 0w24), 0w255))),
                             chr (Word32.toInt
                                  (Word32.andb(Word32.>>(i, 0w16), 0w255))),
                             chr (Word32.toInt
                                  (Word32.andb(Word32.>>(i, 0w8), 0w255))),
                             chr (Word32.toInt
                                  (Word32.andb(i, 0w255)))]
        handle _ => raise SSHProt (print "exn at 1578"; print "\n"; "exn at 1578")

    fun vtoi v =
        let fun vv x = CharVector.sub (v, x)
        in
            ord (vv 0) * 256 * 256 * 256 +
            ord (vv 1) * 256 * 256 +
            ord (vv 2) * 256 +
            ord (vv 3)
        end handle _ => raise SSHProt (print "exn at 1790"; print "\n"; "exn at 1790")

    fun ftow fl l =
        let
            fun go nil = 0w0
              | go (h::t) =
                case ListUtil.Alist.find (op=) fl h of
                    NONE => raise SSHProt "bad flag"
                  | SOME b => Word32.orb(Word32.<<(0w1, Word.fromInt b), go t)
        in go l
        end handle _ => raise SSHProt (print "exn at 2057"; print "\n"; "exn at 2057")

    fun wtof fl w =
        let
            fun go nil = nil
              | go ((f,b)::t) = 
              if Word32.andb(Word32.<<(0w1, Word.fromInt b), w) > 0w0 
              then f :: go t else go t
        in
            go fl
        end

    fun ftov l n =
        wtov (ftow l n)

    (* XXX calculate *actual* number of bits *)
(*
    fun ntov n =
        case MLton.IntInf.rep n of
            MLton.IntInf.Small w => i16tov (4*8) ^ wtov (Word32.fromInt (Word.toInt w))
          | MLton.IntInf.Big w => 
                let val s = MLton.IntInf.size n
                    val _ = print ("Big. Words: " ^ Int.toString s ^ "\n")
                    val _ = print ("actual length w: " ^ 
                                   Int.toString (Vector.length w) 
                                   ^ "\n")
                    val lw = Vector.length w
                in
                    i16tov (s * 4 * 8) ^
                    CharVector.concat (map wtov (List.drop (Vector.foldr op:: nil w,
                                                            (lw - s))))
                end
                handle _ => raise SSHProt (print "exn at 2376"; print "\n"; "exn at 2376")
*)

    (* Extract bytes by division; more portable and more predictable.
       Less efficient, but bignums are not used in the steady state of ssh. *)
    fun ntobytes n = 
        let fun bts m acc =
                if m = Number.n0 then acc
                else let val (q, r) = Number.quotrem (m, n256)
                     in
                         bts q ((chr (N.toInt r))::acc)
                     end
        in implode (bts n nil)
        end

    fun ntov n =        
        let val bytes = ntobytes n
        in i16tov (size bytes * 8) ^ bytes
        end

    fun rton (rdr as {char, ...} : Reader.reader) =
        let
            val bits = Reader.rb16 rdr
            val bytes = (bits + 7) div 8
            fun go 0 acc = acc
              | go n acc = go (n - 1) (N.+(N.*(acc, n256), N.fromInt(ord (char()))))
        in
            go bytes Number.n0
        end

    fun rtos (rdr as {vec, ...} : Reader.reader) =
      let
        val len = Reader.rb32 rdr
      in
        if len > MAX_PACKET (* actually something like MAX_PACKET - 8.
                               but it will fail safely in reader if too
                               big anyway *)
           orelse len < 0
        then raise SSHProt "bad length"
        else ();
        (vec len) handle _ => raise SSHProt "bad string"
      end


    fun packstring s = (itov (size s)) ^ s

    (* make a sendable packet into a vector.
       doesn't perform the padding step or attach the CRC. *)
    fun sptov (SS Success) = implode [chr 14]
      | sptov (SS Failure) = implode [chr 15]
      | sptov (SS (Stdout s)) = 
               (CharVector.concat
                [implode [chr 17],
                 packstring s])
      | sptov (SS (Pubkey
                  {cookie : CharVector.vector,
                   sbits : int,
                   sexp : IntInf.int,
                   smod : IntInf.int,
                   hbits : int,
                   hexp : IntInf.int,
                   hmod : IntInf.int,
                   protflags : prot list,
                   ciphers : cipher list,
                   auths : auth list})) =
        (CharVector.concat
         [implode [chr 2], (* SSH_SMSGS_PUBLIC_KEY *)
          cookie,
          itov sbits,
          ntov sexp,
          ntov smod,
          itov hbits,
          ntov hexp,
          ntov hmod,
          ftov protvalues protflags,
          ftov ciphervalues ciphers,
          ftov authvalues auths] 
         handle _ => raise SSHProt (print "exn at 3037"; print "\n"; "exn at 3037"))
      | sptov _ = raise SSHProt "oops!"
        
    fun pack {cipher, padzero} pkt =
        let (* length includes CRC, but not pad. *)
            val l = CharVector.length pkt + 4 
            val _ = print ("Sending packet of length " ^ Int.toString l ^ "\n")
            val pad = if padzero then CharVector.tabulate(8 - (l mod 8),
                                                          fn _ => #"\000")
                      else CryptRand.vec (8 - (l mod 8))
            val crc = CRC32.crcstringi pkt (CRC32.crcstring pad)
            val dat = 
                CharVector.concat
                [itov l,
                 cipher (CharVector.concat [pad,
                                            pkt,
                                            wtov crc])]
        in
            print ("What I send:\n" ^ StringUtil.hexdump dat ^ "\n");
            dat
        end handle _ => raise SSHProt (print "exn at 3540"; 
                                       print "\n"; 
                                       "exn at 3540")

    fun send sock cph s =
        SockUtil.sendveca sock (SockUtil.stov (pack cph (sptov s)))

    (* XXX doesn't need padzero *)
    fun recv sock ({cipher, padzero}) =
        let 
            val l = vtoi (SockUtil.vtos (SockUtil.recva sock 4))
            val lpad = 8 - (l mod 8)
            val _ = print (Int.toString l ^ " bytes data/cksum, " ^ 
                           Int.toString lpad ^ " padding\n")
            val _ = l > MAX_PACKET andalso raise SSHProt "packet too large"

            (* get padding, payload, and checksum *)
            val body = SockUtil.vtos (SockUtil.recva sock (lpad + (l - 4) + 4))

            val _ = print ("Encrypted body: \n" ^
                           StringUtil.hexdump body ^ "\n");

            (* decrypt *)
            val body = cipher body

            val _ = print "(decrypted)\n"
            val br = Reader.fromvec body
            val pad = (#vec br) lpad
            val _ = print "padding:\n"
            val _ = print (StringUtil.hexdump pad)
              
            val pkt = (#vec br) (l - 4)
            val _ = print "packet:\n"
            val _ = print (StringUtil.hexdump pkt)

            (* PERF just get a word and compare words below *)
            val cks = (#vec br) 4
            val _ = print "crc32:\n"
            val _ = print (StringUtil.hexdump cks)

            (* verify checksum: *)

            val _ = wtov (CRC32.crcstringi pkt (CRC32.crcstring pad)) = cks 
                        orelse raise SSHProt "CRC32 failed on packet"
        in
            pkt
        end
    handle e => raise (print "exn at 3979"; print "\n"; e)

    (* get ie cipher by its number *)
    fun getbynum name alist n =
      case ListUtil.Alist.find op= (ListUtil.Alist.swap alist) n of
        NONE => raise SSHProt ("unknown " ^ name ^ " number " ^ 
                               Int.toString n)
      | SOME d => d

    (* in these, might want to ensure that we use up the whole packet,
       instead of possibly just some prefix. *)
    fun getpacket sock (cph as ({cipher, padzero})) =
        let val pkt as {char, vec, ...} = Reader.fromvec (recv sock cph)
        in case ord (char ()) of
            0 => raise SSHProt "got SSH_MSG_NONE"
          | 1 => raise SSHProt "got SSH_MSG_DISCONNECT" (* XXX maybe return and handle elsewhere *)
          | 2 => raise SSHProt "SSH_SMSG_PUBLIC key should only be sent by server"
          | 3 => RC(Sessionkey { cipher = 
                                  case ListUtil.Alist.find (op=) 
                                       (ListUtil.Alist.swap ciphervalues) 
                                       (ord (char())) of
                                      SOME c => c
                                    | NONE => raise SSHProt "bad cipher value",
                                 cookie = vec 8,
                                 key = rton pkt,
                                 protflags = wtof protvalues (Reader.rbw32 pkt)})
          | 4 => RC(User (rtos pkt))
          | 10 => RC(RequestPTY UNSUPPORTED)
          | 28 => RC(RequestPortForwarding UNSUPPORTED)
          | 30 => RC(RequestAgentForwarding UNSUPPORTED)
          | 34 => RC(RequestX11Forwarding UNSUPPORTED)
          | 36 => (* ignore debug messages. *) getpacket sock cph
          | 37 => RC(RequestCompression UNSUPPORTED)
          | 12 => RC ExecShell
          | 13 => RC (ExecCmd (rtos pkt))
          | t => raise SSHProt ("uknown message type " ^ Int.toString t)

        end

end