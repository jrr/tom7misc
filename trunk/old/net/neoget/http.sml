
structure Http :> HTTP =
struct

  val itos = Int.toString

  fun splithdr s =
    let
      
      fun f n = 
        if n >= size s then (s, "") else
        case CharVector.sub(s, n) of
          #"\n" => g (n + 1)
        | _ => f (n + 1)
      and g n =
        if n >= size s then (s, "") else
          case CharVector.sub(s, n) of
            #"\n" => (String.substring(s, 0, n - 1),
                      String.substring(s, n + 1, size s - (n + 1)))
          | #"\r" => g (n + 1)
          | _ => f (n + 1)
    in
      f 0
    end

  fun get ua cookie referrer port host path =
    let
      val c = Hsock.connect (Hsock.address host) port
(*      val _ = print ("[" ^ host ^ "] connected...\n") *)
      val req = "GET " ^ path ^ " HTTP/1.0\r\n" ^
                "Accept: image/gif, image/jpeg, image/pjpeg, " ^
                "image/png, */*\r\n" ^
                "Referer: " ^ referrer ^ "\r\n" ^
                "Accept-Language: en-us\r\n" ^
                "Connection: Close\r\n" ^
                "User-Agent: " ^ ua ^ "\r\n" ^
                "Host: " ^ host ^ ":" ^ itos port ^ "\r\n" ^
                "Cookie: " ^ cookie ^ "\r\n" ^
                "\r\n"
                
    in
      Hsock.send c req;
(*      print ("[" ^ host ^ "] sent [" ^ req ^ "]\n"); *)
      splithdr (Hsock.recvall c)
    end

  fun post ua cookie referrer port host path body =
      let
	  val c = Hsock.connect (Hsock.address host) port
	  val req = 
	      "POST " ^ path ^ " HTTP/1.0\r\n" ^
	      "User-Agent: " ^ ua ^ "\r\n" ^
	      "Content-type: application/x-www-form-urlencoded\r\n" ^
	      "Content-Length: " ^ Int.toString (size body) ^ "\r\n" ^
	      "Host: " ^ host ^ ":" ^ itos port ^ "\r\n" ^
	      "Accept: image/gif, image/jpeg, image/pjpeg, " ^
	      "image/png, */*\r\n" ^
	      "Cookie: " ^ cookie ^ "\r\n" ^
	      "Referer: " ^ referrer ^ "\r\n" ^
	      "\r\n" ^
	      body
      in
	  Hsock.send c req;
	  splithdr (Hsock.recvall c)
      end

  (* RFC 2396 *)
  val okspec = StringUtil.charspec "[-_!*'()A-Za-z0-9./~]"
  fun charenc #" " = "+"    (* though this is de facto, I think it is non-RFC! *)
    | charenc c =
      if okspec c then str c
      else "%" ^ StringUtil.bytetohex (ord c)

  val urlencode = String.translate charenc

  fun querystring l = StringUtil.delimit "&" (map (fn (a, b) =>
						   urlencode a ^ "=" ^ urlencode b) l)

end
