
(* User authentication. This currently only supports MD5Crypt (PAM)
   style passwords in /etc/shadow. *)

structure Auth =
struct

(* example *)
(* tom:$1$lew6vG6F$yVtGMObaYNke7OcpFk0QV0:11485:0:99999:7:-1:-1:134538220
 *)

  val shadow_file = 
    Params.param "/etc/shadow" 
                 (SOME ("-shadow", "The shadow passwd file")) 
                 "shadow_file"

  (* XXX I don't know what the other fields in the shadow
     password lines mean. Some of them are for account disabling,
     I think -- that is NOT SUPPORTED in this release. (Anyone
     with a valid password can connect, even to a disabled acct.) *)
  fun check (user, pass) =
    let
      val f = TextIO.openIn (!shadow_file)
      fun ckline () =
        case TextIO.inputLine f of
          NONE => Util.B "not in shadow file"
        | SOME s => 
              (case String.fields (StringUtil.ischar #":") s of
                  [usr, pwf, _, _, _, _, _, _, _] =>
                    if user = usr then
                      let
                        val (salt, hash) = MD5Crypt.getfields pwf
                      in
                        if MD5Crypt.crypt salt pass = hash then
                           case Passwd.lookup user of
                            SOME {uid,home,name,gid,...} => Util.A {uid=uid,
                                                                    gid=gid,
                                                                    home=home,
                                                                    name=name}
                          | NONE => Util.B "in shadow, not passwd?"
                        else Util.B "WRONG password"
                      end handle e => Util.B ("Exception: " ^ exnMessage e ^ " -- Bad shadow format?")
                    else ckline ()
                | _ => Util.B ("Bad shadow file?")) (* bad file? stop reading. *)
    in
        ckline () before TextIO.closeIn f
    end handle _ => Util.B "Error reading shadow file"

end
