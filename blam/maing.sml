

val gpsac = Params.flag false (SOME("-c",
                                    "use C gpsa engine")) "gpsac"

val _ =
  case Params.docommandline () of
    [a, b] => ignore
      (if !gpsac
       then print (Int.toString (GPSA_C.bestalignment (a, b)) ^ "\n")
       else 
         let 
           val n1 = size a
           val n2 = size b
         in
           print (Int.toString n1 ^ " x " ^ Int.toString n2 ^ "\n");
           print (a ^ "\n");
           print (b ^ "\n");
           print (Int.toString (GPSA.bestalignment (a, b)) ^ "\n")
         end)
  | _ =>
      let in
        print (Params.usage ());
        print "blam [options] str1 str2\n"
      end
