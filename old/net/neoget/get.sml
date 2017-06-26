
val _ = case Params.docommandline () of
    [host, url] =>
    print (#2 (Http.get "useragent" "" "" 80 host url))
  | _ => print ("usage: get host.com /path/to/file\n")

