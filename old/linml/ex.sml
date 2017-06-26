
local open AST
in

    (* thread that deadlocks itself *)
    val dl =
	(Lett ([("r", "rr"), 
		("s", "ss")], Chan Bool, 
	       Seq[Close (Send(Var "ss",True)),
		   Recv (Var "rr")]))

    (* inter-thread communication *)
    val com =
	(Lett ([("r", "rr"),
		("s", "ss")], Chan Bool,
	       Seq[Spawn (Lfn ("x", Tensor [],
			       Lett ([("data", "d"),
				      ("channel", "c")],
				     Recv (Var "rr"),
				     Seq[Close (Var "c"),
					 If (Var "d", Var "x", Var "x")]))),
		   Close (Send (Var "ss", False))]))

    (* simple spawn test *)
    val spawn = 
	Seq[Spawn (Lfn ("x", Tensor[], Var "x")),
	    True]


    val protodom = Tensor [("c0", Tvar "key1"),
                           ("c1", Bang (Lolli (Tensor[("1", String),
                                                      ("2", Tvar "key1")],
                                               Tvar "key2"))),
                           ("c2", Bang (Lolli (Tvar "key2",
                                               Tensor[("1", Bool),
                                                      ("2", Tvar "key3")]))),
                           ("c3", Bang (Lolli (Tvar "key3",
                                               Tvar "done")))]
      
    val protot =
	All ("key1",
	     All ("key2",
		  All ("key3",
		       All ("done",
			    Lolli (protodom,
				   Tvar "done")))))

    (* the keys just carry the linear resources, and don't change them. *)
    val key1 = Tensor [("str", Schan String),
                       ("boo", Rchan Bool)]
    val key2 = key1
    val key3 = key2

    val done = Tensor []

    val arg =
      Tuple [("c0", Tuple[("str", Var "send-strings"), ("boo", Var "receive-bools")]),
             ("c1", Exp (Lfn ("q",
                              Tensor[("1", String),("2",key1)],
                              Lett ([("1", "ss"),
                                     ("2", "k")],
                                    Var "q",
                                    Lett ([("str", "str"),
                                           ("boo", "boo")],
                                          Var "k",
                                          Let ("newstr", Send (Var "str", Var "ss"),
                                               Tuple[("str", Var "newstr"),
                                                     ("boo", Var "boo")])))))),
             ("c2", Exp (Lfn ("q", key2,
                              Lett ([("str", "str"),
                                     ("boo", "boo")],
                                    Var "q",
                                    Lett ([("data", "d"),
                                           ("channel", "c")],
                                          Recv (Var "boo"),
                                          Tuple[("1", Var "d"),
                                                ("2", Tuple[("str", Var "str"),
                                                            ("boo", Var "c")])]))))),
             ("c3", Exp (Lfn ("q", key3,
                              Lett ([("str", "str"),
                                     ("boo", "boo")],
                                    Var "q",
                                    Seq [Close (Var "str"),
                                         Close (Var "boo")]))))
             ]
               

    val server = Lfn ("zz",
                      Tensor[],
                      Seq[Lett ([("data", "str"),
                                 ("channel", "chn")],
                                (Recv (Var "receive-strings")),
                                Seq [Close (Var "chn"),
                                     Print (Var "str"),
                                     Print (Str "\n")]),
                          Close (Send (Var "send-bools", True)),
                          Var "zz"])

    val protoshell =
        Lett ([("r", "receive-bools"),
               ("s", "send-bools")], Chan Bool,
              Lett ([("r", "receive-strings"),
                     ("s", "send-strings")], Chan String,
                    Seq [Spawn server,
                         Lfn ("client", protot,
                              Spawn (Lfn ("y", Tensor[],
                                          Seq [Var "y",
                                               Lapp(Tapp(Tapp(Tapp(Tapp(Var "client",
                                                                        key1),
                                                                   key2),
                                                              key3),
                                                         done),
                                                    arg)])))]
                    ))

    val client =
      Tfn ("key1",
           Tfn("key2",
               Tfn("key3",
                   Tfn("done",
                       Lfn ("cs", protodom, 
                            Lett ([("c0", "c0"),
                                   ("c1", "c1"),
                                   ("c2", "c2"),
                                   ("c3", "c3")],
                                  Var "cs",
                                  Use ("cc1", Var "c1",
                                       Use ("cc2", Var "c2",
                                            Use ("cc3", Var "c3",
                                                 Let ("k2", Lapp (Var "cc1",
                                                                  Tuple[("1", Str "Hello"),
                                                                        ("2", Var "c0")]),
                                                      Lett([("1", "bl"),
                                                            ("2", "k3")],
                                                           Lapp (Var "cc2",
                                                                 Var "k2"),
                                                           Seq[If (Var "bl",
                                                                   Print (Str "Got True\n"),
                                                                   Print (Str "Got False\n")),
                                                               Lapp (Var "cc3",
                                                                     Var "k3")]
                                                           )))))))))))
      

    val example =
      Lapp(protoshell, client)

end