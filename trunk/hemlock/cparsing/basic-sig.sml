signature BASIC_PARSING =
sig

    (* Parser with token type 't, result type 'a *)
    type ('a,'t) parser

    (* succeed with given value *)
    val succeed : 'a -> ('a,'t) parser
    (* fail immediately *)
    val fail : ('a,'t) parser

    (* check for end of input *)
    val done : 'a -> ('a,'t) parser
    (* admit anything, provided there's something on the input *)
    val any : ('t,'t) parser

    (* sequential successful composition of parsers *)
    val -- : ('a,'t) parser * ('a -> ('b,'t) parser) -> ('b,'t) parser
    (* sequential failing composition of parsers *)
    val ## : ('a,'t) parser * (Pos.pos -> ('a,'t) parser) -> 
              ('a,'t) parser

    (* grab position *)
    val !! : ('a,'t) parser  -> ('a * Pos.pos,'t) parser

    (* to handle mutually-recursive parsers *)
    val $ : (unit -> ('a,'t) parser) -> ('a,'t) parser

    (* to construct a recursive parser *)
    val fix : (('a,'t) parser -> ('a,'t) parser) -> ('a,'t) parser

    (* re-parse same input, given result of first parse *)
    val lookahead : ('a,'t) parser -> ('a -> ('b,'t) parser) -> 
                      ('b,'t) parser

    (* parse this stream before reading any other input *)
    val push : ('t * Pos.pos) Stream.stream -> 
                ('a,'t) parser -> ('a, 't) parser

    (* parse a stream *)
    val parse : ('a,'t) parser -> ('t * Pos.pos) Stream.stream -> 
                 'a option

    (* transform a stream by repeatedly parsing it *)
    val transform : ('a,'t) parser -> ('t * Pos.pos) Stream.stream -> 
                     'a Stream.stream

end