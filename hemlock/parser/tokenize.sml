
(* Combinator-style tokenizer. Takes characters as inputs, outputs
   items of type Tokens.token (see tokens.sml). This was adapted
   from a very old tokenizer for something else, so there are some
   aspects of it that are a little strange. *)

structure Tokenize :> TOKENIZE =
struct

  open Tokens

  open Parsing

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard
  infixr 1 ||

  val quotc = #"\"" (* " *)

  (* Report an error with its position. *)
  (* XXX should raise exception? *)
  fun error s pos = print (Pos.toString pos ^ ": " ^ s ^ "\n")

  (* Succeeds if p succeeds at the current point, but doesn't
     consume anything. *)

  fun ahead p = lookahead p (fn _ => succeed ())

  (* Ignores the result of p *)

  fun ignore p = p return ()

  fun midchar #"_" = true
    | midchar #"'" = true
    | midchar #"-" = true
    | midchar _ = false

  val isSymbolic = Char.contains "!%&$#+-/:<=>@\\~`^|*."

  (* these never appear as part of a longer identifier *)
  val isSep = Char.contains "()[],{}; "

  fun identchar c = Char.isAlpha c orelse Char.isDigit c orelse midchar c

  (* probably should have \r, \t, \x05, etc. *)
  val escapechar = 
    ((literal #"\\" >> literal quotc) ||
     (literal #"\\" >> literal #"\\") ||
     (literal #"\\" && literal #"n" return #"\n"))

  val getchar = (satisfy (fn x => x <> quotc andalso x <> #"\\") ||
                 escapechar)

  val integer = 
      (repeat1 (satisfy Char.isDigit))
      wth (Option.valOf o Int.fromString o implode)

  val insidechars = (repeat getchar) wth implode

  val stringlit = 
    (literal quotc) >>
    ((insidechars << (literal quotc)) 
      guard error "Unclosed string or bad escape.")

  val char = ((literal #"?" >> (satisfy (fn x => x <> #"\\"))) ||
              (literal #"?" >> escapechar))

  val float = ((repeat  (satisfy Char.isDigit)) <<
               (literal #".")) &&
                  (repeat1 (satisfy Char.isDigit))
                  wth (fn (a, b) =>
                       (Option.valOf (Real.fromString 
                                      ("0." ^ (implode b)))) +
                       (case a of 
                            nil => 0.0
                          | _ => Real.fromInt 
                                (Option.valOf (Int.fromString 
                                               (implode a)))))

  val number = integer || (literal #"~" >> integer) wth op~

  fun comment () = string [#"(",#"*"]
      && (repeat ($insideComment) && string [#"*",#")"]
          guard error "Unterminated comment.")

  (* Either a nested comment or a single character (which is not
     start of a nested comment or the comment terminator). *)

  and insideComment () =
      ignore ($comment)
         || any -- (fn #"*" => ahead (satisfy (fn x => x <> #")"))
                     | #"(" => ahead (satisfy (fn x => x <> #"*"))
                     | _    => succeed ())

  (* White space. *)

  val space = repeat (ignore ($comment) || ignore (satisfy Char.isSpace))

  val keywords =
      [("and", AND),
       ("andalso", ANDALSO),
       ("andthen", ANDTHEN),
       ("as", AS),
       ("case", CASE),
       ("datatype", DATATYPE),
       ("import", IMPORT),
       ("do", DO),
       ("else", ELSE),
       ("end", END),
       ("exception", EXCEPTION),
       ("fn", FN),
       ("fun", FUN),
       ("handle", HANDLE),
       ("if", IF),
       ("in", IN),
       ("infix", INFIX),
       ("infixr", INFIXR),
       ("let", LET),
(*         ("lib", LIB),
       ("library", LIBRARY), *)
       ("newtag", NEWTAG),
       ("nonfix", NONFIX),
       ("of", OF),
       ("op", OP),
       ("orelse", ORELSE),
       ("otherwise", OTHERWISE),
(*         ("sig", SIG),
       ("signature", SIGNATURE),*)
       ("deriving", DERIVING),
(*         ("struct", STRUCT),
       ("structure", STRUCTURE), *)
       ("submit", SUBMIT),
       ("spawn", SPAWN),
       ("syncall", SYNCALL),
       ("tagtype", TAGTYPE),
       ("then", THEN),
       ("type", TYPE),
       ("val", VAL),
       ("raise", RAISE),
       ("(", LPAREN),
       (")", RPAREN),
       ("->", ARROW),
       (",", COMMA),
       ("*", TIMES),
       ("/", DIVIDE),
       ("#", HASH),
       ("[", LSQUARE),
       ("]", RSQUARE),
       ("|", BAR),
       ("_", UNDERSCORE),
       (":", COLON),
       (";", SEMICOLON),
       ("=", EQUALS),
       ("=>", DARROW)
       ]

  fun ident s =
      let
          fun id nil = ID s
            | id ((a,b)::t) = if a = s then b else id t
      in
          id keywords
      end

  val letters = satisfy Char.isAlpha && 
                repeat (satisfy identchar) wth op::

  val word = 
      alt [letters wth implode,
           (satisfy isSep) wth Char.toString,
           repeat1 (satisfy isSymbolic) wth implode]


  val token = space >> !! (alt [char wth CHAR,
                                float wth FLOAT,
                                number wth INT,
                                stringlit wth STRLIT,
                                word wth ident,
                                literal #"_" return UNDERSCORE 
                                      (* XXX ok, but nasty... *)
                                ])
end