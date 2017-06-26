
structure Tokens =
struct

    datatype token =
        STRLIT of string
      | ID of string
      | INT of int
      | CHAR of char
      | FLOAT of real
      | FN
      | LET
      | IN
      | OP
      | END
      | CASE
      | OF
      | AS
      | ELSE
      | IF
      | THEN
      | TIMES
      | DIVIDE
      | HASH
      | DO
      | VAL
      | AND
      | FUN
      | TYPE
      | DATATYPE
      | DERIVING
      | EXCEPTION
      | HANDLE
      | TAGTYPE
      | NEWTAG
      | ANDALSO
      | ANDTHEN
      | ORELSE
      | OTHERWISE
      | INFIX
      | INFIXR
      | NONFIX
      | STRUCTURE
      | STRUCT
      | LIBRARY
      | LIB
      | SIGNATURE
      | SIG
      | COLONGREATER
      | DOT
      | LPAREN
      | RPAREN
      | DARROW
      | ARROW
      | COLON
      | SEMICOLON
      | LBRACE
      | RAISE
      | RBRACE
      | BAR
      | UNDERSCORE
      | EQUALS
      | COMMA
      | SYNCALL
      | SPAWN
      | SUBMIT
      | LSQUARE
      | RSQUARE
      | IMPORT

    fun eq (FN, FN) = true
      | eq (LET, LET) = true
      | eq (IN, IN) = true
      | eq (END, END) = true
      | eq (CASE, CASE) = true
      | eq (OF, OF) = true
      | eq (AS, AS) = true
      | eq (ELSE, ELSE) = true
      | eq (IF, IF) = true
      | eq (THEN, THEN) = true
      | eq (TIMES, TIMES) = true
      | eq (DIVIDE, DIVIDE) = true
      | eq (HASH, HASH) = true
      | eq (DO, DO) = true
      | eq (VAL, VAL) = true
      | eq (AND, AND) = true
      | eq (FUN, FUN) = true
      | eq (TYPE, TYPE) = true
      | eq (DATATYPE, DATATYPE) = true
      | eq (EXCEPTION, EXCEPTION) = true
      | eq (HANDLE, HANDLE) = true
      | eq (TAGTYPE, TAGTYPE) = true
      | eq (NEWTAG, NEWTAG) = true
      | eq (ANDALSO, ANDALSO) = true
      | eq (ANDTHEN, ANDTHEN) = true
      | eq (ORELSE, ORELSE) = true
      | eq (OTHERWISE, OTHERWISE) = true
      | eq (INFIX, INFIX) = true
      | eq (INFIXR, INFIXR) = true
      | eq (NONFIX, NONFIX) = true
      | eq (STRUCTURE, STRUCTURE) = true
      | eq (STRUCT, STRUCT) = true
      | eq (LIBRARY, LIBRARY) = true
      | eq (LIB, LIB) = true
      | eq (SIGNATURE, SIGNATURE) = true
      | eq (SIG, SIG) = true
      | eq (COLONGREATER, COLONGREATER) = true
      | eq (RAISE, RAISE) = true
      | eq (DOT, DOT) = true
      | eq (LPAREN, LPAREN) = true
      | eq (RPAREN, RPAREN) = true
      | eq (DARROW, DARROW) = true
      | eq (ARROW, ARROW) = true
      | eq (COLON, COLON) = true
      | eq (SEMICOLON, SEMICOLON) = true
      | eq (LBRACE, LBRACE) = true
      | eq (RBRACE, RBRACE) = true
      | eq (OP, OP) = true
      | eq (BAR, BAR) = true
      | eq (UNDERSCORE, UNDERSCORE) = true
      | eq (DERIVING, DERIVING) = true
      | eq (EQUALS, EQUALS) = true
      | eq (COMMA, COMMA) = true
      | eq (SUBMIT, SUBMIT) = true
      | eq (SYNCALL, SYNCALL) = true
      | eq (SPAWN, SPAWN) = true
      | eq (LSQUARE, LSQUARE) = true
      | eq (RSQUARE, RSQUARE) = true
      | eq (IMPORT, IMPORT) = true
      | eq _ = false
end