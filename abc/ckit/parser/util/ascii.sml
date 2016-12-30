(* Copyright (c) 1998 by Lucent Technologies *)

(* Copyright 1989 by AT&T Bell Laboratories *)
(* ascii.sml *)

structure Ascii =
struct
    val caret            = 94
    val colon            = 58
    val comma            = 44
    val del              = 127
    val dollar           = 36
    val dot              = 46
    val dquote           = 34
    val equal            = 61
    val formfeed         = 12
    val greaterthan      = 62
    val lbrace           = 123
    val lbracket         = 91
    val lc_a             = 97
    val lc_n             = 110
    val lc_t             = 116
    val lc_z             = 122
    val lessthan         = 60
    val lparen           = 40
    val minus            = 45
    val newline          = 10
    val nine             = 57
    val percent          = 37
    val plus             = 43
    val query            = 63
    val rbrace           = 125
    val rbracket         = 93
    val return           = 13
    val rparen           = 41
    val semicolon        = 59
    val sharp            = 35
    val slash            = 47
    val space            = 32
    val squote           = 39
    val star             = 42
    val tab              = 9
    val tilde            = 126
    val uc_a             = 65
    val uc_z             = 90
    val underscore       = 95
    val zero             = 48

    fun isDigit (char) = char >= zero andalso char <= nine

end  (* structure Ascii *)
