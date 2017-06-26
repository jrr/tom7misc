
structure Initfix :> INITFIX =
struct

    val initial =
        [
         ("<", (2, Parsing.Non)),
         ("<=", (2, Parsing.Non)),
         (">", (2, Parsing.Non)),
         (">=", (2, Parsing.Non)),
         ("<>", (2, Parsing.Non)),

         (":=", (1, Parsing.Non)),

         ("+", (4, Parsing.Left)),
         ("-", (4, Parsing.Left)),
         ("*", (5, Parsing.Left)),
         ("div", (5, Parsing.Left)),
         ("mod", (5, Parsing.Left))]

end