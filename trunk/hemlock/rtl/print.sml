
structure RTLPrint :> RTLPRINT =
struct

    open RTL

    val tab = "    "

    val itos = Int.toString

    fun printp { code, entry, data } =
        let 
            fun cctos Exported = "(exported)"
              | cctos Internal = "(internal)"
        in
            print (";; Program entry: " ^ entry ^ "\n\n");

            print ";;          Code:\n";
            StringMap.appi (fn (lab, (typ, cod)) =>
                            let in
                                print ("\n" ^ lab ^ ":\n");
                                print (";; type: " ^ cctos typ ^ "\n");
                                printc cod
                            end) code;

            print "\n;;        Data:\n";
            StringMap.appi (fn (lab, (typ, data)) =>
                            let in
                                print ("\n" ^ lab ^ ":\n");
                                print (";; type: " ^ ttos typ ^ "\n");
                                print (tab ^ "DB ");
                                print (StringUtil.delimit ", "
                                       (Word8Vector.foldr 
                                        (fn (w,r) =>
                                         StringUtil.bytetohex 
                                         (Word8.toInt w) :: r) 
                                        nil data));
                                print "\n"
                            end) data
        end

    and ttos String = "string"
      | ttos T = "T"
      | ttos Int = "Int"
      | ttos Junk = "?"
      | ttos (Code (l, sl)) = 
        "(" ^ StringUtil.delimit ", " (map ttos l) ^ " SP: [" ^
        StringUtil.delimit ", " (map ttos sl) ^ "] => 0)"

    and printc (C (i, c)) = (printi i; printc c)
      | printc (Jmp src) =
        printl (tab ^ "JMP " ^ stos src)
      | printc (Return (b, s)) = printl (tab ^ "RET_" ^ Bool.toString b ^ 
                                         " " ^ stos s)
      | printc (Error s) = printl (tab ^ "ERROR : " ^ s)


    and stos (Rco i) = "R" ^ itos i
      | stos (Rin i) = "[" ^ stos (Rco i) ^ "]"
      | stos (Imm i) = itos i
      | stos (Lab l) = l
      | stos (Sco i) = "S{" ^ itos i ^ "}"

    and dtos (Rdst i) = "R" ^ itos i
      | dtos (Rto i) = "[" ^ dtos (Rdst i) ^ "]"
      | dtos (Sdst i) = "S{" ^ itos i ^ "}"

    and otos ADD = "ADD"
      | otos SUB = "SUB"
      | otos XOR = "XOR"
      | otos OR = "OR"
      | otos AND = "AND"

    and wtos INT = "INT"
      | wtos STRING = "STRING"
      | wtos REF = "REF"
      | wtos CODE = "CODE"

    and ctos E = "E"
      | ctos NE = "NE"
      | ctos L = "L"
      | ctos LE = "LE"
      | ctos G = "G"
      | ctos GE = "GE"

    and printi i =
        let in
            print tab;
            printl
            (case i of
                 Mov (d, s) => "MOV " ^ dtos d ^ ", " ^ stos s
               | Op (oper, i, s) => otos oper ^ " r" ^ itos i ^ ", " ^ stos s
               | Quotrem (src) => "QUOTREM " ^ stos src
               | Multiply (src) => "MULTIPLY " ^ stos src
               | Project (idx, d, s) => "PROJECT(" ^ stos idx ^ ") " ^ 
                                         dtos d ^ ", " ^ stos s
               | Salloc i => "SALLOC " ^ itos i
               | Sdelete i => "SDELETE " ^ itos i
               | Checkt (wt, d, s) => "CHECKT(" ^ wtos wt ^ ") " ^ 
                                         dtos d ^ ", " ^ stos s
               | Checkit (d, dd, s) => "CHECKIT (" ^ dtos d ^ ", " ^ 
                                         dtos dd ^ "), " ^ stos s
               | Setref (s, ss) => "SETREF " ^ stos s ^ ", " ^ stos ss
               | Jc (c, s) => "J" ^ ctos c ^ " " ^ stos s
               | Alloc (d, wt, s) => "ALLOC " ^ wtos wt ^ " " ^ 
                                         dtos d ^ ", " ^ stos s
               | Allocs (d, t, s) => "ALLOCSUM #" ^ itos t ^ " " ^ 
                                     dtos d ^ ", " ^ stos s
               | Alloct (d, sl) => "ALLOCT " ^ dtos d ^ ", (" ^
                                   StringUtil.delimit ", " (map stos sl) ^ ")"
               | Cmp (a, b) => "CMP " ^ stos a ^ ", " ^ stos b
               | Callpop (what, nargs, ss) => "CALL " ^ what ^ "(" ^ 
                                              itos nargs ^ " args, " ^
                                              itos ss ^ " local)"
               | Push s => "PUSH " ^ stos s
               | Pop d => "POP " ^ dtos d
               | Array (d, s1, s2, i) => "ARRAY " ^ 
                                              dtos d ^ ", " ^ stos s1 ^ ", " ^ 
                                              stos s2 ^ "(@ " ^ itos i ^ ")"
               | Array0 (d, i) => "ARRAY0 " ^ dtos d ^ " (@ " ^ itos i ^ ")"
               | Update (sa, si, sv) => "UPDATE " ^ stos sa ^ ", " ^ stos si ^
                                              ", " ^ stos sv
               | Marshall (d, s, i) => "MARSHALL " ^ dtos d ^ ", " ^ stos s ^ 
                                              "(@ " ^ itos i ^ ")"
               | Allocsv (d, s, ss) => "ALLOCSV " ^ dtos d ^ ", " ^ stos s ^
                                              "(@ " ^ itos ss ^ ")"

               | Unmarshvec (d, s, ss) => "UMVEC " ^ dtos d ^ ", " ^ stos s ^
                                              "(@ " ^ itos ss ^ ")"

               | Getwitvec (d, s, ss) => "GETWITVEC " ^ 
                                              dtos d ^ ", " ^ stos s ^
                                              "(@ " ^ itos ss ^ ")"

               | Boxa (d, s) => "BOXA " ^ dtos d ^ ", " ^ stos s
                                              
               | Spawn (d, s1, s2, ss, b) => "SPAWN_" ^ (if b then "true " 
                                                         else "false ")
                                              ^ dtos d ^ ", " ^ stos s1 ^
                                              ", " ^ stos s2 ^
                                              "(@ " ^ itos ss ^ ")"

               | Checkv (d, s) => "CHECKV " ^ dtos d ^ ", " ^ stos s
               | Unboxsv (d, s, ss) => "UNBOXSV " ^ dtos d ^ ", " ^ stos s ^
                                              "(@ " ^ itos ss ^ ")"

               | Concat (d, s, ss) => "CONCAT " ^ dtos d ^ ", " ^ stos s ^
                                              "(@ " ^ itos ss ^ ")"

               | Random (d, ss) => "RANDOM " ^ dtos d ^
                                              "(@ " ^ itos ss ^ ")"
(*             | _ => " ** INSTRUCTION UNIMPLEMENTED **" *))
        end

    and printl s = (print s; print "\n")

end
