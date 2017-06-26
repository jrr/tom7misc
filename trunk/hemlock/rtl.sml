
structure RTL =
struct

    (* (typed) Register Transfer Language. 
       This is much like a generic TAL. We assume a fixed
       (but unspecified) number of registers, ...

       A program is structured as a list of code blocks. These
       represent the function bodies in the outer level 'FIX'
       in the CPS language. A program also has an entry point
       (one of the labels to serve as the "beginning" of the
       program) and a series of data blocks.

       *)
    
    type label = string

    datatype src =
        (* contents of register n *)
        Rco of int
        (* indirect through reg n *)
      | Rin of int
        (* immediate value *)
      | Imm of int
        (* immediate label *)
      | Lab of label
        (* stack offset (words) where top of stack is 0 *)
      | Sco of int

    datatype dst =
        (* into a register n, 0-based *)
        Rdst of int
        (* to the address in register n *)
      | Rto of int
        (* stack offset (words) where top of stack is 0 *)
      | Sdst of int

    datatype oper =
        ADD | SUB | XOR | OR | AND (* ... *)

    datatype wordtag = INT | STRING | REF | CODE

    datatype cond =
        E | NE | L | LE | G | GE

    (* XXX: write down rules about what kinds of
       operands are allowed *)

    (* XXX consider changing stuff like Alloc's
       dst to be an int (register num) instead
       of memory location, since we require that
       in total. *)

    datatype inst =
        Mov of dst * src
      (* left hand must be register num, since it is
         source and dest of op *)
      | Op of oper * int * src
      (* puts quot in reg 0, rem in reg 3. *)
      | Quotrem of src
      (* puts result in reg3 : reg0 *)
      | Multiply of src
      (* allocate space for n more words on the stack, shifting
         the stack pointer in the process *)
      | Salloc of int
      | Sdelete of int
      | Cmp of src * src
      | Jc of cond * src
      (* alloc an int, string, ref, etc. The contents
         is in 'src'. Happiest when dst = Rdst 0 *)
      | Alloc of dst * wordtag * src
      (* alloc a tagged sum; the integer is the tag *)
      | Allocs of dst * int * src

      (* ensure the tag of src is wt, put contents in dst *)
      | Checkt of wordtag * dst * src

      (* set value inside first src to second src *)
      | Setref of src * src
      (* first is tag, second is contents : T *)
      | Checkit of dst * dst * src

      (* concatenate an unboxed vector of strings into a single string *)
      | Concat of dst * src * int

      (* check that src is a vector containing only strings.
         put an unboxed array of unboxed strings in dst.
         give the current stack size.
         *)
      | Unboxsv of dst * src * int

      (* take an unboxed array of unboxed strings, and make
         a ttt out of it to put in dst. needs stack size *)
      | Allocsv of dst * src * int

      (* string [] in (cord ids), string [] out (marshalled results) *)
      | Getwitvec of dst * src * int

      (* check that it is a tuple *)
      | Checkv of dst * src

      (* project idx res arr/tup. *)
      | Project of src * dst * src

      (* alloc a tuple; the src list gives the contents
         (only sco allowed) *)
      | Alloct of dst * src list

      | Push of src
      | Pop of dst

      | Boxa of dst * src

      (* for newtag. needs stack size *)
      | Random of dst * int

      (* must be registers, include stack size *)
      | Marshall of dst * src * int

      | Unmarshvec of dst * src * int

      | ArraySize of dst * src

      (* dst <- array (n, init, ssize)
         allocate a new array of non-constant size,
         filled with an initial element. include
         stack size *)
      | Array of dst * src * src * int

      (* needs stack size. 
         generates unboxed 0-len array of ttt *)
      | Array0 of dst * int

      (* dst, code, deps, ssize, continueself *)
      | Spawn of dst * src * src * int * bool

      (* arr idx val *)
      | Update of src * src * src

      (* provide a function symbol (that is, possibly
         null pointer to function), the number of args
         (already placed on top of stack) and the
         number of local variables (not counting those
         just pushed as args). Return is in reg 0.

         The available functions are in talend/runtime.sml
         *)
      | Callpop of string * int * int


    datatype code =
        C of inst * code
      | Jmp of src
      (* return to the calling frame, ending this CPS
         program. If bool=true, then forward. If false
         then finish. *)
      | Return of bool * src

      | Error of string

    datatype rtltype =
        (* r1, .., rNREG , sp = s1::s2:: ... ::nil  -> void *)
        Code of rtltype list * rtltype list
      | String
      | T
      | Int (* ie, 4-byte word *)
      | Junk

    datatype class =
        Exported
      | Internal

    (* StringMaps are really 'labelmaps' *)
    type program =
          (* a series of code blocks, with their types and labels *)
        { code : (class * code) StringMap.map,
          (* the entry point for the program *)
          entry : label,
          (* a series of data blocks, with their types and value *)
          data : (rtltype * Word8Vector.vector) StringMap.map }

end