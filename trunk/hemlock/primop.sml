
structure Primop =
struct

    datatype compare =
        PEq
      | PNeq
      | PLess
      | PLesseq
      | PGreater
      | PGreatereq

    datatype binop =
        PTimes
      | PPlus
      | PMinus
      | PDiv
      | PMod
      | PCmp of compare

    datatype primop =
      (* primitive arithmetic stuff *)
        B of binop
      | PNeg

      (* client only *)
      | PWrite
      | PSubmit

      | PStdout

      (* string concatenation *)
      | PConcat

      (* exception handlers *)
      | PSethandler
      | PGethandler

      (* grid stuff *)
      | PForget


      (* takes code, vector of strings (cord ids).
         bool indicates "continue self" *)
      | PSpawn of bool
      | PWitness
      | PWaitall
      | PGetwitvec

      | PTerminate
      | PReturn of bool

      (* (un)marshalling *)
      | PMarshall
      | PUnmarshvec

      (* generate a random number *)
      | PNewtag

      (* do nothing, just bind a variable to value *)
      | PBind

      (* references *)
      | PSet
      | PGet
      | PRef

      (* arrays and vectors use these *)
      | PArray
      | PArray0
      | PSub
      | PUpdate
      | PArraySize


    type cpsprimop = primop

    (* XXX this should probably be factored out *)
    fun tostring (B PTimes) = "Times"
      | tostring (B PPlus) = "Plus"
      | tostring (B PMinus) = "Minus"
      | tostring (B PDiv) = "Div"
      | tostring (B PMod) = "Mod"
      | tostring (B (PCmp PEq)) = "Eq"
      | tostring (B (PCmp PNeq)) = "Neq"
      | tostring (B (PCmp PLess)) = "Less"
      | tostring (B (PCmp PLesseq)) = "Lesseq"
      | tostring (B (PCmp PGreater)) = "Greater"
      | tostring (B (PCmp PGreatereq)) = "Greatereq"

      | tostring PWrite = "Write"

      | tostring PStdout = "Stdout"

      | tostring PNeg = "Neg"
      | tostring PSethandler = "Sethandler"
      | tostring PGethandler = "Gethandler"

      | tostring PBind = "Bind"

      | tostring PSet = "Set"
      | tostring PGet = "Get"
      | tostring PRef = "Ref"

      | tostring PConcat = "Concat"

      | tostring PMarshall = "Marshall"
      | tostring PUnmarshvec = "Unmarshvec"

      | tostring PGetwitvec = "Getwitvec"

      | tostring PForget = "Forget"
      | tostring PTerminate = "Terminate"
      | tostring (PReturn b) = "Return_" ^ (if b then "fwd" else "ok")
      | tostring (PSpawn b) = "Spawn_" ^ (if b then "cont" else "new")
      | tostring PSubmit = "Submit"
      | tostring PWaitall = "Waitall"

      | tostring PArray = "Array"
      | tostring PArray0 = "Array0"
      | tostring PSub = "Sub"
      | tostring PUpdate = "Update"
      | tostring PArraySize "ArraySize"

      | tostring PWitness = "Witness"

      | tostring PNewtag = "Newtag"

end