
structure TALUtil =
struct

    val error_labels = 
        Params.flag true (SOME ("-error-labels",
                                "Generate labels for debugging tal code " ^
                                "(makes code much bigger)")) 
                          "error-labels"

    val newstring = HemlockUtil.newstring "__"
    val itos = Int.toString

    val pop_errors = (ref nil) : (string * int * string) list ref

    val one_true_label = newstring "one_true_error"

    (* used in marshall and talfns *)
    fun makecode f =
        let val (lab, targs, ret, args, code) = f ()
        in
            ["",
             lab ^ ":",
             "LABELTYPE <All[" ^
             StringUtil.delimit " " (targs @
             ["s1:Ts","s2:Ts","e1:Tcap","e2:Tcap"]) ^ "].code "
             ^ "{cap: &[e1,e2], " ^
                 "EBP: sptr (?E s2 e2), " ^
                 "ESP: sptr (?S " ^ ret ^ " " ^
             StringUtil.delimit "::" args ^ 
             "::se s1 s2 e1 e2)#(?E s2 e2)}>"] @
            code 
        end

    val indent = map (fn s => ("\t" ^ s))

    (* if debug mode is on, generate a new label and handler each time. 
       if off, just return our one true error handler.
       *)
    fun error n reason =
        (case !error_labels of
             true =>
                 let 
                     val lab = newstring "error"
                 in
                     pop_errors := (lab, n, reason) :: !pop_errors;
                     lab
                 end
           | false => one_true_label)

    (* call this after all other code that might call "error" has been
       run. *)
    local val ctr = ref 10000
    in
    fun code () =
        (case !error_labels of
             true =>
                 List.concat
                 (map (fn (lab, num, message) =>
                       let in
                         ctr := !ctr + 1;
                         [";; " ^ message,
                          lab ^ ":",
                          (* something to show in debugger. *)
                          "    MOV EAX,subsume(<B4>, " ^ itos (!ctr)
                                                     (* num *) ^ ")",
                          "    mov dword ptr [_watchme], eax",
                          
                          "    MOV EAX,DWORD PTR [" ^ TAL.exnpacket ^ "]",
                          "    FINIT",
                          "    MOV ESP,EBP",
                          "    POP EBX",
                          "    JMP EBX",
                          ""]
                       end) (!pop_errors)) before pop_errors := nil
           | false =>
                 [";; one true error handler",
                  one_true_label ^ ":",
                  "    MOV EAX,DWORD PTR [" ^ TAL.exnpacket ^ "]",
                  "    FINIT",
                  "    MOV ESP,EBP",
                  "    POP EBX",
                  "    JMP EBX",
                  ""])
    end

    fun data () =
        if !error_labels
        then
            ["_watchme:",
             "LABELTYPE <^*[B4^rw]>",
             "\tDD subsume(<B4>, 0)"]
        else nil

end