
(* Marshall and Unmarshall ttt data. *) 

structure TALMarshall =
struct

    (* referring to popstuff/marsh.pop and popstuff/unmarsh.pop will be
       helpful when reading this, although I have made significant
       changes in order to support things that cannot be directly
       encoded in popcorn. *)


    structure TU = TALUtil

    val tag_integer = 0
    val tag_string = 1
    val tag_ref = 2
    val tag_intt = 3
    val tag_tuple = 4
    val tag_code = 5

    val itos = Int.toString

    val newstring = HemlockUtil.newstring "__"

    (* **** code for marshalling **** *)

    val writeint = newstring "writeint"
    val pushout = newstring "pushout"
    val concat = newstring "concat"
    val getid = newstring "getid"
    val marshall = newstring "marshall"
    val getcode = newstring "getcode"

    (* this could be improved by creating a sorted structure *)
    fun getcode_code () =
        (getcode, nil,
         "B4",
         ["(" ^ TAL.codetype ^ ")"],
         [
          (* ret :: c ... *)
          (* esi = counter
             edi = ctab base 
             ecx = code in question
             eax, ebx scratch
             *)
          
          "   mov ecx, [esp+4]",

          "   mov esi, subsume(<B4>,0)",

          "   fallthru <s1,s2,e1,e2>",
          "getcode_again:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2], " ^
          "ESI : B4, " ^ 
(*          "EDI : ^*[S(gcsize)^r, (^*[array(gcsize, (" ^ TAL.codetype ^ "))])^r], " ^*)
(*        "EDI : B4, " ^ *)
          "ECX : (" ^ TAL.codetype ^ "), " ^
          "EBP: sptr (?E s2 e2), ESP: sptr (?S B4 (All[vanswer:T4 vdep:T4 vdlis:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap] . (?hemcode vanswer vdep vdlis s1 s2 e1 e2))::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   mov edi, ctab",
          "   unpack gcsize, edi, edi",
          "   mov ebx, esi",
          "   unpack gci, ebx, ebx",
          "   cmp ebx, [edi]",
          "   jae " ^ TALUtil.error 0x2221 "array bounds (code not found??)",
          "   mov eax, [edi + 4]",
          "   mov eax, [eax + 4*ebx]",

          "   cmp eax, ecx",
          "   je getcode_out",

          (* not equal *)
          "   inc esi",
          "   jmp tapp(getcode_again,<s1,s2,e1,e2>)",

          "getcode_out:",
          "   ;; found it!",
          "   mov eax, esi",
          "   retn"])

(*
          "    mov ebx, ctab",
          "    unpack unm5size, ebx, ebx",
          "    unpack unm5i, eax, eax",
          "    cmp eax, [ebx]",
          "    jae " ^ popTALUtil.error ^ " ;; array bounds",
          "    mov ebx, [ebx + 4]",
          "    mov ebx, [ebx + 4*eax]",
*)

    fun marshall_code () =
        (marshall, nil,
         "?str",
         (* arg to marshall, newstring *)
         ["`ttt", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})"],
         [
          (* ret :: ttt :: ns ... *)
          (* initialize globals -- marshall is not thread safe *)
(*
          "   MOV EAX,rollsum(<`tttlist?>,0)",
          "   MOV DWORD PTR [_waitq],EAX",
          "   MOV EAX,rollsum(<`tttlist?>,0)", 
          "   MOV DWORD PTR [_waitqlast],EAX",
*)
          "   MOV EAX,rollsum(<`stringlist?>,0)",
          "   MOV DWORD PTR [_outq],EAX",
          "   MOV EAX,subsume(<B4>,1)",
          "   MOV DWORD PTR [_nextindex],EAX",

          (* oops, getid assumes tmap not null! *)
(*
          "   MOV EAX,rollsum(<`tttmap?>,0)",
          "   MOV DWORD PTR [_tmap],EAX",
          "   PUSH DWORD PTR [ESP+4]",
          (* ttt :: ret :: ttt :: ns ... *)
          (* put root on queue. this will always return 0 *)
          "   CALL tapp(" ^ getid ^ ",<ESP 1 4 s1,EBP 1,e1,e2>)",
          "   ADD ESP,4",
*)
          (* tmap := new(root, 0, null, null) *)
          "   MALLOC tmap_mptr,16",
          "   MOV ESI,EAX",
          "   MOV EAX,[ESP+4]",
          "   MOV [ESI],EAX",
          "   MOV EAX,subsume(<B4>,0)",
          "   MOV [ESI+4],EAX",
          "   MOV EAX,rollsum(<`tttmap?>,0)",
          "   MOV [ESI+8],EAX",
          "   MOV EAX,rollsum(<`tttmap?>,0)",
          "   MOV [ESI+12],EAX",
          "   MOV EAX,ESI",
          "   FORGETUNIQUE tmap_mptr",
          "   COERCE rollsum(<`tttmap?>,roll(<^T(0)`tttmap?mem>,forgetname(EAX)))",
          "   MOV DWORD PTR [_tmap],EAX",

          (* waitqlast = waitq = new(root, null) *)
          "   MALLOC wq_mptr,8",
          "   MOV ESI,EAX",
          "   MOV EAX,[ESP+4]",
          "   MOV [ESI],EAX",
          "   MOV EAX,rollsum(<`tttlist?>,0)",
          "   MOV [ESI+4],EAX",
          "   MOV EAX,ESI",
          "   FORGETUNIQUE wq_mptr",
          "   COERCE rollsum(<`tttlist?>,roll(<^T(0)`tttlist?mem>,forgetname(EAX)))",
          "   MOV DWORD PTR [_waitqlast],EAX",
          "   MOV DWORD PTR [_waitq],EAX",

          (* ret :: ttt :: ns ... *)
          "   JMP whiletest$13",
          "whilebody$12:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str `ttt::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",
          (* XXX these two projections could be done with one null check *)
          "   MOV EAX,DWORD PTR unroll(unroll([_waitq]))",
          "   NAMEOBJ n$15,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x5628 "null pointer -- impossible",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$15",
          "   PUSH DWORD PTR [EAX]", (* waitq.head *)
          (* t :: ret :: ttt :: ns ... *)
          "   MOV EAX,DWORD PTR unroll(unroll([_waitq]))",
          "   NAMEOBJ n$16,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x5977 "null pointer -- impossible",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$16",
          "   MOV EAX,[EAX+4]",
          "   MOV DWORD PTR [_waitq],EAX", (* waitq := waitq.next *)
(*        "   MOV EBX,DWORD PTR [_waitq]", *)
          "   mov ebx, eax",
          "   MOV EAX,rollsum(<`tttlist?>,0)",
          "   CMP EBX,EAX",
          "   JNE iffalse$18",
          "iftrue$17:",
          (* waitq = null *)
          "   MOV EAX,rollsum(<`tttlist?>,0)",
          "   MOV DWORD PTR [_waitqlast],EAX", (* waitqlast := 0 *)
          "   JMP ifend$19",
          "iffalse$18:",
          "   FALLTHRU <s1,s2,e1,e2>",
          "tc_fast$20:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `ttt::(?S ?str `ttt::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "ifend$19:",
          (* t :: ret :: ttt :: ns ... *)
          (* check tag *)
          "   MOV EAX,unroll([ESP])", (* t *)
          "   NAMEOBJ uptr$23,EAX",
          "   MOV ECX,[EAX]",
          "   CMP ECX," ^ itos TAL.SUMTAG,
          "   JE sumtag_value",
          "   CMP ECX," ^ itos (TAL.tagfor RTL.CODE),
          "   JE code_value$28",
          "   CMP ECX," ^ itos TAL.TUPLETAG,
          "   JE aaa_value$27",
          "   CMP ECX," ^ itos (TAL.tagfor RTL.REF),
          "   JE rrr_value$24",
          "   CMP ECX," ^ itos (TAL.tagfor RTL.STRING),
          "   JE sss_value$26",
          "   CMP ECX," ^ itos (TAL.tagfor RTL.INT),
          "   JE iii_value$25",
          (*  (and INDTAG?) *)
          "   JMP default$22",

          "sumtag_value:",

          (* t :: ret :: ttt :: ns ... *)

          "   mov esi,eax",
          "   mov ebx,[rec(forgetname(esi))+4]",
          "   mov eax,[rec(forgetname(esi))+8]",
          "   removename uptr$23",
          "   push ebx",
          "   push eax",
          "   push subsume(<B4>,12)", (* sizeof (intt) *)

          (* 12 :: ttt :: iii :: t :: ret :: ttt :: ns ... *)
          "   mov eax,[esp+24]", (* newstr *)
          "   call tapp(eax, <ESP 1 7 s1, EBP 1, e1, e2>)",
          "   mov [esp],eax",
          (* s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)
          "   push dword ptr [esp+4]",
          (* ttt :: s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)
          "   call tapp(" ^ getid ^ ",<ESP 1 8 s1,EBP 1,e1,e2>)",
          "   mov [esp],eax",
          (* getid(ttt) :: s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)

          (* write tag @ 0 *)
          "   push subsume(<B4>," ^ itos tag_intt ^ ")",
          "   push subsume(<B4>,0)",
          "   push dword ptr [esp+12]",
          (* ss :: 0 :: tag :: getid(ttt) :: s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)
          "   call tapp(" ^ writeint ^ ",<ESP 3 11 s1,EBP 1,e1,e2>)",
          "   add esp,12",
          (* getid(ttt) :: s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)

          (* write getid(ttt) at 8 *)
          "   push subsume(<B4>,8)",
          "   push dword ptr [esp+8]",
          (* s :: 8 :: getid(ttt) :: s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)
          "   call tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   add esp,12",
          (* s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)

          (* write int at 4 *)
          "   push dword ptr [esp+8]", (* iii *)
          "   push subsume(<B4>,4)",
          "   push dword ptr [esp+8]", (* ss *)
          (* s :: 4 :: iii :: s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)
          "   call tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   add esp,12",
          (* s :: ttt :: iii :: t :: ret :: ttt :: ns ... *)

          (* push onto output (s already at top of tal stack) *)
          "   call tapp(" ^ pushout ^ ",<ESP 1 7 s1,EBP 1,e1,e2>)",
          "   add esp,12",
          (* t :: ret :: ttt :: ns ... *)

          "   jmp tapp(endswitch$21,<s1,s2,e1,e2>)",

          "code_value$28:",
          "   MOV EAX,[rec(forgetname(EAX))+4]",
          "   REMOVENAME uptr$23",
          "   PUSH EAX",
          (* c :: t :: ret :: ttt :: ns ... *)
          "   PUSH subsume(<B4>,8)", (* sizeof (code) *)

          (* without these three lines, tal can't decide that
             two beta-eta equal types are in fact equal *)
          "   FALLTHRU <s1,s2,e1,e2>",
          newstring "tal_is_dumb" ^ ":",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::(" ^ TAL.codetype ^ ")::`ttt::(?S ?str `ttt::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",

          (* 8 :: c :: t :: ret :: ttt :: ns ... *)
          "   MOV EAX,[ESP+20]", (* newstring *)
          "   CALL tapp(EAX,<ESP 1 6 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          (* ss :: c :: t :: ret :: ttt :: ns ... *)
          (* writeint(ss, 0, tag_code) *)
          "   PUSH subsume(<B4>," ^ itos tag_code ^ ")",
          "   PUSH subsume(<B4>,0)",
          "   PUSH DWORD PTR [ESP+8]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 9 s1,EBP 1,e1,e2>)",
          "   ADD ESP,12",
          (* ss :: c :: t :: ret :: ttt :: ns ... *)
          "   PUSH DWORD PTR [ESP]",
          "   PUSH subsume(<B4>,4)",
          "   PUSH DWORD PTR [ESP+12]",
          (* c :: 4 :: ss :: ss :: c :: t :: ret :: ttt :: ns ... *)
          "   CALL tapp(" ^ getcode ^ ",<ESP 1 9 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          (* getcode(c) :: 4 :: ss :: ss :: c :: t :: ret :: ttt :: ns ... *)
          "   push dword ptr [esp+4]",
          "   push dword ptr [esp+12]",
          (* ss :: 4 :: getcode(c) :: 4 :: ss :: ss :: c :: t :: ret :: ttt :: ns ... *)
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 11 s1,EBP 1,e1,e2>)",
          "   add esp,20",
          (* ss :: c :: t :: ret :: ttt :: ns ... *)
          "   CALL tapp(" ^ pushout ^ ",<ESP 1 6 s1,EBP 1,e1,e2>)",
          "   add esp, 8",
          (* t :: ret :: ttt :: ns ... *)
          "   JMP tapp(endswitch$21,<s1,s2,e1,e2>)",
          "aaa_value$27:",

          (* pretty much straight from popcorn *)
          "   MOV EAX,[rec(forgetname(EAX))+4]",
          "   REMOVENAME uptr$23",
          "   PUSH EAX",
          "   MOV EAX,[ESP]",
          "   UNPACK ?sz$29,EAX,EAX",
          "   PUSH DWORD PTR subsume(<B4>,[EAX])",
          "   PUSH subsume(<B4>,8)",
          "   PUSH DWORD PTR [ESP+4]",
          "   MOV EAX,subsume(<B4>,2)",
          "   MOV ECX,EAX",
          "   POP EAX",
          "   SHL EAX,CL",
          "   POP EBX",
          "   ADD EAX,EBX",
          "   PUSH EAX",
          "   MOV EAX,[ESP+24]",
          "   CALL tapp(EAX,<ESP 1 7 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          "   PUSH subsume(<B4>," ^ itos tag_tuple ^ ")",
          "   PUSH subsume(<B4>,0)",
          "   PUSH DWORD PTR [ESP+8]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   ADD ESP,12",
          "   PUSH DWORD PTR [ESP+4]",
          "   PUSH subsume(<B4>,4)",
          "   PUSH DWORD PTR [ESP+8]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   ADD ESP,8",
          "   MOV DWORD PTR [ESP],subsume(<B4>,0)",
          (* u :: s :: len :: ... *)
          "   JMP fortest$30",
          "forbody$31:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::?str::B4::(?arr `ttt)::`ttt::(?S ?str `ttt::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   PUSH DWORD PTR [ESP+4]",
          (* s :: u :: s :: len :: ... *)
          "   PUSH subsume(<B4>,8)",
          (* 8 :: s :: u :: s :: len :: ... *)
          "   PUSH DWORD PTR [ESP+8]", (* was 16. but want u, not len *)
          (* u :: 8 :: s :: u :: s :: len :: ... *)
          "   MOV EAX,subsume(<B4>,2)",
          "   MOV ECX,EAX",
          "   POP EAX",
          "   SHL EAX,CL",
          "   POP EBX",
          "   ADD EAX,EBX",
          "   PUSH EAX",
          "   MOV EBX,[ESP+20]",
          "   MOV EAX,[ESP+8]",
          "   UNPACK ?sz$34,EBX,EBX",
          "   UNPACK i$35,EAX,EAX",
          "   CMP EAX,[EBX]",
          "   JAE " ^ TALUtil.error 0x14286 "array bounds",
          "   MOV EBX,[EBX+4]",
          "   PUSH DWORD PTR [EBX+4*EAX]",
          "   CALL tapp(" ^ getid ^ ",<ESP 1 11 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          "   PUSH DWORD PTR [ESP]",
          "   PUSH DWORD PTR [ESP+8]",
          "   PUSH DWORD PTR [ESP+16]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 14 s1,EBP 1,e1,e2>)",
          "   ADD ESP,24",
          "forcount$32:",
          "   MOV EBX,[ESP]",
          "   MOV EAX,subsume(<B4>,1)",
          "   ADD EAX,EBX",
          "   MOV [ESP],EAX",
          "   PUSH EAX",
          "   MOV EAX,subsume(<B4>,1)",
          "   SUB [ESP],EAX",
          "   POP EAX",
          "fortest$30:",
          "   MOV EBX,[ESP]",
          "   MOV EAX,[ESP+8]",
          "   CMP EBX,EAX",
          "   JL tapp(forbody$31,<s1,s2,e1,e2>)",
          "forend$33:",
          "   ADD ESP,4",
          "   PUSH DWORD PTR [ESP]",
          "   CALL tapp(" ^ pushout ^ ",<ESP 1 8 s1,EBP 1,e1,e2>)",
          "   ADD ESP,16",
          "   JMP tapp(endswitch$21,<s1,s2,e1,e2>)",
          
          "rrr_value$24:",
          (* t :: ret :: ttt :: ns ... *)

          "   MOV EAX,[rec(forgetname(EAX))+4]",
          "   REMOVENAME uptr$23",
          "   PUSH EAX",
          "   PUSH subsume(<B4>,8)", (* sizeof (ref) *)
          (* 8 :: x :: t :: ret :: ttt :: ns ... *)
          "   MOV EAX,[ESP+20]",
          "   CALL tapp(EAX,<ESP 1 6 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          "   PUSH DWORD PTR [ESP+4]",
          (* x :: s :: x :: t :: ret :: ttt :: ns ... *)
          "   CALL tapp(" ^ getid ^ ",<ESP 1 7 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          (* getid(x) :: s :: x :: t :: ret :: ttt :: ns ... *)
          (* write tag @ 0 *)
          "   PUSH subsume(<B4>," ^ itos tag_ref ^ ")",
          "   PUSH subsume(<B4>,0)",
          "   PUSH DWORD PTR [ESP+12]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   ADD ESP,12",
          (* write getid(x) at 4 *)
          "   PUSH DWORD PTR [ESP]",
          "   PUSH subsume(<B4>,4)",
          "   PUSH DWORD PTR [ESP+12]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   ADD ESP,12",
          "   PUSH DWORD PTR [ESP+4]",
          (* push *)
          "   CALL tapp(" ^ pushout ^ ",<ESP 1 8 s1,EBP 1,e1,e2>)",
          "   ADD ESP,16",
          "   JMP tapp(endswitch$21,<s1,s2,e1,e2>)",
          
          "sss_value$26:",
          (* essentially straight from popcorn *)
          
          "   MOV EAX,[rec(forgetname(EAX))+4]",
          "   REMOVENAME uptr$23",
          "   PUSH EAX",
          "   MOV EAX,[ESP]",
          "   UNPACK ?sz$36,EAX,EAX",
          "   PUSH DWORD PTR subsume(<B4>,[EAX])",
          "   MOV EAX,[ESP]",
          "   ADD EAX,8",
          "   PUSH EAX",
          "   MOV EAX,[ESP+24]",
          "   CALL tapp(EAX,<ESP 1 7 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          "   PUSH subsume(<B4>," ^ itos tag_string ^ ")",
          "   PUSH subsume(<B4>,0)",
          "   PUSH DWORD PTR [ESP+8]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   ADD ESP,12",
          "   PUSH DWORD PTR [ESP+4]",
          "   PUSH subsume(<B4>,4)",
          "   PUSH DWORD PTR [ESP+8]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   ADD ESP,8",
          "   MOV DWORD PTR [ESP],subsume(<B4>,0)",
          "   JMP fortest$37",
          "forbody$38:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::?str::B4::?str::`ttt::(?S ?str `ttt::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   PUSH DWORD PTR [ESP+4]",
          "   MOV EBX,[ESP+4]",
          "   MOV EAX,subsume(<B4>,8)",
          "   ADD EAX,EBX",
          "   PUSH EAX",
          "   MOV EBX,[ESP+20]",
          "   MOV EAX,[ESP+8]",
          "   UNPACK ?sz$41,EBX,EBX",
          "   UNPACK i$42,EAX,EAX",
          "   CMP EAX,[EBX]",
          "   JAE " ^ TALUtil.error 0x18501 "array bounds",
          "   MOV EBX,[EBX+4]",
          "   MOV ECX,EAX",
          "   MOV EAX,0",
          "   MOV AL,[EBX+ECX]",
          "   POP EDI",
          "   POP ESI",
          "   UNPACK ?sz$43,ESI,ESI",
          "   UNPACK i$44,EDI,EDI",
          "   CMP EDI,[ESI]",
          "   JAE " ^ TALUtil.error 0x18824 "array bounds",
          "   MOV ESI,[ESI+4]",
          "   MOV [ESI+EDI],AL",
          "forcount$39:",
          "   MOV EBX,[ESP]",
          "   MOV EAX,subsume(<B4>,1)",
          "   ADD EAX,EBX",
          "   MOV [ESP],EAX",
          "   PUSH EAX",
          "   MOV EAX,subsume(<B4>,1)",
          "   SUB [ESP],EAX",
          "   POP EAX",
          "fortest$37:",
          "   MOV EBX,[ESP]",
          "   MOV EAX,[ESP+8]",
          "   CMP EBX,EAX",
          "   JL tapp(forbody$38,<s1,s2,e1,e2>)",
          "forend$40:",
          "   ADD ESP,4",
          "   PUSH DWORD PTR [ESP]",
          "   CALL tapp(" ^ pushout ^ ",<ESP 1 8 s1,EBP 1,e1,e2>)",
          "   ADD ESP,16",
          "   JMP tapp(endswitch$21,<s1,s2,e1,e2>)",

          "iii_value$25:",
          
          (* t :: ret :: ttt :: ns ... *)
          
          "   MOV EAX,[rec(forgetname(EAX))+4]",
          "   REMOVENAME uptr$23",
          "   PUSH EAX",
          "   PUSH subsume(<B4>,8)", (* sizeof int *)
          (* 8 :: i :: t :: ret :: ttt :: ns ... *)
          "   MOV EAX,[ESP+20]",
          "   CALL tapp(EAX,<ESP 1 6 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          (* ss :: i :: t :: ret :: ttt :: ns ... *)
          (* write_int(s, 0, int_tag) *)
          "   PUSH subsume(<B4>," ^ itos tag_integer ^ ")",
          "   PUSH subsume(<B4>,0)",
          "   PUSH DWORD PTR [ESP+8]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 9 s1,EBP 1,e1,e2>)",
          "   ADD ESP,12",
          (* ss :: i :: t :: ret :: ttt :: ns ... *)
          (* write_int(s, 4, i) *)
          "   PUSH DWORD PTR [ESP+4]", (* i *)
          "   PUSH subsume(<B4>,4)",
          "   PUSH DWORD PTR [ESP+8]", (* ss *)
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 9 s1,EBP 1,e1,e2>)",
          "   ADD ESP,12",
          (* and push *)
          "   PUSH DWORD PTR [ESP]",
          "   CALL tapp(" ^ pushout ^ ",<ESP 1 7 s1,EBP 1,e1,e2>)",
          "   ADD ESP,12",
          "   JMP tapp(endswitch$21,<s1,s2,e1,e2>)",
          
          "default$22:",
          "   FALLTHRU <s1,s2,e1,e2>",
          "endswitch$21:",
          (* t :: ret :: ttt :: ns ... *)
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `ttt::(?S ?str `ttt::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   ADD ESP,4",
          "whiletest$13:",
          (* ret :: ttt :: ns ... *)
          "   MOV EBX,DWORD PTR [_waitq]",
          "   MOV EAX,rollsum(<`tttlist?>,0)",
          "   CMP EBX,EAX",
          "   JNE tapp(whilebody$12,<s1,s2,e1,e2>)",
          "whileend$14:",
          "   PUSH DWORD PTR [ESP+8]", (* newstring *)
          "   PUSH DWORD PTR [_outq]",
          (* return concat_list(outq, ns) *)
          "   CALL tapp(" ^ concat ^ ",<ESP 2 5 s1,EBP 1,e1,e2>)",
          "   ADD ESP,8",
          "   RETN"])
        
    fun getid_code () =
        (getid, nil,
         "B4",
         ["`ttt"],
         ["   PUSH subsume(<B4>,-1)",
          "   PUSH DWORD PTR [_tmap]",
          (* tmp :: i :: ret :: ttt :: ... *)
          "   JMP whiletest$46",
          "whilebody$45:",
          (* tmp :: i :: ret :: ttt :: ... *)
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `tttmap?::B4::(?S B4 `ttt::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   MOV EAX,unroll(unroll([ESP]))", (* tmp *)
          "   NAMEOBJ n$51,EAX",
          "   CMP EAX,0",
          (* this is raised *)
          "   JE " ^ TALUtil.error 0x22481 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$51",
          "   MOV EAX, [EAX]", (* tmp.t *)
          "   MOV EBX, [ESP+12]", (* t arg *)
          "   CMP EAX, EBX",
          "   JNE iffalse$49", (* tmp.t = t ? *)
          "iftrue$48:",
          (* tmp.t = t ! *)
          "   MOV EAX,unroll(unroll([ESP]))",
          "   NAMEOBJ n$52,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x22927 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$52",
          "   MOV EAX,[EAX+4]", (* tmp.i *)
          "   ADD ESP,8",
          "   RETN",
          "iffalse$49:",
          (* tmp :: i :: ret :: ttt :: ... *)
          "   FALLTHRU <s1,s2,e1,e2>",
          "tc_fast$53:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `tttmap?::B4::(?S B4 `ttt::se s1 s2 e1 e2)#(?E s2 e2)}>",
          (* t <> tmp.t *)

          "   MOV EAX,unroll(unroll([ESP]))" (* tmp *),
          "   NAMEOBJ n$57,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x23582 "null pointer - impossible",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$57",
          "   MOV EBX, [ESP+12]", (* t *)
          "   MOV EAX, [EAX]",
          "   CMP EAX, EBX",
          "   JGE iffalse$55", (* XXX right order? doesn't really matter,
                                  we just need something consistent, and
                                  this is the only such test. *)
          "iftrue$54:",
          (* less *)
          "   MOV EAX,unroll(unroll([ESP]))",
          "   NAMEOBJ n$61,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x24181 "null pointer - impossible",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$61",
          "   MOV EBX,[EAX+8]", (* tmp.l *)
          "   MOV EAX,rollsum(<`tttmap?>,0)", (* null *)
          "   CMP EBX,EAX",
          "   JE iffalse$59",
          "iftrue$58:",
          (* nb, original code was wrong, so I changed to JE instead of JNE *)  
          (* not null *)
          "   MOV EAX,unroll(unroll([ESP]))",
          "   NAMEOBJ n$62,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x24710 "null pointer - still impossible",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$62",
          "   MOV EAX,[EAX+8]",
          "   MOV [ESP],EAX", (* tmp := tmp.l *)
          "   JMP ifend$60",
          "iffalse$59:",
          (* less, is null *)
          "   FALLTHRU <s1,s2,e1,e2>",
          "tc_fast$63:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `tttmap?::B4::(?S B4 `ttt::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   MOV EAX,DWORD PTR [_nextindex]",
          "   MOV [ESP+4],EAX", (* i := nextindex *)
          "   PUSH DWORD PTR [ESP]", (* why? *)
          (* tmp :: tmp :: i :: ret :: ttt :: ... *)
          "   MALLOC mptr$64,16",
          "   MOV ESI,EAX",
          "   MOV EAX,[ESP+16]", (* t *)
          "   MOV [ESI],EAX",
          "   MOV EAX,[ESP+8]", (* i *)
          "   MOV [ESI+4],EAX",
          "   MOV EAX,rollsum(<`tttmap?>,0)", (* null *)
          "   MOV [ESI+8],EAX",
          "   MOV EAX,rollsum(<`tttmap?>,0)", (* null *)
          "   MOV [ESI+12],EAX",
          "   MOV EAX,ESI",
          "   FORGETUNIQUE mptr$64",
          "   COERCE rollsum(<`tttmap?>,roll(<^T(0)`tttmap?mem>,forgetname(EAX)))",
          "   POP ESI", (* tmp *)
          (* tmp :: i :: ret :: ttt :: ... *)
          "   COERCE unroll(unroll(ESI))", 
          "   NAMEOBJ n$65,ESI",
          "   CMP ESI,0",
          "   JE " ^ TALUtil.error 0x26161 "null pointer",
          "   COERCE forgetname(ESI)",
          "   REMOVENAME n$65",
          "   MOV [ESI+8],EAX", (* tmp.l := new *)
          "ifend$60:",
          (* tmp :: i :: ret :: ttt :: ... *)
          "   JMP ifend$56",
          "iffalse$55:",
          "   FALLTHRU <s1,s2,e1,e2>",
          "tc_fast$66:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `tttmap?::B4::(?S B4 `ttt::se s1 s2 e1 e2)#(?E s2 e2)}>",
          (* greater *)
          "   MOV EAX,unroll(unroll([ESP]))",
          "   NAMEOBJ n$70,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x26814 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$70",
          "   MOV EBX,[EAX+12]",
          "   MOV EAX,rollsum(<`tttmap?>,0)",
          "   CMP EBX,EAX",
          "   JE iffalse$68", (* ditto -- original code was wrong *)
          "iftrue$67:",
          (* not null *)
          "   MOV EAX,unroll(unroll([ESP]))",
          "   NAMEOBJ n$71,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x27266 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$71",
          "   MOV EAX,[EAX+12]",
          "   MOV [ESP],EAX", (* tmp := tmp.r *)
          "   JMP ifend$69",
          "iffalse$68:",
          "   FALLTHRU <s1,s2,e1,e2>",
          "tc_fast$72:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `tttmap?::B4::(?S B4 `ttt::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   MOV EAX,DWORD PTR [_nextindex]",
          "   MOV [ESP+4],EAX", (* i := nextindex *)
          "   PUSH DWORD PTR [ESP]",
          (* tmp :: tmp :: i :: ret :: ttt :: ... *)
          "   MALLOC mptr$73,16",
          "   MOV ESI,EAX",
          "   MOV EAX,[ESP+16]",
          "   MOV [ESI],EAX",
          "   MOV EAX,[ESP+8]",
          "   MOV [ESI+4],EAX",
          "   MOV EAX,rollsum(<`tttmap?>,0)",
          "   MOV [ESI+8],EAX",
          "   MOV EAX,rollsum(<`tttmap?>,0)",
          "   MOV [ESI+12],EAX",
          "   MOV EAX,ESI",
          "   FORGETUNIQUE mptr$73",
          "   COERCE rollsum(<`tttmap?>,roll(<^T(0)`tttmap?mem>,forgetname(EAX)))",
          "   POP ESI",
          "   COERCE unroll(unroll(ESI))",
          "   NAMEOBJ n$74,ESI",
          "   CMP ESI,0",
          "   JE " ^ TALUtil.error 0x28563 "null pointer",
          "   COERCE forgetname(ESI)",
          "   REMOVENAME n$74",
          "   MOV [ESI+12],EAX", (* tmp.r := new *)
          "ifend$69:",
          "ifend$56:",
          (* tmp :: i :: ret :: ttt :: ... *)
          "ifend$50:",
          "whiletest$46:",
          (* tmp :: i :: ret :: ttt :: ... *)
          "   MOV EBX,[ESP+4]",
          "   MOV EAX,subsume(<B4>,0)",
          "   CMP EBX,EAX",
          (* while i < 0 *)
          "   JL tapp(whilebody$45,<s1,s2,e1,e2>)",
          "whileend$47:",
          (* done with loop because i >= 0 *)
          (* tmp :: i :: ret :: ttt :: ... *)

          "   MOV EBX,DWORD PTR [_nextindex]",
          "   MOV EAX,subsume(<B4>,1)",
          "   ADD EAX,EBX",
          "   MOV DWORD PTR [_nextindex],EAX", (* nextindex++ *)
          "   PUSH EAX", (* XXX ++ weirdness *)
          "   MOV EAX,subsume(<B4>,1)",
          "   SUB [ESP],EAX",
          "   POP EAX",
          (* tmp :: i :: ret :: ttt :: ... *)

          (* push onto queue *)

          "   MOV EBX,DWORD PTR [_waitq]",
          "   MOV EAX,rollsum(<`tttlist?>,0)",
          "   CMP EBX,EAX",
          "   JNE iffalse$76",
          "iftrue$75:",
          (* is null *)
          "   MALLOC mptr$78,8", (* new tttlist *)
          "   MOV ESI,EAX",
          "   MOV EAX,[ESP+12]", (* t *)
          "   MOV [ESI],EAX",
          "   MOV EAX,rollsum(<`tttlist?>,0)",
          "   MOV [ESI+4],EAX",
          "   MOV EAX,ESI",
          "   FORGETUNIQUE mptr$78",
          "   COERCE rollsum(<`tttlist?>,roll(<^T(0)`tttlist?mem>,forgetname(EAX)))",
          "   MOV DWORD PTR [_waitqlast],EAX",
          "   MOV DWORD PTR [_waitq],EAX",
          "   JMP ifend$77",
          "iffalse$76:",
          "   FALLTHRU <s1,s2,e1,e2>",
          "tc_fast$79:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `tttmap?::B4::(?S B4 `ttt::se s1 s2 e1 e2)#(?E s2 e2)}>",
          (* isn't null *)
          "   PUSH DWORD PTR [_waitqlast]", 
          (* wqlast :: tmp :: i :: ret :: ttt :: ... *)
          "   MALLOC mptr$80,8",
          "   MOV ESI,EAX",
          "   MOV EAX,[ESP+16]", (* t *)
          "   MOV [ESI],EAX",
          "   MOV EAX,rollsum(<`tttlist?>,0)",
          "   MOV [ESI+4],EAX",
          "   MOV EAX,ESI",
          "   FORGETUNIQUE mptr$80",
          "   COERCE rollsum(<`tttlist?>,roll(<^T(0)`tttlist?mem>,forgetname(EAX)))",
          "   POP ESI",
          "   COERCE unroll(unroll(ESI))",
          "   NAMEOBJ n$81,ESI",
          "   CMP ESI,0",
          "   JE " ^ TALUtil.error 0x31191 "null pointer",
          "   COERCE forgetname(ESI)",
          "   REMOVENAME n$81",
          "   MOV [ESI+4],EAX", (* waitqlast.next := new *)
          "   MOV EAX,DWORD PTR unroll(unroll([_waitqlast]))",
          "   NAMEOBJ n$82,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x31495 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$82",
          "   MOV EAX,[EAX+4]", (* waitqlast.next *)
          "   MOV DWORD PTR [_waitqlast],EAX", (* waitqlast := waitqlast.next *)
          "ifend$77:",
          "   MOV EAX,[ESP+4]", (* i *)
          "   ADD ESP,8",
          "   RETN"])

    (* directly from popcorn *)
    fun writeint_code () =
        (writeint, nil,
         "B4",
         ["?str", "B4", "B4"],
         TALUtil.indent
         ["PUSH DWORD PTR [ESP+4]",
          "PUSH DWORD PTR [ESP+12]",
          "PUSH DWORD PTR [ESP+20]",
          "MOV EAX,subsume(<B4>,24)",
          "MOV ECX,EAX",
          "POP EAX",
          "SAR EAX,CL",
          "PUSH EAX",
          "MOV EAX,subsume(<B4>,255)",
          "POP EBX",
          "AND EAX,EBX",
          "POP EDI",
          "POP ESI",
          "UNPACK ?sz$3,ESI,ESI",
          "UNPACK i$4,EDI,EDI",
          "CMP EDI,[ESI]",
          "JAE " ^ TALUtil.error 0x32458 "array bounds",
          "MOV ESI,[ESI+4]",
          "MOV [ESI+EDI],AL",
          "PUSH DWORD PTR [ESP+4]",
          "MOV EBX,[ESP+12]",
          "MOV EAX,subsume(<B4>,1)",
          "ADD EAX,EBX",
          "PUSH EAX",
          "PUSH DWORD PTR [ESP+20]",
          "MOV EAX,subsume(<B4>,16)",
          "MOV ECX,EAX",
          "POP EAX",
          "SAR EAX,CL",
          "PUSH EAX",
          "MOV EAX,subsume(<B4>,255)",
          "POP EBX",
          "AND EAX,EBX",
          "POP EDI",
          "POP ESI",
          "UNPACK ?sz$5,ESI,ESI",
          "UNPACK i$6,EDI,EDI",
          "CMP EDI,[ESI]",
          "JAE " ^ TALUtil.error 0x33103 "array bounds",
          "MOV ESI,[ESI+4]",
          "MOV [ESI+EDI],AL",
          "PUSH DWORD PTR [ESP+4]",
          "MOV EBX,[ESP+12]",
          "MOV EAX,subsume(<B4>,2)",
          "ADD EAX,EBX",
          "PUSH EAX",
          "PUSH DWORD PTR [ESP+20]",
          "MOV EAX,subsume(<B4>,8)",
          "MOV ECX,EAX",
          "POP EAX",
          "SAR EAX,CL",
          "PUSH EAX",
          "MOV EAX,subsume(<B4>,255)",
          "POP EBX",
          "AND EAX,EBX",
          "POP EDI",
          "POP ESI",
          "UNPACK ?sz$7,ESI,ESI",
          "UNPACK i$8,EDI,EDI",
          "CMP EDI,[ESI]",
          "JAE " ^ TALUtil.error 0x33747 "array bounds",
          "MOV ESI,[ESI+4]",
          "MOV [ESI+EDI],AL",
          "PUSH DWORD PTR [ESP+4]",
          "MOV EBX,[ESP+12]",
          "MOV EAX,subsume(<B4>,3)",
          "ADD EAX,EBX",
          "PUSH EAX",
          "MOV EBX,[ESP+20]",
          "MOV EAX,subsume(<B4>,255)",
          "AND EAX,EBX",
          "POP EDI",
          "POP ESI",
          "UNPACK ?sz$9,ESI,ESI",
          "UNPACK i$10,EDI,EDI",
          "CMP EDI,[ESI]",
          "JAE " ^ TALUtil.error 0x34235 "array bounds",
          "MOV ESI,[ESI+4]",
          "MOV [ESI+EDI],AL",
          "MOV EAX,subsume(<B4>,0)",
          "RETN"])

    fun pushout_code () =
        (pushout, nil,
         "B4",
         ["?str"],
         TALUtil.indent
         ["MALLOC mptr$11,8",
          "MOV ESI,EAX",
          "MOV EAX,[ESP+4]",
          "MOV [ESI],EAX",
          "MOV EAX,DWORD PTR [_outq]",
          "MOV [ESI+4],EAX",
          "MOV EAX,ESI",
          "FORGETUNIQUE mptr$11",
          "COERCE rollsum(<`stringlist?>," ^ 
                          "roll(<^T(0)`stringlist?mem>,forgetname(EAX)))",
          "MOV DWORD PTR [_outq],EAX",
          "MOV EAX,subsume(<B4>,0)",
          "RETN"])

    fun concat_code () =
        (concat, nil,
         "?str",
         ["`stringlist?",
          "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})"],
         [
          "   PUSH subsume(<B4>,0)",
          "   PUSH subsume(<B4>,0)",
          "   PUSH DWORD PTR [ESP+12]",
          "   JMP concat_whiletest$84",
          "concat_whilebody$83:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `stringlist?::B4::B4::(?S ?str `stringlist?::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   MOV EBX,[ESP+4]",
          "   MOV EAX,subsume(<B4>,1)",
          "   ADD EAX,EBX",
          "   MOV [ESP+4],EAX",
          "   PUSH EAX",
          "   MOV EAX,subsume(<B4>,1)",
          "   SUB [ESP],EAX",
          "   POP EAX",
          "   PUSH DWORD PTR [ESP+8]",
          "   MOV EAX,unroll(unroll([ESP+4]))",
          "   NAMEOBJ n$86,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x36077 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$86",
          "   MOV EAX,[EAX]",
          "   UNPACK ?sz$87,EAX,EAX",
          "   MOV EAX,subsume(<B4>,[EAX])",
          "   POP EBX",
          "   ADD EAX,EBX",
          "   MOV [ESP+8],EAX",
          "   MOV EAX,unroll(unroll([ESP]))",
          "   NAMEOBJ n$88,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x36500 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$88",
          "   MOV EAX,[EAX+4]",
          "   MOV [ESP],EAX",
          "concat_whiletest$84:",
          "   MOV EBX,[ESP]",
          "   MOV EAX,rollsum(<`stringlist?>,0)",
          "   CMP EBX,EAX",
          "   JNE tapp(concat_whilebody$83,<s1,s2,e1,e2>)",
          "concat_whileend$85:",
          "   MOV EBX,[ESP+8]",
          "   MOV EAX,subsume(<B4>,4)",
          "   ADD EAX,EBX",
          "   PUSH EAX",
          "   MOV EAX,[ESP+24]",
          "   CALL tapp(EAX,<ESP 1 7 s1,EBP 1,e1,e2>)",
          "   MOV [ESP],EAX",
          "   PUSH DWORD PTR [ESP+8]",
          "   PUSH subsume(<B4>,0)",
          "   PUSH DWORD PTR [ESP+8]",
          "   CALL tapp(" ^ writeint ^ ",<ESP 3 10 s1,EBP 1,e1,e2>)",
          "   ADD ESP,8",
          "   MOV DWORD PTR [ESP],subsume(<B4>,4)",
          "   JMP concat_whiletest$90",
          "concat_whilebody$89:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::?str::`stringlist?::B4::B4::(?S ?str `stringlist?::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   PUSH subsume(<B4>,0)",
          "   JMP concat_fortest$92",
          "concat_forbody$93:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::B4::?str::`stringlist?::B4::B4::(?S ?str `stringlist?::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "   PUSH DWORD PTR [ESP+8]",
          "   MOV EBX,[ESP+8]",
          "   MOV EAX,subsume(<B4>,1)",
          "   ADD EAX,EBX",
          "   MOV [ESP+8],EAX",
          "   PUSH EAX",
          "   MOV EAX,subsume(<B4>,1)",
          "   SUB [ESP],EAX",
          "   POP EAX",
          "   PUSH EAX",
          "   MOV EAX,unroll(unroll([ESP+36]))",
          "   NAMEOBJ n$96,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x38674 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$96",
          "   MOV EBX,[EAX]",
          "   MOV EAX,[ESP+8]",
          "   UNPACK ?sz$97,EBX,EBX",
          "   UNPACK i$98,EAX,EAX",
          "   CMP EAX,[EBX]",
          "   JAE " ^ TALUtil.error 0x38963 "array bounds",
          "   MOV EBX,[EBX+4]",
          "   MOV ECX,EAX",
          "   MOV EAX,0",
          "   MOV AL,[EBX+ECX]",
          "   POP EDI",
          "   POP ESI",
          "   UNPACK ?sz$99,ESI,ESI",
          "   UNPACK i$100,EDI,EDI",
          "   CMP EDI,[ESI]",
          "   JAE " ^ TALUtil.error 0x39287 "array bounds",
          "   MOV ESI,[ESI+4]",
          "   MOV [ESI+EDI],AL",
          "concat_forcount$94:",
          "   MOV EBX,[ESP]",
          "   MOV EAX,subsume(<B4>,1)",
          "   ADD EAX,EBX",
          "   MOV [ESP],EAX",
          "   PUSH EAX",
          "   MOV EAX,subsume(<B4>,1)",
          "   SUB [ESP],EAX",
          "   POP EAX",
          "concat_fortest$92:",
          "   PUSH DWORD PTR [ESP]",
          "   MOV EAX,unroll(unroll([ESP+32]))",
          "   NAMEOBJ n$101,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x39861 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$101",
          "   MOV EAX,[EAX]",
          "   UNPACK ?sz$102,EAX,EAX",
          "   MOV EAX,subsume(<B4>,[EAX])",
          "   POP EBX",
          "   CMP EBX,EAX",
          "   JL tapp(concat_forbody$93,<s1,s2,e1,e2>)",
          "concat_forend$95:",
          "   ADD ESP,4",
          "   MOV EAX,unroll(unroll([ESP+24]))",
          "   NAMEOBJ n$103,EAX",
          "   CMP EAX,0",
          "   JE " ^ TALUtil.error 0x40372 "null pointer",
          "   COERCE forgetname(EAX)",
          "   REMOVENAME n$103",
          "   MOV EAX,[EAX+4]",
          "   MOV [ESP+24],EAX",
          "concat_whiletest$90:",
          "   MOV EBX,[ESP+24]",
          "   MOV EAX,rollsum(<`stringlist?>,0)",
          "   CMP EBX,EAX",
          "   JNE tapp(concat_whilebody$89,<s1,s2,e1,e2>)",
          "concat_whileend$91:",
          "   MOV EAX,[ESP+4]",
          "   ADD ESP,20",
          "   RETN"])

    (* **** code for unmarshalling **** *)


    val getint = newstring "getint"
    val make_init_array = newstring "make_init_array"
    val unmarshall = newstring "unmarshall"
    val flatten = newstring "flatten"
        
    (* flatten and do backpatching. *)
    fun flatten_code () =
        (flatten, nil,
         "`ttt",
         ["(?arr `ttt)"],
         [
          (* S: ret :: ar ... *)
          "    MOV EAX,[ESP+4]",
          "    UNPACK ?sz$62,EAX,EAX",
          "    PUSH DWORD PTR subsume(<B4>,[EAX])", (* size(ar) *)
          "    PUSH subsume(<B4>,0)",
          (* S: i :: num :: ret :: ar ... *)
          "    JMP fortest$63",
          "forbody$64:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::B4::(?S `ttt (?arr `ttt)::se s1 s2 e1 e2)#(?E s2 e2)}>",
          (* S: i :: num :: ret :: ar ... *)
          "    MOV EBX,[ESP+12]", (* ar *)
          "    MOV EAX,[ESP]", (* i *)
          "    UNPACK ?sz$70,EBX,EBX",
          "    UNPACK i$71,EAX,EAX",
          "    CMP EAX,[EBX]",
          "    JAE " ^ TALUtil.error 0x41955 "array bounds",
          "    MOV EBX,[EBX+4]",
          "    MOV EAX,unroll([EBX+4*EAX])",
          "    NAMEOBJ uptr$69,EAX",
          "    MOV ECX,[EAX]",
          (* do switch on eax's tag *)
          "    CMP ECX," ^ itos TAL.TUPLETAG,
          "    JE aaa_value$73",
          "    CMP ECX," ^ itos (TAL.tagfor RTL.REF),
          "    JE rrr_value$72",
          "    CMP ECX," ^ itos TAL.SUMTAG,
          "    JE mmm_value$99",
          (* XXX add int_t *)
          "    JMP default$68",

          "aaa_value$73:",
          (* S: i :: num :: ret :: ar ... *)
          (* this part simply copied from popcorn with
             tags changed -- should be ok *)
          "    MOV EAX,[rec(forgetname(EAX))+4]",
          "    REMOVENAME uptr$69",
          "    PUSH EAX",
          "    MOV EAX,[ESP]",
          (* size of array inside *)
          "    UNPACK ?sz$74,EAX,EAX",
          "    PUSH DWORD PTR subsume(<B4>,[EAX])",
          "    PUSH subsume(<B4>,0)",
          "    JMP fortest$75",
          "forbody$76:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::B4::(?arr `ttt)::B4::B4::(?S `ttt (?arr `ttt)::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "    MOV EBX,[ESP+8]",
          "    MOV EAX,[ESP]",
          "    UNPACK ?sz$82,EBX,EBX",
          "    UNPACK i$83,EAX,EAX",
          "    CMP EAX,[EBX]",
          "    JAE " ^ TALUtil.error 0x43396 "array bounds",
          "    MOV EBX,[EBX+4]",
          "    MOV EAX,unroll([EBX+4*EAX])",
          "    NAMEOBJ uptr$81,EAX",
          "    MOV ECX,[EAX]",
          "    CMP ECX," ^ itos TAL.INDTAG,
          "    JNE default$80 ;; not an indirect",

          "    MOV EAX,[rec(forgetname(EAX))+4]",
          "    REMOVENAME uptr$81",
          "    PUSH EAX", (* idx *)
          (* fetch the indirect value ar[idx] *)
          "    MOV EBX,[ESP+28]",
          "    MOV EAX,[ESP]",
          "    UNPACK ?sz$85,EBX,EBX",
          "    UNPACK i$86,EAX,EAX",
          "    CMP EAX,[EBX]",
          "    JAE " ^ TALUtil.error 0x44034 "array bounds",
          "    MOV EBX,[EBX+4]",
          "    PUSH DWORD PTR [EBX+4*EAX]",
          "    PUSH DWORD PTR [ESP+16]",
          (* write it into the array (tuple) *)
          "    MOV EDI,[ESP+12]",
          "    MOV EAX,[ESP+4]",
          "    POP ESI",
          "    UNPACK ?sz$87,ESI,ESI",
          "    UNPACK i$88,EDI,EDI",
          "    CMP EDI,[ESI]",
          "    JAE " ^ TALUtil.error 0x44452 "array bounds",
          "    MOV ESI,[ESI+4]",
          "    MOV [ESI+4*EDI],EAX",
          "    ADD ESP,8",
          "    JMP tapp(endswitch$79,<s1,s2,e1,e2>)",
          "default$80:",
          "    FALLTHRU <s1,s2,e1,e2>",
          "endswitch$79:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::B4::(?arr `ttt)::B4::B4::(?S `ttt (?arr `ttt)::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "forcount$77:",
          "    MOV EBX,[ESP]",
          "    MOV EAX,subsume(<B4>,1)",
          "    ADD EAX,EBX",
          "    MOV [ESP],EAX",
          "    PUSH EAX",
          "    MOV EAX,subsume(<B4>,1)",
          "    SUB [ESP],EAX",
          "    POP EAX",
          "fortest$75:",
          "    MOV EBX,[ESP]",
          "    MOV EAX,[ESP+4]",
          "    CMP EBX,EAX",
          "    JL tapp(forbody$76,<s1,s2,e1,e2>)",
          "forend$78:",
          "    ADD ESP,12",
          "    JMP tapp(endswitch$67,<s1,s2,e1,e2>)"] @
         let
             (* give the label "rrr_value",
                offset into structure where ttt field is found,
                and the tag of the target *)
             fun flat_onefield (lab, offset, tag) =
                 [lab ^ ":",
                  (* S: i :: num :: ret :: ar ... *)
                  (* get contents of ttt = x *)
                  (* save the ttt itself for later*)
                  "    MOV EDX,EAX",
                  "    MOV EAX,[rec(forgetname(EAX))+" ^ itos offset ^ "]",
                  (*        "    REMOVENAME uptr$69", *)
                  "    PUSH EAX",
                  (* S: x :: i :: num :: ret :: ar ... *)
                  "    MOV EAX,unroll([ESP])", (* tag for inner *)
                  "    NAMEOBJ uptr$91,EAX",
                  "    MOV ECX,[EAX]",
                  "    CMP ECX," ^ itos TAL.INDTAG,
                  "    JNE default$90 ;; wasn't indirect",
                  (* get id inside indirect *)
                  "    MOV EAX,[rec(forgetname(EAX))+4]",
                  "    REMOVENAME uptr$91",
                  "    PUSH EAX",
                  (* S: id :: x :: i :: num :: ret :: ar ... *)
                  (* load eax = ar[id] *)
                  "    MOV EBX,[ESP+20]", (* ar *)
                  "    MOV EAX,[ESP]", (* id *) (* XXX didn't need to push *)
                  "    UNPACK ?sz$93,EBX,EBX",
                  "    UNPACK i$94,EAX,EAX",
                  "    CMP EAX,[EBX]",
                  "    JAE " ^ TALUtil.error 0x46949 "array bounds",
                  "    MOV EBX, [EBX+4]",
                  "    MOV EAX, [EBX+4*EAX]",
                  (* eax holds new looked up ttt *)
                  
                  (* set edx.ref = eax *)
                  "    mov [rec(forgetname(edx))+" ^ itos offset ^ "], eax",
                  (* XXX removename? *)
                  
                  "    ADD ESP,4",
                  (* S: x :: i :: num :: ret :: ar ... *)
                  "    JMP tapp(endswitch$89,<s1,s2,e1,e2>)"]
         in
           (* XXX tag param not necessary *)
             flat_onefield ("rrr_value$72", 4, TAL.INDTAG) @
             flat_onefield ("mmm_value$99", 8, TAL.SUMTAG)
         end


         @
         ["default$90:",
        "    FALLTHRU <s1,s2,e1,e2>",
        "endswitch$89:",
        "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr `ttt::B4::B4::(?S `ttt (?arr `ttt)::se s1 s2 e1 e2)#(?E s2 e2)}>",
        (* S: x :: i :: num :: ret :: ar ... *)
        "    ADD ESP,4",
        "    JMP tapp(endswitch$67,<s1,s2,e1,e2>)",
        "default$68:",
        "    FALLTHRU <s1,s2,e1,e2>",
        (* S: i :: num :: ret :: ar ... *)

        "endswitch$67:",
        "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::B4::(?S `ttt (?arr `ttt)::se s1 s2 e1 e2)#(?E s2 e2)}>",
        "forcount$65:",
        (* i ++ *)
        "    MOV EBX,[ESP]",
        "    MOV EAX,subsume(<B4>,1)",
        "    ADD EAX,EBX",
        "    MOV [ESP],EAX",
        (* this thing again ... ??? *)
        "    PUSH EAX",
        "    MOV EAX,subsume(<B4>,1)",
        "    SUB [ESP],EAX",
        "    POP EAX",
        "fortest$63:",
        (* S: i :: num :: ret :: ar ... *)
        "    MOV EBX,[ESP]",
        "    MOV EAX,[ESP+4]",
        "    CMP EBX,EAX",
        (* continue if i < num *)
        "    JL tapp(forbody$64,<s1,s2,e1,e2>)",
        "forend$66:",
        (* return ar[0] *)
        "    ADD ESP,4",
        "    MOV EBX,[ESP+8]",
        "    MOV EAX,subsume(<B4>,0)",
        "    UNPACK ?sz$95,EBX,EBX",
        "    UNPACK i$96,EAX,EAX",
        "    CMP EAX,[EBX]",
        "    JAE " ^ TALUtil.error 0x49131 "bounds",
        "    MOV EBX,[EBX+4]",
        "    MOV EAX,[EBX+4*EAX]",
        "    ADD ESP,4",
        "    RETN"])


    (* read four bytes from the string at the
       location given, then turn that into a word
       in big-endian order.

       copied straight from popcorn.

       this code really bites! I know there are
       array bounds checks, but I doubt it
       needs to be this long. Shifting and
       or-ing in place would probably make
       it better.
       *)
    fun getint_code () =
        (getint, nil,
         "B4",
         ["?str", "B4"],
         map (fn s => "    " ^ s)
         ["MOV EBX,[ESP+4]",
          "MOV EAX,[ESP+8]",
          "UNPACK ?sz$54,EBX,EBX",
          "UNPACK i$55,EAX,EAX",
          "CMP EAX,[EBX]",
          "JAE " ^ TALUtil.error 0x49922 "out of bounds",
          "MOV EBX,[EBX+4]",
          "MOV ECX,EAX",
          "MOV EAX,0",
          "MOV AL,[EBX+ECX]",
          "MOVZX EAX,AL",
          "PUSH EAX",
          "MOV EAX,subsume(<B4>,24)",
          "MOV ECX,EAX",
          "POP EAX",
          "SHL EAX,CL",
          "PUSH EAX",
          "PUSH DWORD PTR [ESP+8]",
          "MOV EBX,[ESP+16]",
          "MOV EAX,subsume(<B4>,1)",
          "ADD EAX,EBX",
          "POP EBX",
          "UNPACK ?sz$56,EBX,EBX",
          "UNPACK i$57,EAX,EAX",
          "CMP EAX,[EBX]",
          "JAE " ^ TALUtil.error 0x50501 "out of bounds",
          "MOV EBX,[EBX+4]",
          "MOV ECX,EAX",
          "MOV EAX,0",
          "MOV AL,[EBX+ECX]",
          "MOVZX EAX,AL",
          "PUSH EAX",
          "MOV EAX,subsume(<B4>,16)",
          "MOV ECX,EAX",
          "POP EAX",
          "SHL EAX,CL",
          "POP EBX",
          "OR EAX,EBX",
          "PUSH EAX",
          "PUSH DWORD PTR [ESP+8]",
          "MOV EBX,[ESP+16]",
          "MOV EAX,subsume(<B4>,2)",
          "ADD EAX,EBX",
          "POP EBX",
          "UNPACK ?sz$58,EBX,EBX",
          "UNPACK i$59,EAX,EAX",
          "CMP EAX,[EBX]",
          "JAE " ^ TALUtil.error 0x51125 "out of bounds",
          "MOV EBX,[EBX+4]",
          "MOV ECX,EAX",
          "MOV EAX,0",
          "MOV AL,[EBX+ECX]",
          "MOVZX EAX,AL",
          "PUSH EAX",
          "MOV EAX,subsume(<B4>,8)",
          "MOV ECX,EAX",
          "POP EAX",
          "SHL EAX,CL",
          "POP EBX",
          "OR EAX,EBX",
          "PUSH EAX",
          "PUSH DWORD PTR [ESP+8]",
          "MOV EBX,[ESP+16]",
          "MOV EAX,subsume(<B4>,3)",
          "ADD EAX,EBX",
          "POP EBX",
          "UNPACK ?sz$60,EBX,EBX",
          "UNPACK i$61,EAX,EAX",
          "CMP EAX,[EBX]",
          "JAE " ^ TALUtil.error 0x51748 "out of bounds",
          "MOV EBX,[EBX+4]",
          "MOV ECX,EAX",
          "MOV EAX,0",
          "MOV AL,[EBX+ECX]",
          "MOVZX EAX,AL",
          "POP EBX",
          "OR EAX,EBX",
          "RETN"])

    fun make_init_array_code () =
         (make_init_array, nil,
          "(?arr `ttt)",
          [
           (* array making function *)
           "(All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap]." ^
           "code {cap: &[e1,e2],EBP: sptr (?E s2 e2)," ^
           "ESP: sptr (?S (?arr va) B4::va::se s1 s2 e1 e2)" ^
           "#(?E s2 e2)})",
           (* number of elements *)
           "B4"],
          [
           (* ret :: mkarr :: num ... *)

           (* create ttt:indirect(-1) *)
           "    MALLOC mptr$48,8",
           "    MOV ESI,EAX",
           "    MOV DWORD PTR [ESI]," ^ itos TAL.INDTAG,
           "    MOV EAX,subsume(<B4>,-1)",
           "    MOV [ESI+4],EAX",
           "    FORGETUNIQUE mptr$48",
           "    PUSH rollsum(<`ttt>,forgetname(ESI))",
           (* init :: ret :: mkarr :: num ... *)
           "    PUSH DWORD PTR [ESP+12]",
           (* num :: init :: ret :: mkarr :: num ... *)
           "    MOV EAX,[ESP+12]", (* mkarr *)
           (* make array *)
           "    CALL tapp(EAX,<`ttt,ESP 2 5 s1,EBP 1,e1,e2>)",
           "    ADD ESP,4",
           "    MOV [ESP],EAX",
           (* loop over each to initialize *)
           "    PUSH subsume(<B4>,0)",
           "    JMP fortest$49",
           "forbody$50:",
           "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::(?arr `ttt)::(?S (?arr `ttt) (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S (?arr va) B4::va::se s1 s2 e1 e2)#(?E s2 e2)})::B4::se s1 s2 e1 e2)#(?E s2 e2)}>",
           "    PUSH DWORD PTR [ESP+4]",
           "    PUSH DWORD PTR [ESP+4]",
           (* create new ttt:indirect(self) *)
           "    MALLOC mptr$53,8",
           "    MOV ESI,EAX",
           "    MOV DWORD PTR [ESI]," ^ itos TAL.INDTAG,
           (* use index for contents *)
           "    MOV EAX,[ESP+8]",
           "    MOV [ESI+4],EAX",
           "    FORGETUNIQUE mptr$53",
           "    MOV EAX,rollsum(<`ttt>,forgetname(ESI))",
           "    POP EDI",
           "    POP ESI",
           (* assign into array *)
           "    UNPACK ?sz$54,ESI,ESI",
           "    UNPACK i$55,EDI,EDI",
           "    CMP EDI,[ESI]",
           "    JAE " ^ TALUtil.error 0x54140 "array out of bounds",
           "    MOV ESI,[ESI+4]",
           "    MOV [ESI+4*EDI],EAX",
           (* increment counter *)
           "    MOV EBX,[ESP]",
           "    MOV EAX,subsume(<B4>,1)",
           "    ADD EAX,EBX",
           "    MOV [ESP],EAX",
           (* do nothing??? from popcorn *)
           "    PUSH EAX",
           "    MOV EAX,subsume(<B4>,1)",
           "    SUB [ESP],EAX",
           "    POP EAX",
           (* continue if counter < num *)
           "fortest$49:",
           "    MOV EBX,[ESP]",
           "    MOV EAX,[ESP+16]",
           "    CMP EBX,EAX",
           "    JL tapp(forbody$50,<s1,s2,e1,e2>)",
           "forend$52:",
           "    ADD ESP,4",
           "    POP EAX",
           "    RETN "])

    fun unmarshall_code () =
        (unmarshall, nil,
         "`ttt",
         [
          (* create array given length and initializer *)
          "(All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S (?arr va) B4::va::se s1 s2 e1 e2)#(?E s2 e2)})",
          (* create string given length *)
          "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})",
          (* string to unmarshall *)
          "?str"],
         [
          (* S: ret :: mkar :: mks :: s :: ... *)
          (* get num from beginning of string *)
          "    PUSH subsume(<B4>,0)",
          "    PUSH DWORD PTR [ESP+16]", (* s *)
          (* S: s :: 0 :: ret :: mkar :: mks :: s :: ... *)
          "    CALL tapp(" ^ getint ^ ",<ESP 2 6 s1,EBP 1,e1,e2>)",
          (* put at top of stack *)
          "    ADD ESP,4",
          "    MOV [ESP],EAX",
          (* S: num :: ret :: mkar :: mks :: s :: ... *)
          "    PUSH DWORD PTR [ESP]",
          "    PUSH DWORD PTR [ESP+12]",
          (* S: mkar :: num :: num :: ret ... *)
          (* make array of size num, using code above *)
          "    CALL tapp(" ^ make_init_array ^ ",<ESP 2 7 s1,EBP 1,e1,e2>)",
          "    ADD ESP,4",
          "    MOV [ESP],EAX",
          (* S: arr :: num :: ret ... *)
          "    PUSH DWORD PTR [ESP+4]",
          (* S: num :: arr :: num :: ret ... *)
          (* idx = num - 1 *)
          "    MOV EAX,subsume(<B4>,1)",
          "    SUB [ESP],EAX",
          (* S: idx :: arr :: num :: ret ... *)
          "    PUSH subsume(<B4>,4)",
          (* S: ch :: idx :: ar :: num :: ret ... *)
          "    JMP whiletest$4",
          "whilebody$3:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::B4::(?arr `ttt)::B4::(?S `ttt (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S (?arr va) B4::va::se s1 s2 e1 e2)#(?E s2 e2)})::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::?str::se s1 s2 e1 e2)#(?E s2 e2)}>",
          (* process a single item from the string *)
          (* S: ch :: idx :: ar :: num :: ret ... *)
          "    PUSH DWORD PTR [ESP]",
          "    PUSH DWORD PTR [ESP+32]",
          (* S: s :: ch :: ch :: idx :: ar :: num :: ret ... *)
          (* get type tag *)
          "    CALL tapp(" ^ getint ^ ",<ESP 2 10 s1,EBP 1,e1,e2>)",
          "    ADD ESP,4",
          "    MOV [ESP],EAX",
          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* ch += 4 *)
          "    MOV EBX,[ESP+4]",
          "    MOV EAX,subsume(<B4>,4)",
          "    ADD EAX,EBX",
          "    MOV [ESP+4],EAX",
          (* switch on ty *)
          "    MOV EAX,[ESP]",
          "    CMP EAX," ^ itos tag_integer,
          "    JNE um_not_inttag",
          (* integer tag *)
          " ;; tag " ^ itos tag_integer ^ " -- integer",
          "    PUSH DWORD PTR [ESP+12]",
          "    PUSH DWORD PTR [ESP+12]",
          "    PUSH DWORD PTR [ESP+12]",
          "    PUSH DWORD PTR [ESP+44]",
          (* S: s? :: ch :: idx :: ar :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* get the int *)
          "    CALL tapp(" ^ getint ^ ",<ESP 2 13 s1,EBP 1,e1,e2>)",
          "    ADD ESP,4",
          "    MOV [ESP],EAX",
          (* S: int :: idx :: ar :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* create the ttt *)
          "    MALLOC mptr$8,8",
          "    POP DWORD PTR [EAX+4]",
          (* S: idx :: ar :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    MOV DWORD PTR [EAX], " ^ itos (TAL.tagfor RTL.INT),
          "    FORGETUNIQUE mptr$8",
          "    COERCE rollsum(<`ttt>,forgetname(EAX))",
          "    POP EDI",
          "    POP ESI",
          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* array bounds check *)
          "    UNPACK ?sz$9,ESI,ESI",
          "    UNPACK i$10,EDI,EDI",
          "    CMP EDI,[ESI]",
          "    JAE " ^ TALUtil.error 0x59045 "array out of bounds",
          "    MOV ESI,[ESI+4]",
          (* write into array *)
          "    MOV [ESI+4*EDI],EAX",
          (* ch += 4 *)
          "    MOV EBX,[ESP+4]",
          "    MOV EAX,subsume(<B4>,4)",
          "    ADD EAX,EBX",
          "    MOV [ESP+4],EAX",
(*          "    ADD ESP,4", *)
          (* S: ch :: idx :: ar :: num :: ret ... *)
          "    JMP tapp(endswitch$6,<s1,s2,e1,e2>)",

          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* EAX is ty, not 0 *)
          "um_not_inttag:",

          "    CMP EAX," ^ itos tag_string,
          "    JNE l1$11",
          (* string tag *)
          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)
          " ;; tag is " ^ itos tag_string ^ " -- string",
          "    PUSH DWORD PTR [ESP+4]",
          "    PUSH DWORD PTR [ESP+36]",
          (* S: s :: ch :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* get string length *)
          "    CALL tapp(" ^ getint ^ ",<ESP 2 11 s1,EBP 1,e1,e2>)",
          "    ADD ESP,4",
          "    MOV [ESP],EAX",
          (* S: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* ch += 4 *)
          "    MOV EBX,[ESP+8]",
          "    MOV EAX,subsume(<B4>,4)",
          "    ADD EAX,EBX",
          "    MOV [ESP+8],EAX",
          "    PUSH DWORD PTR [ESP]",
          (* S: slen :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* get newstring *)
          "    MOV EAX,[ESP+36]",
          "    CALL tapp(EAX,<ESP 1 11 s1,EBP 1,e1,e2>)",
          "    MOV [ESP],EAX",
          "    PUSH subsume(<B4>,0)",
          (* S: i :: ss :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* loop for i 0 .. slen *)
          "    JMP fortest$12",
          "forbody$13:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::?str::B4::B4::B4::B4::(?arr `ttt)::B4::(?S `ttt (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S (?arr va) B4::va::se s1 s2 e1 e2)#(?E s2 e2)})::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::?str::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "    PUSH DWORD PTR [ESP+4]",
          "    PUSH DWORD PTR [ESP+4]",
          "    PUSH DWORD PTR [ESP+52]",
          (* S: s? :: i :: ss :: i :: ss :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* read s[ch + i] *)
          "    MOV EBX,[ESP+28]", (* ch *)
          "    MOV EAX,[ESP+12]", (* i *)
          "    ADD EAX,EBX", (* ch + i *)
          "    POP EBX", (* s? *)
          (* S: i :: ss :: i :: ss :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    UNPACK ?sz$16,EBX,EBX",
          "    UNPACK i$17,EAX,EAX",
          "    CMP EAX,[EBX]",
          "    JAE " ^ TALUtil.error 0x61893 "array bounds",
          (* get string data *)
          "    MOV EBX,[EBX+4]",
          "    MOV ECX,EAX",
          "    MOV EAX,0",
          (* get byte at s + ecx  *)
          "    MOV AL,[EBX+ECX]",
          "    POP EDI", (* i *)
          "    POP ESI", (* ss *)
          (* S: i :: ss :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    UNPACK ?sz$18,ESI,ESI",
          "    UNPACK i$19,EDI,EDI",
          "    CMP EDI,[ESI]",
          "    JAE " ^ TALUtil.error 0x62390 "array bounds",
          (* write ss[i] = al *)
          "    MOV ESI,[ESI+4]",
          "    MOV [ESI+EDI],AL",
          (* i ++ *)
          "    MOV EBX,[ESP]",
          "    MOV EAX,subsume(<B4>,1)",
          "    ADD EAX,EBX",
          "    MOV [ESP],EAX",
          (* again, do nothing?? *)
          "    PUSH EAX",
          "    MOV EAX,subsume(<B4>,1)",
          "    SUB [ESP],EAX",
          "    POP EAX",
          "fortest$12:",
          (* S: i :: ss :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    MOV EBX,[ESP]",
          "    MOV EAX,[ESP+8]",
          "    CMP EBX,EAX",
          (* if i < slen, repeat *)
          "    JL tapp(forbody$13,<s1,s2,e1,e2>)",
          "forend$15:",
          "    ADD ESP,4",
          (* S: ss :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* ch += slen *)
          "    MOV EBX,[ESP+12]", (* ch *)
          "    MOV EAX,[ESP+4]", (* slen *)
          "    ADD EAX,EBX",
          "    MOV [ESP+12],EAX",

          "    PUSH DWORD PTR [ESP+20]",
          "    PUSH DWORD PTR [ESP+20]",
          (* S: idx :: ar :: ss :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* allocate new ttt:string *)
          "    MALLOC mptr$20,8",
          "    MOV ESI,EAX",
          "    MOV DWORD PTR [ESI]," ^ itos (TAL.tagfor RTL.STRING),
          "    MOV EAX,[ESP+8]", (* ss *)
          "    MOV [ESI+4],EAX",
          "    FORGETUNIQUE mptr$20",
          "    MOV EAX,rollsum(<`ttt>,forgetname(ESI))",
          "    POP EDI", (* idx *)
          "    POP ESI", (* ar *)
          (* S: ss :: slen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* check bounds *)
          "    UNPACK ?sz$21,ESI,ESI",
          "    UNPACK i$22,EDI,EDI",
          "    CMP EDI,[ESI]",
          "    JAE " ^ TALUtil.error 0x64228 "array bounds",
          "    MOV ESI,[ESI+4]",
          (* ar[idx] = new *)
          "    MOV [ESI+4*EDI],EAX",
          "    ADD ESP,8",
          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    JMP tapp(endswitch$6,<s1,s2,e1,e2>)",

          "l1$11:",
          "    CMP EAX," ^ itos tag_ref,
          "    JNE l2$23",
          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)

          " ;; tag is " ^ itos tag_ref ^ " -- ref",
          "    PUSH DWORD PTR [ESP+12]",
          "    PUSH DWORD PTR [ESP+12]",
          "    PUSH DWORD PTR [ESP+20]",
          "    PUSH DWORD PTR [ESP+16]",
          "    PUSH DWORD PTR [ESP+48]",
          (* S: s? :: ch :: ar :: idx :: ar :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* get int = offset of ttt *)
          "    CALL tapp(" ^ getint ^ ",<ESP 2 14 s1,EBP 1,e1,e2>)",
          "    ADD ESP,8",
          "    POP EBX", (* ar *)
          (* S: idx :: ar :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* fetch ar[eax] *)
          "    UNPACK ?sz$25,EBX,EBX",
          "    UNPACK i$26,EAX,EAX",
          "    CMP EAX,[EBX]",
          "    JAE " ^ TALUtil.error 0x65395 "array bounds",
          "    MOV EBX,[EBX+4]",
          "    PUSH DWORD PTR [EBX+4*EAX]",
          (* S: dest :: idx :: ar :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* create ttt:ref *)
          "    MALLOC mptr$24,8",
          "    POP DWORD PTR [EAX+4]",
          (* S: idx :: ar :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    MOV DWORD PTR [EAX]," ^ itos (TAL.tagfor RTL.REF),
          "    FORGETUNIQUE mptr$24",
          "    COERCE rollsum(<`ttt>,forgetname(EAX))",
          "    POP EDI", (* idx *)
          "    POP ESI", (* ar *)
          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* write EAX=new into ar[idx] *)
          "    UNPACK ?sz$27,ESI,ESI",
          "    UNPACK i$28,EDI,EDI",
          "    CMP EDI,[ESI]",
          "    JAE " ^ TALUtil.error 0x66219 "array bounds",
          "    MOV ESI,[ESI+4]",
          "    MOV [ESI+4*EDI],EAX",
          (* ch += 4 *)
          "    MOV EBX,[ESP+4]",
          "    MOV EAX,subsume(<B4>,4)",
          "    ADD EAX,EBX",
          "    MOV [ESP+4],EAX",
          "    JMP tapp(endswitch$6,<s1,s2,e1,e2>)",

          "l2$23:",
          "    CMP EAX," ^ itos tag_tuple,
          "    JNE l4$29",
          (* tag is array/tuple *)
          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)

          " ;; tag is " ^ itos tag_tuple ^ " -- tuple",
          "    PUSH DWORD PTR [ESP+4]",
          "    PUSH DWORD PTR [ESP+36]",
          (* S: s? :: ch :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* get array length *)
          "    CALL tapp(" ^ getint ^ ",<ESP 2 11 s1,EBP 1,e1,e2>)",
          "    ADD ESP,4",
          "    MOV [ESP],EAX",
          (* S: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* ch += 4 *)
          "    MOV EBX,[ESP+8]",
          "    MOV EAX,subsume(<B4>,4)",
          "    ADD EAX,EBX",
          "    MOV [ESP+8],EAX",
          (* get ar[0] as initializer *)
          "    MOV EBX,[ESP+16]",
          "    MOV EAX,subsume(<B4>,0)",
          "    UNPACK ?sz$30,EBX,EBX",
          "    UNPACK i$31,EAX,EAX",
          "    CMP EAX,[EBX]",
          "    JAE " ^ TALUtil.error 0x67559 "array bounds",
          "    MOV EBX,[EBX+4]",
          "    PUSH DWORD PTR [EBX+4*EAX]",
          "    PUSH DWORD PTR [ESP+4]",
          (* S: alen :: a[0] :: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    MOV EAX,[ESP+36]", (* make_array *)
          "    CALL tapp(EAX,<`ttt,ESP 2 12 s1,EBP 1,e1,e2>)",
          "    ADD ESP,4",
          "    MOV [ESP],EAX",
          "    PUSH subsume(<B4>,0)",
          (* S: i :: aa :: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    JMP fortest$32",
          "forbody$33:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::(?arr `ttt)::B4::B4::B4::B4::(?arr `ttt)::B4::(?S `ttt (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S (?arr va) B4::va::se s1 s2 e1 e2)#(?E s2 e2)})::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::?str::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "    PUSH DWORD PTR [ESP+4]",
          "    PUSH DWORD PTR [ESP+4]",
          "    PUSH DWORD PTR [ESP+32]",
          "    PUSH DWORD PTR [ESP+28]",
          "    PUSH DWORD PTR [ESP+60]",
          (* S: s :: ch :: ar :: i :: aa :: i :: aa :: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    CALL tapp(" ^ getint ^ ",<ESP 2 17 s1,EBP 1,e1,e2>)",
          "    ADD ESP,8",
          "    POP EBX", (* ar *)
          (* S: i :: aa :: i :: aa :: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* eax is now the index of the array element *)
          "    UNPACK ?sz$36,EBX,EBX",
          "    UNPACK i$37,EAX,EAX",
          "    CMP EAX,[EBX]",
          "    JAE " ^ TALUtil.error 0x69323 "array bounds",
          (* eax = ar[eax] *)
          "    MOV EBX,[EBX+4]",
          "    MOV EAX,[EBX+4*EAX]",
          "    POP EDI", (* i *)
          "    POP ESI", (* aa *)
          (* S: i :: aa :: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    UNPACK ?sz$38,ESI,ESI",
          "    UNPACK i$39,EDI,EDI",
          "    CMP EDI,[ESI]",
          "    JAE " ^ TALUtil.error 0x69728 "array bounds",
          "    MOV ESI,[ESI+4]",
          (* aa[i] = eax *)
          "    MOV [ESI+4*EDI],EAX",
          (* ch += 4 *)
          "    MOV EBX,[ESP+16]",
          "    MOV EAX,subsume(<B4>,4)",
          "    ADD EAX,EBX",
          "    MOV [ESP+16],EAX",

          (* i ++ *)
          "    MOV EBX,[ESP]",
          "    MOV EAX,subsume(<B4>,1)",
          "    ADD EAX,EBX",
          "    MOV [ESP],EAX",
          (* ??? *)
          "    PUSH EAX",
          "    MOV EAX,subsume(<B4>,1)",
          "    SUB [ESP],EAX",
          "    POP EAX",
          "fortest$32:",
          (* S: i :: aa :: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    MOV EBX,[ESP]",
          "    MOV EAX,[ESP+8]",
          "    CMP EBX,EAX",
          (* again if i < alen *)
          "    JL tapp(forbody$33,<s1,s2,e1,e2>)",
          "forend$35:",
          "    ADD ESP,4",
          "    PUSH DWORD PTR [ESP+20]",
          "    PUSH DWORD PTR [ESP+20]",
          (* S: idx :: ar :: aa :: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          (* create ttt:array *)
          "    MALLOC mptr$40,8",
          "    MOV ESI,EAX",
          "    MOV DWORD PTR [ESI]," ^ itos TAL.TUPLETAG,
          "    MOV EAX,[ESP+8]", (* aa *)
          "    MOV [ESI+4],EAX",
          "    FORGETUNIQUE mptr$40",
          "    MOV EAX,rollsum(<`ttt>,forgetname(ESI))",
          "    POP EDI", (* idx *)
          "    POP ESI", (* ar *)
          (* S: aa :: alen :: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    UNPACK ?sz$41,ESI,ESI",
          "    UNPACK i$42,EDI,EDI",
          "    CMP EDI,[ESI]",
          "    JAE " ^ TALUtil.error 0x71411 "array bounds",
          "    MOV ESI,[ESI+4]",
          (* ar[idx] = new *)
          "    MOV [ESI+4*EDI],EAX",
          "    ADD ESP,8",
          (* S: ty :: ch :: idx :: ar :: num :: ret ... *)
          "    JMP tapp(endswitch$6,<s1,s2,e1,e2>)",

          "l4$29:",
          "    CMP EAX," ^ itos tag_intt,
          "    JNE l5_unm",
          (* tag is int_t *)

          " ;; tag is " ^ itos tag_intt ^ " -- int_t",

          (* S: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          "    push dword ptr [esp + 4]", (* ch *)
          "    push dword ptr [esp + 36]", (* s *)
          (* S: ch :: s :: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          "    call tapp(" ^ getint ^ ",<ESP 2 11 s1, EBP 1, e1, e2>)",

          "    add esp, 4",
          "    mov [esp], eax",
          (* S: t :: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)

          (* ch += 4 *)
          "    mov eax, [esp + 8]",
          "    mov ebx, subsume(<B4>, 4)",
          "    add eax, ebx",
          "    mov [esp + 8], eax",

          "    push eax", (* ch *)          
          "    push dword ptr [esp + 40]", (* s *)
          (* S: s :: ch :: t :: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          "    call tapp(" ^ getint ^ ",<ESP 2 12 s1, EBP 1, e1, e2>)",
          "    add esp, 8",
          (* S: t :: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          (* get ar[eax] *)
          "    mov ebx, [esp + 16]", (* ar *)
          "    unpack unm3size, ebx, ebx",
          "    unpack unm3i, eax, eax",
          "    cmp eax, [ebx]",
          "    jae " ^ TALUtil.error 0x73103 "array bounds",
          "    mov ebx, [ebx+4]",
          "    push dword ptr [ebx+4*eax]",
          (* S: friend :: t :: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          (* create new  *)
          "    malloc unm3ptr, 12",
          "    pop dword ptr [eax + 8]", (* friend *)
          "    pop dword ptr [eax + 4]", (* t *)
          "    mov dword ptr [eax], " ^ itos TAL.SUMTAG,
          (* S: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          "    forgetunique unm3ptr",
          "    coerce rollsum(<`ttt>, forgetname(eax))",
          "    mov edi, [esp + 8]",
          "    mov esi, [esp + 12]",
          "    unpack unm3size2, esi, esi",
          "    unpack unm3i2, edi, edi",
          "    cmp edi, [esi]",
          "    jae " ^ TALUtil.error 0x73916 "array bounds",
          "    mov esi, [esi + 4]",
          "    mov [esi + 4 * edi], eax",
          
          (* ch += 4 again *)
          "    mov ebx, [esp + 4]",
          "    mov eax, subsume(<B4>, 4)",
          "    add eax, ebx",
          "    mov [esp + 4], eax",
          "    jmp tapp(endswitch$6, <s1, s2, e1, e2>)",

          "",
          "l5_unm:",
          "    cmp eax," ^ itos tag_code,
          "    jne l6_unm",
          (* tag is code *)

          " ;; tag is " ^ itos tag_code ^ " -- code",

          (* S: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          "    push dword ptr [esp + 4]", (* ch *)
          "    push dword ptr [esp + 36]", (* s *)
          (* S: ch :: s :: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          "    call tapp(" ^ getint ^ ",<ESP 2 11 s1, EBP 1, e1, e2>)",
          "    add esp, 4",
          "    mov [esp], eax",
          (* S: cidx :: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)

          (* ch += 4 *)
          "    mov eax, [esp + 8]",
          "    mov ebx, subsume(<B4>, 4)",
          "    add eax, ebx",
          "    mov [esp + 8], eax",

          (* now, look it up in the code table *)
          "    pop eax",

          (* get ctab[eax] *)
          "    mov ebx, ctab",
          "    unpack unm5size, ebx, ebx",
          "    unpack unm5i, eax, eax",
          "    cmp eax, [ebx]",
          "    jae " ^ TALUtil.error 0x75393 "array bounds",
          "    mov ebx, [ebx + 4]",
          "    mov ebx, [ebx + 4*eax]",
          
          (* allocate it *)
          "    malloc unm5ptr, 8",
          "    mov dword ptr [eax]," ^ itos (TAL.tagfor RTL.CODE),
          "    mov dword ptr [eax + 4], ebx",
          "    forgetunique unm5ptr",

          (* S: ty :: ch :: idx :: ar :: num :: ret :: mkar :: mks :: s ... *)
          "    mov esi, [esp + 12]",
          "    mov edi, [esp + 8]",
          "    unpack unm5size2, esi, esi",
          "    unpack unm5i2, edi, edi",
          "    cmp edi, [esi]",
          "    jae " ^ TALUtil.error 0x76017 "array bounds",
          "    mov esi, [esi + 4]",
          "    mov [esi + 4 * edi], rollsum(<`ttt>,forgetname(eax))",
          "    jmp tapp(endswitch$6, <s1, s2, e1, e2>)",
          

          "l6_unm:",
          " ;; default -- bad tag",
          "    JMP " ^ TALUtil.error 0x76302 "bad tag",

          "endswitch$6:",
          "LABELTYPE <All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr B4::B4::B4::(?arr `ttt)::B4::(?S `ttt (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S (?arr va) B4::va::se s1 s2 e1 e2)#(?E s2 e2)})::(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})::?str::se s1 s2 e1 e2)#(?E s2 e2)}>",
          "    PUSH DWORD PTR [ESP+8]",
          "    MOV EAX,subsume(<B4>,1)",
          "    SUB [ESP],EAX",
          "    POP EAX",
          "    MOV [ESP+8],EAX",
          "    PUSH EAX",
          "    MOV EAX,subsume(<B4>,1)",
          "    POP EBX",
          "    ADD EAX,EBX",
          "    ADD ESP,4",
          "whiletest$4:",
          (* S: ch :: idx :: ar :: num :: ret ... *)
          (* test idx >= 0 *)
          "    MOV EBX,[ESP+4]",
          "    MOV EAX,subsume(<B4>,0)",
          "    CMP EBX,EAX",
          "    JGE tapp(whilebody$3,<s1,s2,e1,e2>)",
          "whileend$5:",
          (* S: ch :: idx :: ar :: num :: ret ... *)
          (* done. call flatten(ar) and return the result *)
          "    PUSH DWORD PTR [ESP+8]",
          "    CALL tapp(" ^ flatten ^ ",<ESP 1 9 s1,EBP 1,e1,e2>)",
          "    ADD ESP,20",
          "    RETN "])

    fun code () =
        (* mar *)
        map TU.makecode [writeint_code, pushout_code, concat_code, getid_code,
                         getcode_code, marshall_code] @
        (* un *)
        map TU.makecode [make_init_array_code, getint_code, flatten_code, 
                         unmarshall_code]


    (* used in marshall *)
    val types =
        ["TYPE <stringlist? :T4 = ^T(0)`stringlist?mem>",
         "TYPE <stringlist?mem :Tm 8 = *[?str^rw,`stringlist?^rw]>",
         "TYPE <tttlist? :T4 = ^T(0)`tttlist?mem>",
         "TYPE <tttlist?mem :Tm 8 = *[`ttt^rw,`tttlist?^rw]>",
         "TYPE <tttmap? :T4 = ^T(0)`tttmap?mem>",
         "TYPE <tttmap?mem :Tm 16 = *[`ttt^rw,B4^rw,`tttmap?^rw,`tttmap?^rw]>"]
        
    val data =
        (* marshall/unmarshall data *)
        ["_waitqlast:",
         "LABELTYPE <^*[`tttlist?^rw]>",
         "\tDD rollsum(<`tttlist?>,0)",
         "_waitq:",
         "LABELTYPE <^*[`tttlist?^rw]>",
         "\tDD rollsum(<`tttlist?>,0)",
         "_tmap:",
         "LABELTYPE <^*[`tttmap?^rw]>",
         "\tDD rollsum(<`tttmap?>,0)",
         "_outq:",
         "LABELTYPE <^*[`stringlist?^rw]>",
         "\tDD rollsum(<`stringlist?>,0)",
         "_nextindex:",
         "LABELTYPE <^*[B4^rw]>",
         "\tDD subsume(<B4>,0)"]

    (* XXX in fact, we don't need name of entry point since
       it will always be in ctab at location 0 *)
    fun entry s = 
        "\000\000\000\003" ^ (* three items *)
        
        (* now in reverse order... *)
        (* 2 = environment (use int 0, never inspected) *)
        "\000\000\000" ^ implode [chr (tag_integer)] ^
        "\000\000\000\000" ^

        (* 1 = code pointer *)
        "\000\000\000" ^ implode [chr (tag_code)] ^
        "\000\000\000\000" ^ (* always code table offset 0 *)

        (* 0 = closure tuple *)
        "\000\000\000" ^ implode [chr (tag_tuple)] ^
        "\000\000\000\002" ^ (* 2 elements *)
        "\000\000\000\001" ^ (* code *)
        "\000\000\000\002" (* env *)

end
