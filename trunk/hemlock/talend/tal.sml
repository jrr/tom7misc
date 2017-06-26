
(* lots of messy TAL types, mostly copied from popcorn *)

structure TAL =
struct

    exception TAL of string

    val itos = Int.toString

    (* there are lots more reserved words... *)
    val reserved =
        ["prove", "coerce", "subsume", 
         "al", "bl", "cl", "dl",
         "ah", "bh", "ch", "dh", "ax", "bx", "cx", "dx",
         "eax", "ebx", "ecx", "edx"]

    (* ensure that if anyone ever uses these names, 
       they receive a number appended to them. *)
    val _ = app (ignore o Variable.namedvar) reserved



    val exnpacket = "_NullPointer_pkt"
(*     val exnpacket = "_UnionVariant_pkt"
 *)
    val poptypes = 
       [";; standard popcorn types",
        "TYPE      <?arr = fn c:T . Exist[?sz:Sint].^*[S(?sz)^r,(^*[array(?sz,c^rw)])^r]>",
        "TYPE      <?str = Exist[?sz:Sint].^*[S(?sz)^r,(^*[array(?sz,B1^rw)])^r]>",
        "TYPE      <?exnname = fn c$0:Tm . ^T^rw(c$0)*[B4^rw]>",
        "TYPE      <?exn = Exist[c$0:Tm].^*[(?exnname c$0)^r,?str^r,c$0]>",
        "TYPE      <?H = fn s:Ts e:Tcap . code {cap: e,EAX: ?exn,ESP: sptr s}>",
        "TYPE      <?E = fn s:Ts e:Tcap . (?H s e)::s>",
        "TYPE      <?Ha = fn ?s1:Ts ?s2:Ts ?e1:Tcap ?e2:Tcap . (sptr (?E ?s2 ?e2))::(?s1#(?E ?s2 ?e2))>",
        "TYPE      <?S = fn ?ret:T4 ?sp:Ts ?s1:Ts ?s2:Ts ?e1:Tcap ?e2:Tcap . (code {cap: &[?e1,?e2],EAX: ?ret,EBP: sptr (?E ?s2 ?e2),ESP: sptr (?sp#?s1)#(?E ?s2 ?e2)})::(?sp#?s1)>",
        "TYPE      <?Sv = fn ?sp:Ts ?s1:Ts ?s2:Ts ?e1:Tcap ?e2:Tcap . (code {cap: &[?e1,?e2],EBP: sptr (?E ?s2 ?e2),ESP: sptr (?sp#?s1)#(?E ?s2 ?e2)})::(?sp#?s1)>",
        "TYPE      <?Sf = fn ?sp:Ts ?s1:Ts ?s2:Ts ?e1:Tcap ?e2:Tcap . (code {cap: &[?e1,?e2],ST0,EBP: sptr (?E ?s2 ?e2),ESP: sptr (?sp#?s1)#(?E ?s2 ?e2)})::(?sp#?s1)>",
        "TYPE      <?Ss = fn ?ret:T4 ?sp:Ts ?s1:Ts ?s2:Ts ?e1:Tcap ?e2:Tcap . (code {cap: &[?e1,?e2],EAX: ?ret,EBP: sptr (?E ?s2 ?e2),ESP: sptr ?s1#(?E ?s2 ?e2)})::(?sp#?s1)>",
        "TYPE      <?Ssv = fn ?sp:Ts ?s1:Ts ?s2:Ts ?e1:Tcap ?e2:Tcap . (code {cap: &[?e1,?e2],EBP: sptr (?E ?s2 ?e2),ESP: sptr ?s1#(?E ?s2 ?e2)})::(?sp#?s1)>",
        "TYPE      <?Ssf = fn ?sp:Ts ?s1:Ts ?s2:Ts ?e1:Tcap ?e2:Tcap . (code {cap: &[?e1,?e2],ST0,EBP: sptr (?E ?s2 ?e2),ESP: sptr ?s1#(?E ?s2 ?e2)})::(?sp#?s1)>",

        ""]

    val args_to_main =
        [
         ("arg", "?str"),
         ("witness", "?str"),
         ("random", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S B4 B4::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("newstring", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str B4::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("newarray", "(All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S (?arr va) B4::va::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("dep_one", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S vdep ?str::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("dep_and", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S vdep vdlis::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("dep_or", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S vdep vdlis::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("deplist_empty", "vdlis"),
         ("deplist_cons", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S vdlis vdep::vdlis::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("answer_ok", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S vanswer ?str::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("answer_forward", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S vanswer ?str::se s1 s2 e1 e2)#(?E s2 e2)})"),
         ("submit", "(All[s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S ?str ?str::vdep::(^T[0,1])::se s1 s2 e1 e2)#(?E s2 e2)})")]

    (* number of arguments to main_function. We need
       to know its stack layout for other stuff. *)
    val mainargs = length args_to_main

    val stack_to_main = 
        "(?S vanswer " ^
        StringUtil.delimit "::" (map #2 args_to_main) ^
        "::se s1 s2 e1 e2)#(?E s2 e2)"

    (* get type of main argument *)
    fun typeof s =
        case ListUtil.Alist.find op= args_to_main s of
            NONE => raise TAL ("There is no argument " ^ s ^ " to main.")
          | SOME x => x

    (* get depth of the named argument in main's stack *)
    fun argdepth s =
        case ListUtil.position (fn (a, _) => a = s) args_to_main of
            NONE => raise TAL ("There is no argument " ^ s ^ " to main.")
          | SOME x => x

    (* ttt 1: int 2: string 3: code 4: ref 5: int_t 6: array (tuple)
       7: indirect (used in marshalling)
       *)
    fun tagfor RTL.INT = 1
      | tagfor RTL.STRING = 2
      | tagfor RTL.CODE = 3
      | tagfor RTL.REF = 4

    val SUMTAG = 5
    val TUPLETAG = 6
    val INDTAG = 7

    fun nameof RTL.INT = "int"
      | nameof RTL.STRING = "string"
      | nameof RTL.CODE = "code"
      | nameof RTL.REF = "ref"

    val hemtypes = 
        [";; types for hemlock",
         
         (* ttt and hemcode are mutually recursive, so first we 
            make 'a hemcodex ... 
            (note that we also abstract the type of the incoming
            registers, so that we can make error handlers 
            with junk) *)

        "TYPE <?hemcodex = fn ttta:T4 tregs:T4 vanswer:T4 vdep:T4 vdlis:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap . code {cap: &[e1,e2], EBX: tregs, ECX: tregs, EDX : tregs, ESI: tregs, EDI: tregs, EBP: sptr (?E s2 e2), ESP: sptr ttta::" ^ stack_to_main ^ "}>",

        (* then we make ttt referring to ttt hemcodex ... *)
        "TYPE <ttt : T4 = ^+[" ^
         StringUtil.delimit ", "
         ["*[S(" ^ itos (tagfor RTL.INT) ^ ")^r, B4^rw]",
          "*[S(" ^ itos (tagfor RTL.STRING) ^ ")^r, ?str^r]",
          "*[S(" ^ itos (tagfor RTL.REF) ^ ")^r, `ttt^rw]",
          "*[S(" ^ itos SUMTAG ^ ")^r, B4^r, `ttt^rw]",
          "*[S(" ^ itos INDTAG ^ ")^r, B4^rw]",
          "*[S(" ^ itos TUPLETAG ^ ")^r, (?arr ` ttt)^r]",
          "*[S(" ^ itos (tagfor RTL.CODE) ^ ")^r, (All [vanswer:T4 vdep:T4 vdlis:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap] . (?hemcodex (`ttt) (`ttt) vanswer vdep vdlis s1 s2 e1 e2))^r]"] ^
        "]>",

        (* then we make hemcode = ttt hemcodex *)
        "TYPE <?hemcode = fn vanswer:T4 vdep:T4 vdlis:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap . ?hemcodex (`ttt) (`ttt) vanswer vdep vdlis s1 s2 e1 e2>"]


    (* for popcorn dlopen *)
    val dyninittype = "All[vb:T4 vc:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?Sv (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S va vb::?str::(^*[Rep(type va)])::se s1 s2 e1 e2)#(?E s2 e2)})::vb::(All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?Sv vc::?str::(^*[Rep(type va)])::va::se s1 s2 e1 e2)#(?E s2 e2)})::vc::(^T[0,1])::se s1 s2 e1 e2)#(?E s2 e2)}"

    (* the function that is loaded as the cord entry point. *)
    val mainfntype = "All[vanswer:T4 vdep:T4 vdlis:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr " ^ stack_to_main ^ "}"

    (* see mainstub.tal for explanation. hemcode is defined in tal.sml *)
    val codetype =
        "All[vanswer:T4 vdep:T4 vdlis:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap] . (?hemcode vanswer vdep vdlis s1 s2 e1 e2)"

    fun mainfn mn unmarsh =
        [";; XXX FIXME  should load default exception handler!",
         ";; (we need some ttt there, so let's just put the int 32767 for now)",
         "MALLOC def_exn_handler__, 8",
         "MOV DWORD PTR [EAX], 1", (* assumes int tag of 1 *)
         "MOV DWORD PTR [EAX + 4], subsume(<B4>, 32767)",
         "FORGETUNIQUE def_exn_handler__",
         "COERCE rollsum(<`ttt>, forgetname(EAX))",
         (* exception handler *)
         "PUSH EAX",
         "",

         (* unmarshall closure arg *)
                              (* stack holds exception handler, and return address *)
         "push dword ptr [esp + " ^ itos (4*(2 + 0 + argdepth "arg")) ^ "]",
         "push dword ptr [esp + " ^ itos (4*(2 + 1 + argdepth "newstring")) ^ "]",
         "push dword ptr [esp + " ^ itos (4*(2 + 2 + argdepth "newarray")) ^ "]",

         "call tapp(" ^ unmarsh ^ ", <ESP 3 " ^ itos (mainargs + 1 (* exn *) + 
                                                      1 (* ret *) + 3 (* args *)) ^
                                     "s1, EBP 1, e1, e2>)",
                                     
         "add esp, 12",
         (* eax holds closure *)

         ";; put dummy ttt in each register",
         "MOV EBX, EAX",
         "MOV ECX, EAX",
         "MOV EDX, EAX",
         "MOV ESI, EAX",
         "MOV EDI, EAX",
         ""]

(*
         ";; note .. though we have pushed a ttt onto s1, hemcode knows this,",
         ";; so we don't need to pass it in here.",
         "JMP tapp(" ^ mn ^ ", <vanswer, vdep, vdlis, s1, s2, e1, e2>)",
         ""]
*)
        (* same as popcorn outputs. Could be simplified; I'm not sure what all
           these flags are for. *)
        val dyninit =
            ["",
             "   MOV  EAX,DWORD PTR [_looked_up_old]",
             "   CMP  EAX,1",
             "   JE  di_iffalse$7",
             "di_iftrue$6:",
             "   MOV  EAX,sum(<^T[0,1]>,1)",
             "   MOV  DWORD PTR [_looked_up_old],EAX",
             "   MOV  EAX,[ESP+20]",
             "   CMP  EAX,0",
             "   JE  di_iffalse$10",
             "di_iftrue$9:",
             "   RETN",
             "di_iffalse$10:",
             "   FALLTHRU  <vb,vc,s1,s2,e1,e2>",
             "di_tc_fast$12:",
             "LABELTYPE <All[vb:T4 vc:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?Sv (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S va vb::?str::(^*[Rep(type va)])::se s1 s2 e1 e2)#(?E s2 e2)})::vb::(All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?Sv vc::?str::(^*[Rep(type va)])::va::se s1 s2 e1 e2)#(?E s2 e2)})::vc::(^T[0,1])::se s1 s2 e1 e2)#(?E s2 e2)}>",
             "di_ifend$11:",
             "   JMP  di_ifend$8",
             "di_iffalse$7:",
             "   FALLTHRU  <vb,vc,s1,s2,e1,e2>",
             "di_tc_fast$13:",
             "LABELTYPE <All[vb:T4 vc:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?Sv (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S va vb::?str::(^*[Rep(type va)])::se s1 s2 e1 e2)#(?E s2 e2)})::vb::(All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?Sv vc::?str::(^*[Rep(type va)])::va::se s1 s2 e1 e2)#(?E s2 e2)})::vc::(^T[0,1])::se s1 s2 e1 e2)#(?E s2 e2)}>",
             "di_ifend$8:",
             "   MOV  EAX,DWORD PTR [_is_updated]",
             "   CMP  EAX,1",
             "   JE  di_iffalse$15",
             "di_iftrue$14:",
             "   MOV  EAX,sum(<^T[0,1]>,1)",
             "   MOV  DWORD PTR [_is_updated],EAX",
             "   PUSH  _main_function",
             "   PUSH  type_of_mainfn",
             "   PUSH  name_of_mainfn",
             "   PUSH  DWORD PTR [ESP+28]",
             "   MOV  EAX,[ESP+28]",
             "   CALL  tapp(EAX,<" ^ mainfntype ^ ",ESP 4 10 s1,EBP 1,e1,e2>)",
             "   ADD  ESP,16",
             "   JMP  di_ifend$16",
             "di_iffalse$15:",
             "   FALLTHRU  <vb,vc,s1,s2,e1,e2>",
             "di_tc_fast$20:",
             "LABELTYPE <All[vb:T4 vc:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?Sv (All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?S va vb::?str::(^*[Rep(type va)])::se s1 s2 e1 e2)#(?E s2 e2)})::vb::(All[va:T4 s1:Ts s2:Ts e1:Tcap e2:Tcap].code {cap: &[e1,e2],EBP: sptr (?E s2 e2),ESP: sptr (?Sv vc::?str::(^*[Rep(type va)])::va::se s1 s2 e1 e2)#(?E s2 e2)})::vc::(^T[0,1])::se s1 s2 e1 e2)#(?E s2 e2)}>",
             "di_ifend$16:",
             "   RETN"]

        (* store "main_function", its type, and some flags used by dyninit *)
        val dyninit_data =
            [";; dyninit data",
             "nom_data:",
             "LABELTYPE <^*[array(13,B1^rw)]>",
             "COERCE  array(0,0,<B1^rw>,?)",
             "\tDB \"main_function\"",
             "name_of_mainfn:",
             "COERCE  pack(<13>,?,<?str>)",
             "\tDD 13",
             "\tDD nom_data",
             "type_of_mainfn:",
             "LABELTYPE <^*[Rep(type " ^ mainfntype ^ ")]>",
             "\tDREP TYPE " ^ mainfntype,
             "",
             "_looked_up_old:",
             "LABELTYPE <^*[(^T[0,1])^rw]>",
             "\tDD sum(<^T[0,1]>,0)",
             "_is_updated:",
             "LABELTYPE <^*[(^T[0,1])^rw]>",
             "\tDD sum(<^T[0,1]>,0)"]
  
end
