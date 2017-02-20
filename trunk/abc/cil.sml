structure CIL =
struct

  (* Name of the main function; the program is expected to
     have exactly one of these. *)
  val main = "main"
  exception CIL of string

  datatype loc = Local of string | Global of string

  (* Width of data for a read or write.
      - This is where we'd support bitfields
        (I think there would be both an offset and size.)
      - This is maybe also where we'd support struct copying. *)
  datatype width = Width8 | Width16 | Width32

  datatype signedness = Signed | Unsigned

  (* Post-elaboration type *)
  datatype typ =
    Pointer of typ
  | Code of typ * typ list
  | Struct of (string * typ) list
  (* We can do computation on 8-, 16-, and 32-bit words.
     Additionally, we need to know the width of loads and
     stores. *)

  (* We only care about signedness during the ToCIL phase.
     XXX some clean way to discard this info for the final program. *)
  | Word32 of signedness
  | Word16 of signedness
  | Word8 of signedness

  fun typwidth (Pointer _) = Width16
    | typwidth (Code _) = Width16
    | typwidth (Struct _) = raise CIL "unimplemented structs"
    | typwidth (Word32 _) = Width32
    | typwidth (Word16 _) = Width16
    | typwidth (Word8 _) = Width8

  datatype value =
    Var of string
    (* typ is the type of the thing thing pointed to. *)
  | AddressLiteral of loc * typ
    (* name of function, return type, argument types *)
  | FunctionLiteral of string * typ * typ list
  | Word8Literal of Word8.word
  | Word16Literal of Word16.word
  | Word32Literal of Word32.word

    (* Maybe should be pointer to bytes? *)
  | StringLiteral of string

  (* Here's how we handle integral widths and signedness.
     After conversion to CIL, everything should be completely explicit:
      - All values have a specific representation, like Word8 or Word16.
        The signedness of these does not change their representation, so
        it is not stored.
      - We have explicit Truncate and Promote expressions to convert
        between word widths, including sign extension.
      - Operations all have specific types that they operate on. Some
        operators come in a signed and unsigned version. *)

  (* XXX maybe recursive instances of value should really just be a variable,
     basically, a register? *)
  datatype exp =
    Value of value
    (* For dst < src. Just discards bits. *)
  | Truncate of { src: width, dst: width, v: value }
    (* For dst > src. Sign-extends if signed is true. *)
  | Promote of { signed: bool, src: width, dst: width, v: value }
    (* TODO: LoadImmediate *)
  | Plus of width * value * value
  | Minus of width * value * value
    (* Not clear that we support 8-bit times / div? *)
  | Times of width * value * value
  | SignedDivision of width * value * value
  | UnsignedDivision of width * value * value
    (* Also signed mod? *)
  | UnsignedMod of width * value * value
    (* For LeftShift and RightShift, shift amount should be word8. *)
  | LeftShift of width * value * value
    (* Recall that C does not define the behavior of shifts on negative values,
       so this is unsigned. *)
  | RightShift of width * value * value
    (* "Greater" and "Less" are signed.
       "Above" and "Below" are unsigned. *)
  | Greater of width * value * value
  | GreaterEq of width * value * value
  | Above of width * value * value
  | AboveEq of width * value * value
  | Less of width * value * value
  | LessEq of width * value * value
  | Below of width * value * value
  | BelowEq of width * value * value
  | Eq of width * value * value
  | Neq of width * value * value
  (* These are all bitwise. && and || are compiled away. *)
  | And of width * value * value
  | Or of width * value * value
  | Xor of width * value * value
  | Not of width * value
  | Complement of width * value
  | Negate of width * value

  (* Should just be Math and then LOAD? *)
  (* | Subscript of value * value
     | Member of value * string *)
  | Call of value * value list
  | Load of width * value

  (* Alternative to the operator forms. *)
  datatype cond =
    CLess of width * value * value
  | CLessEq of width * value * value
  | CBelow of width * value * value
  | CBelowEq of width * value * value
  | CEq of width * value * value
  | CNeq of width * value * value

  datatype stmt =
    Bind of string * typ * exp * stmt
    (* Like bind, but no variable. *)
  | Do of exp * stmt
    (* Store (width, address, value, rest) *)
  | Store of width * value * value * stmt
    (* GotoIf (cond, true-label, else-branch). *)
  | GotoIf of cond * string * stmt
  | Goto of string
  | Return of value
  | End

  datatype function =
    Func of { args : (string * typ) list,
              ret : typ,
              (* Start label; must appear in blocks. *)
              body : string,
              blocks : (string * stmt) list }

  datatype global =
    Glob of { typ : typ,
              (* Byte pattern the global should start with, if
                 desired. This is expected to be the correct size.
                 Initialization may overwrite this. *)
              bytes : Word8Vector.vector option,
              (* Initialization should write (only) to this global,
                 and should not depend on other globals. Start
                 label must appear in blocks. *)
              init : { start : string,
                       blocks : (string * stmt) list } option }

  datatype program =
    Program of { main : string,
                 functions : (string * function) list,
                 globals : (string * global) list }

  fun typtos (Pointer t) = "(" ^ typtos t ^ " ptr)"
    | typtos (Struct t) = "... TODO STRUCT ..."
    | typtos (Code (ret, args)) = ("(" ^
                                   StringUtil.delimit " , " (map typtos args) ^
                                   " -> " ^ typtos ret ^ ")")
    | typtos (Word32 Unsigned) = "u32"
    | typtos (Word32 Signed) = "i32"
    | typtos (Word16 Unsigned) = "u16"
    | typtos (Word16 Signed) = "i16"
    | typtos (Word8 Unsigned) = "u8"
    | typtos (Word8 Signed) = "i8"

  local
    fun eq_typ (Pointer a, Pointer b) = eq_typ (a, b)
      | eq_typ (Struct a, Struct b) = eq_meml (a, b)
      | eq_typ (Code (aret, aargs), Code (bret, bargs)) =
          eq_typ (aret, bret) andalso eq_typl (aargs, bargs)
      | eq_typ (Word32 s1, Word32 s2) = s1 = s2
      | eq_typ (Word16 s1, Word16 s2) = s1 = s2
      | eq_typ (Word8 s1, Word8 s2) = s1 = s2
      | eq_typ _ = false

    and eq_meml (nil, nil) = true
      | eq_meml ((av, a) :: arest, (bv, b) :: brest) = av = bv andalso
      eq_typ (a, b) andalso eq_meml (arest, brest)
      | eq_meml _ = false

    and eq_typl (nil, nil) = true
      | eq_typl (a :: arest, b :: brest) = eq_typ (a, b) andalso eq_typl (arest, brest)
      | eq_typl _ = false
  in
    val eq_typ = eq_typ
  end

  fun loctos (Local l) = l
    | loctos (Global l) = "GLOBAL " ^ l

  fun valtos (Var v) = v
    | valtos (Word32Literal w) = "0x" ^ Word32.toString w
    | valtos (Word16Literal w) = "0x" ^ Word16.toString w
    | valtos (Word8Literal w) = "0x" ^ Word8.toString w
    (* XXX heuristic for data, etc. *)
    | valtos (StringLiteral s) = "\"" ^ String.toString s ^ "\""
    | valtos (AddressLiteral (a, t)) = "ADDR(" ^ loctos a ^ " : " ^
    typtos t ^ ")"
    | valtos (FunctionLiteral (f, ret, args)) = "FADDR(" ^ f ^ " : " ^
    typtos ret ^ "(" ^ StringUtil.delimit ", " (map typtos args) ^ "))"

  fun widthtos Width32 = "_32"
    | widthtos Width16 = "_16"
    | widthtos Width8 = "_8"

  fun exptos (Value v) = valtos v
    | exptos (Truncate { src, dst, v }) =
    "trunc" ^ widthtos src ^ "_to" ^ widthtos dst ^ " " ^ valtos v
    | exptos (Promote { signed, src, dst, v }) =
    "promote" ^ widthtos src ^ "_to" ^ widthtos dst ^
    (if signed then "_extending" else "") ^ " " ^ valtos v
    | exptos (Plus (w, a, b)) = valtos a ^ " + " ^ valtos b
    | exptos (Minus (w, a, b)) = valtos a ^ " - " ^ valtos b
    | exptos (Times (w, a, b)) = valtos a ^ " * " ^ valtos b
    | exptos (SignedDivision (w, a, b)) = valtos a ^ " /s " ^ valtos b
    | exptos (UnsignedDivision (w, a, b)) = valtos a ^ " / " ^ valtos b
    | exptos (UnsignedMod (w, a, b)) = valtos a ^ " % " ^ valtos b
    | exptos (LeftShift (w, a, b)) = valtos a ^ " << " ^ valtos b
    | exptos (RightShift (w, a, b)) = valtos a ^ " >> " ^ valtos b
    | exptos (Greater (w, a, b)) = valtos a ^ " >s " ^ valtos b
    | exptos (GreaterEq (w, a, b)) = valtos a ^ " >=s " ^ valtos b
    | exptos (Above (w, a, b)) = valtos a ^ " > " ^ valtos b
    | exptos (AboveEq (w, a, b)) = valtos a ^ " >= " ^ valtos b
    | exptos (Less (w, a, b)) = valtos a ^ " <s " ^ valtos b
    | exptos (LessEq (w, a, b)) = valtos a ^ " <=s " ^ valtos b
    | exptos (Below (w, a, b)) = valtos a ^ " < " ^ valtos b
    | exptos (BelowEq (w, a, b)) = valtos a ^ " <= " ^ valtos b
    | exptos (Eq (w, a, b)) = valtos a ^ " == " ^ valtos b
    | exptos (Neq (w, a, b)) = valtos a ^ " != " ^ valtos b
    | exptos (And (w, a, b)) = valtos a ^ " & " ^ valtos b
    | exptos (Or (w, a, b)) = valtos a ^ " | " ^ valtos b
    | exptos (Xor (w, a, b)) = valtos a ^ " ^ " ^ valtos b
    | exptos (Not (w, a)) = "!" ^ valtos a
    | exptos (Complement (w, a)) = "~" ^ valtos a
    | exptos (Negate (w, a)) = "-" ^ valtos a
    (* | exptos (AddressOf s) = "&" ^ s *)
    (* | exptos (Dereference a) = "*" ^ valtos a *)
    (* | exptos (Subscript (a, b)) = valtos a ^ "[" ^ valtos b ^ "]" *)
    (* | exptos (Member (a, s)) = "(" ^ valtos a ^ ")." ^ s *)
    | exptos (Call (f, vl)) = "CALL " ^ valtos f ^
        "(" ^ StringUtil.delimit ", " (map valtos vl) ^ ")"
    | exptos (Load (width, addr)) = "LOAD" ^ widthtos width ^ " " ^ valtos addr

  fun condtos (CLess (w, a, b)) = valtos a ^ " <s " ^ valtos b
    | condtos (CLessEq (w, a, b)) = valtos a ^ " <=s " ^ valtos b
    | condtos (CBelow (w, a, b)) = valtos a ^ " < " ^ valtos b
    | condtos (CBelowEq (w, a, b)) = valtos a ^ " <s " ^ valtos b
    | condtos (CEq (w, a, b)) = valtos a ^ " == " ^ valtos b
    | condtos (CNeq (w, a, b)) = valtos a ^ " != " ^ valtos b

  fun stmttos (Bind (var, t, e, s)) =
        "  " ^ var ^ " : " ^ typtos t ^ " = " ^ exptos e ^ "\n" ^ stmttos s
    | stmttos (Do (e, s)) =
        "  do " ^ exptos e ^ "\n" ^ stmttos s
    | stmttos (Store (width, dest, v, s)) =
        "  " ^ valtos dest ^ " :=" ^ widthtos width ^
        " " ^ valtos v ^ "\n" ^ stmttos s
    | stmttos (GotoIf (c, lab, s)) =
        "  if " ^ condtos c ^ " goto " ^ lab ^ "\n" ^ stmttos s
    | stmttos (Return v) = "  return " ^ valtos v
    | stmttos (Goto lab) = "  goto " ^ lab
    | stmttos End = "  end"

  fun w8vtos v =
    String.concat
    (Word8Vector.foldr (fn (e, b) =>
                        StringUtil.bytetohex (Word8.toInt e) :: b) nil v)

  fun progtos (Program { functions, main, globals }) =
    let
      fun blocktos (lab, s) =
        lab ^ ":\n" ^
        stmttos s

      fun func (s, Func { args, ret, body, blocks }) =
        "FUNC " ^ s ^ "(" ^ StringUtil.delimit ", "
        (map (fn (s, t) => s ^ " : " ^ typtos t) args) ^ ") : " ^
        typtos ret ^ " =\n" ^
        "start at " ^ body ^ "\n" ^
        StringUtil.delimit "\n" (map blocktos blocks)

      fun glob (s, Glob { typ, bytes, init }) = "GLOBAL " ^ s ^ " : " ^
        typtos typ ^ " =" ^
        (case bytes of
           NONE => ""
         | SOME v => " [" ^ w8vtos v ^ "]") ^
        (case init of
           NONE => "\n"
         | SOME { start, blocks } =>
             "\nstart at " ^ start ^ "\n" ^
             StringUtil.delimit "\n" (map blocktos blocks))
    in
      "Entry: " ^ main ^ "\n" ^
      StringUtil.delimit "\n" (map func functions) ^ "\n\n" ^
      StringUtil.delimit "\n" (map glob globals) ^ "\n"
    end

  structure Context :>
  sig
    type context
    val empty : context
    val lookup : context * string -> typ option
    val insert : context * string * typ -> context
  end =
  struct
    structure SM = SplayMapFn(type ord_key = string
                              val compare = String.compare)
    datatype context = C of { vars : typ SM.map }
    val empty = C { vars = SM.empty }
    fun lookup (C { vars, ... }, s) = SM.find (vars, s)
    fun insert (C { vars }, s, t) = C { vars = SM.insert (vars, s, t) }
  end
  type context = Context.context

end
