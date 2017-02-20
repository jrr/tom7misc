(* See my thesis, "Modal Types for Mobile Code", Tom Murphy VII, 2008. *)

signature CILPASSARG =
sig

  type arg
  type selves =
    { selft : arg -> CIL.context -> CIL.typ -> CIL.typ,
      selfv : arg -> CIL.context -> CIL.value -> CIL.value * CIL.typ,
      selfe : arg -> CIL.context -> CIL.exp -> CIL.exp * CIL.typ,
      selfs : arg -> CIL.context -> CIL.stmt -> CIL.stmt }

  (* typ cases *)
  val case_Pointer : arg -> selves * CIL.context -> CIL.typ -> CIL.typ
  val case_Code :
    arg -> selves * CIL.context -> CIL.typ * CIL.typ list -> CIL.typ
  val case_Struct :
    arg -> selves * CIL.context -> (string * CIL.typ) list -> CIL.typ
  val case_Word32 : arg -> selves * CIL.context -> CIL.signedness -> CIL.typ
  val case_Word16 : arg -> selves * CIL.context -> CIL.signedness -> CIL.typ
  val case_Word8 : arg -> selves * CIL.context -> CIL.signedness -> CIL.typ

  (* value cases *)
  val case_Var : arg -> selves * CIL.context -> string -> CIL.value * CIL.typ
  val case_AddressLiteral : arg -> selves * CIL.context ->
    CIL.loc * CIL.typ -> CIL.value * CIL.typ
  val case_FunctionLiteral : arg -> selves * CIL.context ->
    string * CIL.typ * CIL.typ list -> CIL.value * CIL.typ
  val case_Word8Literal :
    arg -> selves * CIL.context -> Word8.word -> CIL.value * CIL.typ
  val case_Word16Literal :
    arg -> selves * CIL.context -> Word16.word -> CIL.value * CIL.typ
  val case_Word32Literal :
    arg -> selves * CIL.context -> Word32.word -> CIL.value * CIL.typ
  val case_StringLiteral : arg -> selves * CIL.context -> string ->
    CIL.value * CIL.typ

  (* exp cases *)
  val case_Value : arg -> selves * CIL.context -> CIL.value -> CIL.exp * CIL.typ
  val case_Truncate : arg -> selves * CIL.context -> { src: CIL.width, dst: CIL.width, v: CIL.value } -> CIL.exp * CIL.typ
  val case_Promote : arg -> selves * CIL.context -> { signed: bool, src: CIL.width, dst: CIL.width, v: CIL.value } -> CIL.exp * CIL.typ
  val case_Plus : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Minus : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Times : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_SignedDivision : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_UnsignedDivision : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_UnsignedMod : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_LeftShift : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_RightShift : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Greater : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_GreaterEq : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Above : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_AboveEq : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Less : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_LessEq : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Below : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_BelowEq : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Eq : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Neq : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_And : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Or : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Xor : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value -> CIL.exp * CIL.typ
  val case_Not : arg -> selves * CIL.context -> CIL.width * CIL.value -> CIL.exp * CIL.typ
  val case_Complement : arg -> selves * CIL.context -> CIL.width * CIL.value -> CIL.exp * CIL.typ
  val case_Negate : arg -> selves * CIL.context -> CIL.width * CIL.value -> CIL.exp * CIL.typ
  val case_Call : arg -> selves * CIL.context -> CIL.value * CIL.value list -> CIL.exp * CIL.typ
  val case_Load : arg -> selves * CIL.context -> CIL.width * CIL.value -> CIL.exp * CIL.typ

  (* stmt cases *)
  val case_Bind : arg -> selves * CIL.context -> string * CIL.typ * CIL.exp * CIL.stmt -> CIL.stmt
  val case_Do : arg -> selves * CIL.context -> CIL.exp * CIL.stmt -> CIL.stmt
  val case_Store : arg -> selves * CIL.context -> CIL.width * CIL.value * CIL.value * CIL.stmt -> CIL.stmt
  val case_GotoIf : arg -> selves * CIL.context -> CIL.cond * string * CIL.stmt -> CIL.stmt
  val case_Return : arg -> selves * CIL.context -> CIL.value -> CIL.stmt
  val case_Goto : arg -> selves * CIL.context -> string -> CIL.stmt
  val case_End : arg -> selves * CIL.context -> CIL.stmt
end

signature CILPASS =
sig
  type arg
  val convertt : arg -> CIL.context -> CIL.typ -> CIL.typ
  val convertv : arg -> CIL.context -> CIL.value -> CIL.value * CIL.typ
  val converte : arg -> CIL.context -> CIL.exp -> CIL.exp * CIL.typ
  val converts : arg -> CIL.context -> CIL.stmt -> CIL.stmt
end
