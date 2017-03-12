functor CILPass(A : CILPASSARG) :> CILPASS where type arg = A.arg =
struct
  type arg = A.arg

  exception CILPass of string
  open CIL

  fun convertt arg ctx typ =
    let val selves = { selft = convertt,
                       selfv = convertv,
                       selfe = converte,
                       selfs = converts }
      fun call f = f arg (selves, ctx)
    in
      case typ of
        Pointer a => call A.case_Pointer a
      | Array a => call A.case_Array a
      | Code a => call A.case_Code a
      | Struct a => call A.case_Struct a
      | Word32 a => call A.case_Word32 a
      | Word16 a => call A.case_Word16 a
      | Word8 a => call A.case_Word8 a
    end

  and convertv arg ctx v =
    let val selves = { selft = convertt,
                       selfv = convertv,
                       selfe = converte,
                       selfs = converts }
      fun call f = f arg (selves, ctx)
    in
      case v of
        Var a => call A.case_Var a
      | AddressLiteral a => call A.case_AddressLiteral a
      | FunctionLiteral a => call A.case_FunctionLiteral a
      | Word8Literal a => call A.case_Word8Literal a
      | Word16Literal a => call A.case_Word16Literal a
      | Word32Literal a => call A.case_Word32Literal a
      | StringLiteral a => call A.case_StringLiteral a
    end

  and converte arg ctx exp =
    let val selves = { selft = convertt,
                       selfv = convertv,
                       selfe = converte,
                       selfs = converts }
      fun call f = f arg (selves, ctx)
    in
      case exp of
        Value a => call A.case_Value a
      | Truncate a => call A.case_Truncate a
      | Promote a => call A.case_Promote a
      | Cast a => call A.case_Cast a
      | Plus a => call A.case_Plus a
      | Minus a => call A.case_Minus a
      | Times a => call A.case_Times a
      | SignedDivision a => call A.case_SignedDivision a
      | UnsignedDivision a => call A.case_UnsignedDivision a
      | UnsignedMod a => call A.case_UnsignedMod a
      | LeftShift a => call A.case_LeftShift a
      | RightShift a => call A.case_RightShift a
      | Greater a => call A.case_Greater a
      | GreaterEq a => call A.case_GreaterEq a
      | Above a => call A.case_Above a
      | AboveEq a => call A.case_AboveEq a
      | Less a => call A.case_Less a
      | LessEq a => call A.case_LessEq a
      | Below a => call A.case_Below a
      | BelowEq a => call A.case_BelowEq a
      | Eq a => call A.case_Eq a
      | Neq a => call A.case_Neq a
      | And a => call A.case_And a
      | Or a => call A.case_Or a
      | Xor a => call A.case_Xor a
      | Not a => call A.case_Not a
      | Yet a => call A.case_Yet a
      | Complement a => call A.case_Complement a
      | Negate a => call A.case_Negate a
      | Call a => call A.case_Call a
      | Load a => call A.case_Load a
      | Builtin a => call A.case_Builtin a
    end

  and converts arg ctx stmt =
    let val selves = { selft = convertt,
                       selfv = convertv,
                       selfe = converte,
                       selfs = converts }
      fun call f = f arg (selves, ctx)
    in
      case stmt of
        Bind a => call A.case_Bind a
      | Do a => call A.case_Do a
      | Store a => call A.case_Store a
      | GotoIf a => call A.case_GotoIf a
      | Return a => call A.case_Return a
      | Goto a => call A.case_Goto a
      | End => call A.case_End
    end
end

functor CILIdentity(type arg) :> CILPASSARG where type arg = arg =
struct
  infixr 9 `
  fun a ` b = a b

  type arg = arg
  open CIL
  type selves = { selft : arg -> context -> typ -> typ,
                  selfv : arg -> context -> value -> value * typ,
                  selfe : arg -> context -> exp -> exp * typ,
                  selfs : arg -> context -> stmt -> stmt }

  fun wordwidth Width8 = Word8 Unsigned
    | wordwidth Width16 = Word16 Unsigned
    | wordwidth Width32 = Word32 Unsigned
  fun fst (a, _) = a

  (* typ *)
  fun case_Pointer arg ({ selft, selfv, selfe, selfs }, ctx) t =
    Pointer ` selft arg ctx t
  fun case_Array arg ({ selft, selfv, selfe, selfs }, ctx) (t, i) =
    Array (selft arg ctx t, i)
  fun case_Code arg ({ selft, selfv, selfe, selfs }, ctx) (ret, args) =
    Code (selft arg ctx ret, map (selft arg ctx) args)
  fun case_Struct arg ({ selft, selfv, selfe, selfs }, ctx) members =
    Struct (ListUtil.mapsecond (selft arg ctx) members)
  fun case_Word32 arg ({ selft, selfv, selfe, selfs }, ctx) = Word32
  fun case_Word16 arg ({ selft, selfv, selfe, selfs }, ctx) = Word16
  fun case_Word8 arg ({ selft, selfv, selfe, selfs }, ctx) = Word8

  (* value *)
  fun case_Var arg ({ selft, selfv, selfe, selfs }, ctx) s =
    case Context.lookup (ctx, s) of
      NONE => raise CIL.CIL ("CILIdentity: unbound variable " ^ s)
    | SOME t => (Var s, t)
  fun case_AddressLiteral arg ({ selft, selfv, selfe, selfs }, ctx) (v, t) =
    let val t = selft arg ctx t
    in (AddressLiteral (v, t), Pointer t)
    end
  fun case_FunctionLiteral arg ({ selft, selfv, selfe, selfs }, ctx)
    (f, t, args) =
    let
      val t = selft arg ctx t
      val args = map (selft arg ctx) args
    in
      (FunctionLiteral (f, t, args), Code (t, args))
    end
  (* XXX this rewrites everything to unsigned -- ok?? *)
  fun case_Word8Literal arg ({ selft, selfv, selfe, selfs }, ctx) w =
    (Word8Literal w, Word8 Unsigned)
  fun case_Word16Literal arg ({ selft, selfv, selfe, selfs }, ctx) w =
    (Word16Literal w, Word16 Unsigned)
  fun case_Word32Literal arg ({ selft, selfv, selfe, selfs }, ctx) w =
    (Word32Literal w, Word32 Unsigned)
  fun case_StringLiteral arg ({ selft, selfv, selfe, selfs }, ctx) s =
    (StringLiteral s, Pointer ` Word8 Unsigned)

  (* exp *)
  fun case_Value arg ({ selft, selfv, selfe, selfs }, ctx) v =
    let val (v, t) = selfv arg ctx v
    in (Value v, t)
    end
  fun case_Truncate arg ({ selft, selfv, selfe, selfs }, ctx) { src, dst, v } =
    let
      val (v, t) = selfv arg ctx v
    in
      (* XXX check compatibility of src and t? *)
      (Truncate { src = src, dst = dst, v = v }, wordwidth dst)
    end
  fun case_Promote arg ({ selft, selfv, selfe, selfs }, ctx)
    { src, dst, v, signed } =
    let
      val (v, t) = selfv arg ctx v
    in
      (* XXX check compatibility of src and t? *)
      (Promote { src = src, dst = dst, signed = signed, v = v },
       wordwidth dst)
    end

  fun case_Cast arg ({ selft, selfv, selfe, selfs }, ctx) { src, dst, v } =
    let
      val (v, t) = selfv arg ctx v
      val src = selft arg ctx src
      val dst = selft arg ctx dst
    in
      (* XXX check compatibility of src and t, and that src/dst have
         the same representation? *)
      (Cast { src = src, dst = dst, v = v }, dst)
    end

  fun case_Plus arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Plus (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b), wordwidth w)
  fun case_Minus arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Minus (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b), wordwidth w)
  fun case_Times arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Times (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b), wordwidth w)
  fun case_SignedDivision arg ({ selft, selfv, selfe, selfs }, ctx)
                          (w, a, b) =
    (SignedDivision (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b), wordwidth w)
  fun case_UnsignedDivision arg ({ selft, selfv, selfe, selfs }, ctx)
                            (w, a, b) =
    (UnsignedDivision (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_UnsignedMod arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (UnsignedMod (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_LeftShift arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (LeftShift (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_RightShift arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (RightShift (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Greater arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Greater (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_GreaterEq arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (GreaterEq (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Above arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Above (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_AboveEq arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (AboveEq (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Less arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Less (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_LessEq arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (LessEq (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Below arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Below (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_BelowEq arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (BelowEq (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Eq arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Eq (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Neq arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Neq (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_And arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (And (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Or arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Or (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Xor arg ({ selft, selfv, selfe, selfs }, ctx) (w, a, b) =
    (Xor (w, fst ` selfv arg ctx a, fst ` selfv arg ctx b),
     wordwidth w)
  fun case_Not arg ({ selft, selfv, selfe, selfs }, ctx) (w, a) =
    (Not (w, fst ` selfv arg ctx a), wordwidth w)
  fun case_Yet arg ({ selft, selfv, selfe, selfs }, ctx) (w, a) =
    (Yet (w, fst ` selfv arg ctx a), wordwidth w)
  fun case_Complement arg ({ selft, selfv, selfe, selfs }, ctx) (w, a) =
    (Complement (w, fst ` selfv arg ctx a), wordwidth w)
  fun case_Negate arg ({ selft, selfv, selfe, selfs }, ctx) (w, a) =
    (Negate (w, fst ` selfv arg ctx a), wordwidth w)

  fun case_Call arg ({ selft, selfv, selfe, selfs }, ctx) (f, args) =
    (case selfv arg ctx f of
       (f, Code (ret, _)) =>
         let val args = map (fst o selfv arg ctx) args
         in
           (Call (f, args), ret)
         end
    | _ => raise CIL.CIL "CILIdentity: call to non-function")
  fun case_Load arg ({ selft, selfv, selfe, selfs }, ctx) (w, a) =
    (case selfv arg ctx a of
       (* XXX check compatibility of width and type? *)
       (a, Pointer t) => (Load (w, a), t)
    | _ => raise CIL.CIL "CILIdentity: load on non-pointer")

  fun case_Builtin arg ({ selft, selfv, selfe, selfs }, ctx) (b, args) =
    let
      val (ret, argtypes) = CIL.builtin_type b
    in
      (* XXX could also check types *)
      if length argtypes <> length args
      then raise CIL.CIL "CILIdentity: wrong number of args to builtin"
      else ();
      (Builtin (b, map (fst o selfv arg ctx) args), ret)
    end

  (* stmt *)
  fun case_Bind arg ({ selft, selfv, selfe, selfs }, ctx) (v, t, e, s) =
    let
      val (e, tv) = selfe arg ctx e
      val t = selft arg ctx t
      (* XXX check that t = tv? *)
      val ctx = Context.insert (ctx, v, t)
      val s = selfs arg ctx s
    in
      Bind (v, t, e, s)
    end
  fun case_Do arg ({ selft, selfv, selfe, selfs }, ctx) (e, s) =
    let
      val (e, t) = selfe arg ctx e
      val s = selfs arg ctx s
    in
      Do (e, s)
    end
  fun case_Store arg ({ selft, selfv, selfe, selfs }, ctx) (w, addr, v, s) =
    let
      val (addr, at) = selfv arg ctx addr
      val (v, vt) = selfv arg ctx v
      val s = selfs arg ctx s
    in
      Store (w, addr, v, s)
    end
  fun case_GotoIf arg ({ selft, selfv, selfe, selfs } : selves, ctx)
                      (c, lab, s) =
    let
      val c =
        case c of
           CLess (w, a, b) =>
             CLess (w, #1 ` selfv arg ctx a, #1 ` selfv arg ctx b)
         | CLessEq (w, a, b) =>
             CLessEq (w, #1 ` selfv arg ctx a, #1 ` selfv arg ctx b)
         | CBelow (w, a, b) =>
             CBelow (w, #1 ` selfv arg ctx a, #1 ` selfv arg ctx b)
         | CBelowEq (w, a, b) =>
             CBelowEq (w, #1 ` selfv arg ctx a, #1 ` selfv arg ctx b)
         | CEq (w, a, b) =>
             CEq (w, #1 ` selfv arg ctx a, #1 ` selfv arg ctx b)
         | CNeq (w, a, b) =>
             CNeq (w, #1 ` selfv arg ctx a, #1 ` selfv arg ctx b)
      val s = selfs arg ctx s
    in
      GotoIf (c, lab, s)
    end
  fun case_Goto arg ({ selft, selfv, selfe, selfs }, ctx) = Goto
  fun case_Return arg ({ selft, selfv, selfe, selfs }, ctx) NONE =
    Return NONE
    | case_Return arg ({ selft, selfv, selfe, selfs }, ctx) (SOME v) =
    let
      val (v, _) = selfv arg ctx v
    in
      Return (SOME v)
    end
  fun case_End arg ({ selft, selfv, selfe, selfs }, ctx) = End
end
