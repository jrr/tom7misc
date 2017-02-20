structure ToCIL :> TOCIL =
struct

  infixr 9 `
  fun a ` b = a b

  exception ToCIL of string
  open CIL

  structure BC = CILUtil.BC
  type 'a bc = 'a BC.blockcollector

  (* Width of load/store for boolean temporaries.
     Anything will work, so this is just a
     performance preference.

     XXX Note that optimizations assume this too.
     *)
  val BOOL_WIDTH = Width16
  val BOOL_TYPE = Word16 Unsigned
  val LiteralTrue = Word16Literal ` Word16.fromInt 1
  val LiteralFalse = Word16Literal ` Word16.fromInt 0

  (* Maybe these should be functor arguments? *)
  val POINTER_WIDTH = Width16
  val POINTER_INT_TYPE = Word16 Signed
  val INT_WIDTH = Width16
  val INT_TYPE = Word16 Signed

  val genvar = CILUtil.genvar

  (* Literal at the given width. The (unsigned) integer must fit. *)
  fun unsigned_literal Width8 i =
      if i >= 0 andalso i < 256
      then Word8Literal ` Word8.fromInt i
      else raise ToCIL ("Unsigned 8-bit literal too large: " ^ Int.toString i)
    | unsigned_literal Width16 i =
      if i >= 0 andalso i < 65536
      then Word16Literal ` Word16.fromInt i
      else raise ToCIL ("Unsigned 16-bit literal too large: " ^ Int.toString i)
    | unsigned_literal Width32 i =
      if i >= 0 (* XXX check this? Hard to do portably *)
      then Word32Literal ` Word32.fromInt i
      else raise ToCIL ("Unsigned 32-bit literal too large: " ^ Int.toString i)

  fun signed_literal Width8 i =
      if i >= ~128 andalso i < 128
      then Word8Literal ` Word8.fromInt i
      else raise ToCIL ("Signed 8-bit literal too large: " ^ Int.toString i)
    | signed_literal Width16 i =
      if i >= ~32768 andalso i < 32768
      then Word16Literal ` Word16.fromInt i
      else raise ToCIL ("Signed 16-bit literal too large: " ^ Int.toString i)
    | signed_literal Width32 i =
      if true (* XXX check *)
      then Word32Literal ` Word32.fromInt i
      else raise ToCIL ("Signed 32-bit literal too large: " ^ Int.toString i)

  datatype normop = PLUS | MINUS | TIMES | DIVIDE | MOD | GT | LT | GTE | LTE |
    EQ | NEQ | BITOR | BITAND | BITXOR
  datatype shortop = AND | OR
  datatype shiftop = LSHIFT | RSHIFT

  datatype binopclass =
      SHORT_CIRCUIT of shortop
    | NORMAL of normop
    | SHIFT of shiftop
    | ASSIGNING of normop
    | ASSIGNING_SHIFT of shiftop

  fun binopclass (b : Ast.binop) =
    case b of
      Ast.Plus => NORMAL PLUS
    | Ast.Minus => NORMAL MINUS
    | Ast.Times => NORMAL TIMES
    | Ast.Divide => NORMAL DIVIDE
    | Ast.Mod => NORMAL MOD
    | Ast.Gt => NORMAL GT
    | Ast.Lt => NORMAL LT
    | Ast.Gte => NORMAL GTE
    | Ast.Lte => NORMAL LTE
    | Ast.Eq => NORMAL EQ
    | Ast.Neq => NORMAL NEQ
    | Ast.BitOr => NORMAL BITOR
    | Ast.BitAnd => NORMAL BITAND
    | Ast.BitXor => NORMAL BITXOR
    | Ast.Lshift => SHIFT LSHIFT
    | Ast.Rshift => SHIFT RSHIFT
    | Ast.And => SHORT_CIRCUIT AND
    | Ast.Or => SHORT_CIRCUIT OR
    | Ast.BinopExt _ => raise ToCIL "No binop extensions are supported"
    | Ast.PlusAssign => ASSIGNING PLUS
    | Ast.MinusAssign => ASSIGNING MINUS
    | Ast.TimesAssign => ASSIGNING TIMES
    | Ast.DivAssign => ASSIGNING DIVIDE
    | Ast.ModAssign => ASSIGNING MOD
    | Ast.XorAssign => ASSIGNING BITXOR
    | Ast.OrAssign => ASSIGNING BITOR
    | Ast.AndAssign => ASSIGNING BITAND
    | Ast.LshiftAssign => ASSIGNING_SHIFT LSHIFT
    | Ast.RshiftAssign => ASSIGNING_SHIFT RSHIFT

  (* For local variables, disambiguate the symbol with the uid. *)
  fun idstring ({ name, uid, ... } : Ast.id) =
    Symbol.name name ^ "$" ^ Pid.toString uid
  (* For globals and functions, we just use the name. These must be
     globally unique and we could use them for linking (?). *)
  fun uidstring ({ name, ... } : Ast.id) = Symbol.name name
  fun newidstring s = s ^ "$$" ^ Pid.toString (Pid.new ())

  fun labstring ({ name, ... } : Ast.label) = Symbol.name name

  fun transtype (t : Ast.ctype) =
    case t of
      Ast.Error => raise ToCIL "Error cannot be translated"
    | Ast.Void => Struct []
    | Ast.Ellipses => raise ToCIL "... unsupported (as yet)"
    (* We ignore const (just affects error checking and optimization)
       and volatile (means nothing) *)
    | Ast.Qual (_, t) => transtype t
    (* Array should be represented as pointer, right? *)
    | Ast.Array _ => raise ToCIL "Array types unimplemented."
    | Ast.Pointer t => Pointer (transtype t)
    | Ast.Function (ret, args) =>
        Code (transtype ret, map (transtype o #1) args)
    | Ast.StructRef tid =>
        raise ToCIL "unimplemented: need to look up struct and inline it"
    | Ast.UnionRef tid => raise ToCIL "unions unimplemented"
    (* All enums currently represented as signed int32.
       PERF: Better if we can use 8/16 in cases where they'll work. *)
    | Ast.EnumRef _ => Word32 Signed
    | Ast.TypeRef tid =>
          raise ToCIL "unimplemented: need to look up typedef and return it"
    | Ast.Numeric (_, _, signedness, intkind, _) =>
       let
         val signedness =
           case signedness of
             Ast.SIGNED => Signed
           | Ast.UNSIGNED => Unsigned
       in
         case intkind of
           Ast.CHAR => Word8 signedness
         | Ast.SHORT => Word16 signedness
         | Ast.INT => Word16 signedness
         | Ast.LONG => Word32 signedness
         (* Note that this could just be translated as Word32, but that would
            not be compatible with C99. *)
         | Ast.LONGLONG => raise ToCIL "unimplemented: long long"
         | _ => raise ToCIL "unimplemented: floating point (type)"
       end

  fun typewidth t =
    case t of
      Pointer _ => Width32
    | Code _ => Width32
    | Struct _ => raise ToCIL "unimplemented: struct lvalues"
    | Word32 _ => Width32
    | Word16 _ => Width16
    | Word8 _ => Width8

  fun ctypewidth t = typewidth ` transtype t

  fun typesignedness t =
    case t of
      Word32 s => s
    | Word16 s => s
    | Word8 s => s
    (* XXX pointers can be compared as integers -- when does that happen? *)
    | _ => raise ToCIL ("Tried to compute signedness of non-numeric type " ^
                        typtos t ^ "(binop applied to it?)")

  (* Compute the "join" of two types used in an arithmetic expression.

     This page is a good explanation of the rules:
     http://securecoding.cert.org/confluence/display/c/
       INT02-C.+Understand+integer+conversion+rules
       *)
  fun jointypes oat obt =
    let
      fun bits (Word8 _) = 8
        | bits (Word16 _) = 16
        | bits (Word32 _) = 32
        | bits _ = raise ToCIL ("attempt to join non-word types. probably " ^
                                "do need to support pointers here, though")

      (* First of all, they need to be promoted to at least "int". *)
      fun up t = if bits t < bits INT_TYPE
                 then INT_TYPE
                 else t
      val at = up oat
      val bt = up obt

      fun sign Unsigned _ = Unsigned
        | sign _ Unsigned = Unsigned
        | sign Signed Signed = Signed
    in
      if bits INT_TYPE < 16
      then raise ToCIL "int type < 16 bits not supported here"
      else ();
      case (at, bt) of
        (Word16 a, Word16 b) => Word16 (sign a b)
      | (Word32 a, Word32 b) => Word32 (sign a b)
      | (Word32 s, Word16 _) => Word32 s
      | (Word16 _, Word32 s) => Word32 s
      | _ => raise ToCIL ("unsupported types to arithmetic operator: " ^
                          typtos oat ^ " (became " ^ typtos at ^ ") and " ^
                          typtos obt ^ " (became " ^ typtos bt ^ ")")
    end

  (* Implicit coercion when we know the source and destination types. *)
  fun implicit { v : value, src : typ, dst : typ }
               (k : value * typ -> stmt) : stmt =
    if eq_typ (src, dst) then k (v, dst)
    else
      let
        (* true if a promotion from a to b should be sign-extended *)
        fun extend Signed _ = true
          | extend _ _ = false
      in
      (case (src, dst) of
         (* Signedness is just a translation artifact; there's nothing to do. *)
         (Word32 _, Word32 _) => k (v, dst)
       | (Word16 _, Word16 _) => k (v, dst)
       | (Word8 _, Word8 _) => k (v, dst)
       | (Word8 sa, Word16 sb) =>
           let val var = genvar "p"
           in Bind (var, dst,
                    Promote { v = v, src = Width8, dst = Width16,
                              signed = extend sa sb },
                    k (Var var, dst))
           end
       | (Word8 sa, Word32 sb) =>
           let val var = genvar "p"
           in Bind (var, dst,
                    Promote { v = v, src = Width8, dst = Width32,
                              signed = extend sa sb },
                    k (Var var, dst))
           end
       | (Word16 sa, Word32 sb) =>
           let val var = genvar "p"
           in Bind (var, dst,
                    Promote { v = v, src = Width16, dst = Width32,
                              signed = extend sa sb },
                    k (Var var, dst))
           end

       | (Word16 sa, Word8 sb) =>
           let val var = genvar "t"
           in Bind (var, dst,
                    Truncate { v = v, src = Width16, dst = Width8 },
                    k (Var var, dst))
           end
       | (Word32 sa, Word8 sb) =>
           let val var = genvar "t"
           in Bind (var, dst,
                    Truncate { v = v, src = Width32, dst = Width8 },
                    k (Var var, dst))
           end
       | (Word32 sa, Word16 sb) =>
           let val var = genvar "t"
           in Bind (var, dst,
                    Truncate { v = v, src = Width32, dst = Width16 },
                    k (Var var, dst))
           end

       | _ => raise ToCIL
           ("implicit coercion from " ^ typtos src ^ " to " ^
            typtos dst ^ " not allowed/implemented. Try casting?"))
      end

  (* Give the constructor and return type for a given AST binop. Both of
     its arguments must have been promoted to the same numeric typ t. *)
  fun opconstructor bop (t : typ) : (value * value -> exp) * typ =
    let
      val width = typewidth t
      val signedness = typesignedness t
      fun atwidth f (a, b) = f (width, a, b)
    in
      case bop of
        PLUS => (atwidth Plus, t)
      | MINUS => (atwidth Minus, t)
      | TIMES => (atwidth Times, t)
      | DIVIDE =>
          (* UnsignedDivision *)
          raise ToCIL "division unimplemented because of signedness"
      | MOD =>
          (* UnsignedMod *)
          raise ToCIL "modulus unimplemented because of signedness"
      | GT => (case signedness of
                 Signed => (atwidth Greater, BOOL_TYPE)
               | Unsigned => (atwidth Above, BOOL_TYPE))
      | LT => (case signedness of
                 Signed => (atwidth Less, BOOL_TYPE)
               | Unsigned => (atwidth Below, BOOL_TYPE))
      | GTE => (case signedness of
                  Signed => (atwidth GreaterEq, BOOL_TYPE)
                | Unsigned => (atwidth AboveEq, BOOL_TYPE))
      | LTE => (case signedness of
                  Signed => (atwidth LessEq, BOOL_TYPE)
                | Unsigned => (atwidth BelowEq, BOOL_TYPE))
      | EQ => (atwidth Eq, BOOL_TYPE)
      | NEQ => (atwidth Neq, BOOL_TYPE)
      | BITOR => (atwidth Or, t)
      | BITAND => (atwidth And, t)
      | BITXOR => (atwidth Xor, t)
    end

  (* Translate an expression as an lvalue. This means producing a value
     that is the lvalue's address. We also pass to the continuation the
     width of the lvalue. And the type of the lvalue (not the address;
     the contents of the address). *)
  fun translvalue (Ast.EXPR (e, _, _) : Ast.expression) (bc : stmt bc)
                  (k : value * width * typ -> stmt) : stmt =
    case e of
      Ast.Id (id as { global = false, ctype, ... }) =>
        let val typ = transtype ctype
        in k (AddressLiteral (Local ` idstring id, typ), typewidth typ, typ)
        end
    | Ast.Id (id as { global = true, ctype, ... }) =>
        let val typ = transtype ctype
        in k (AddressLiteral (Global ` uidstring id, typ), typewidth typ, typ)
        end
    | Ast.Sub (ptr, idx) =>
        transexp ptr bc
        (fn (ptrv, ptrt) =>
         case ptrt of
           Pointer t =>
             let
               val argwidth = typewidth t
               val scaled_idx = genvar "sidx"
               val addr = genvar "off"
               fun scale Width32 v =
                 LeftShift (POINTER_WIDTH, v, Word8Literal 0w2)
                 | scale Width16 v =
                 LeftShift (POINTER_WIDTH, v, Word8Literal 0w1)
                 | scale Width8 v = Value v
             in
               transexp idx bc
               (fn (idxv, idxt) =>
                Bind (scaled_idx, POINTER_INT_TYPE,
                      scale argwidth idxv,
                      (* XXX note this treats pointer as int; should
                         we have explicit conversion? *)
                      Bind (addr, ptrt,
                            Plus (POINTER_WIDTH, ptrv, Var scaled_idx),
                            k (Var addr, argwidth, ptrt))))
             end
         | _ => raise ToCIL ("Attempt to subscript something of " ^
                             "non-array type: " ^ typtos ptrt))

    | Ast.Member _ => raise ToCIL "unimplemented lvalue: Member"
    | Ast.Arrow _ => raise ToCIL "unimplemented lvalue: Arrow"
    | Ast.Deref _ => raise ToCIL "unimplemented lvalue: Deref"

    | _ => raise ToCIL "illegal/unimplemented lvalue"


  and transexplist (es : Ast.expression list) (bc : stmt bc)
                   (k : (value * typ) list -> stmt) : stmt =
    let
      fun tel revl nil = k (rev revl)
        | tel revl (e :: rest) =
        transexp e bc (fn (v, t) => tel ((v, t) :: revl) rest)
    in
      tel nil es
    end

  (* Normal translation of an expression (rvalue). *)
  and transexp (orig_exp as Ast.EXPR (e, _, _) : Ast.expression) (bc : stmt bc)
               (k : value * typ -> stmt) : stmt =
    let
      fun as_lvalue s =
        translvalue orig_exp bc
        (fn (addr, width, typ) =>
         let
           val v = genvar s
         in
           Bind (v, typ, Load (width, addr), k (Var v, typ))
         end)
    in
      case e of
        (* XXX need some way to deduce the types of literals.
           Possibly they can always be signed, and then we
           can rewrite to smaller literals later as an optimization?
           Looks like this may work. *)
        Ast.IntConst i =>
          let
            val typ = Word32 Signed
          in
            k (Word32Literal ` Word32.fromLargeInt i, typ)
          end
      | Ast.RealConst r => raise ToCIL "unimplemented: floating point (literal)"

      (* XXX do function-scope variables declared "static" show as globals?
         we don't currently initialize them, but we need to. *)
      | Ast.Id id => as_lvalue (idstring id)
      | Ast.Sub _ => as_lvalue "sub"
      | Ast.Member _ => as_lvalue "mem"
      | Ast.Arrow _ => as_lvalue "arrow"
      | Ast.Deref _ => as_lvalue "deref"

      | Ast.Binop (bop, a, b) =>
          (case binopclass bop of
             SHORT_CIRCUIT AND =>
               (* a && b is like a ? !!b : false. *)
               transexp a bc
               (fn (av, at) =>
                implicit { src = at, dst = BOOL_TYPE, v = av }
                (fn (av, _) =>
                 let
                   val res = newidstring "andr"
                   val resv = genvar "r"
                   val nresv = genvar "nr"
                   val nnresv = genvar "nnr"
                   val true_lab = BC.genlabel "andtrue"
                   val done_lab = BC.genlabel "anddone"
                 in
                   BC.insert
                   (bc, true_lab,
                    transexp b bc
                    (fn (bv : value, bt : typ) =>
                     implicit { src = bt, dst = BOOL_TYPE, v = bv }
                     (fn (bv, _) =>
                      Bind (nresv, BOOL_TYPE,
                            Not (BOOL_WIDTH, bv),
                            Bind (nnresv, BOOL_TYPE,
                                  Not (BOOL_WIDTH, Var nresv),
                                  Store (BOOL_WIDTH,
                                         AddressLiteral (Local res, BOOL_TYPE),
                                         Var nnresv,
                                         Goto done_lab))))));

                   BC.insert
                   (bc, done_lab,
                    Bind (resv, BOOL_TYPE,
                          Load (BOOL_WIDTH,
                                AddressLiteral (Local res, BOOL_TYPE)),
                          k (Var resv, BOOL_TYPE)));

                   GotoIf (CNeq (BOOL_WIDTH, av, LiteralFalse), true_lab,
                           (* condition fails; store false *)
                           Store (BOOL_WIDTH,
                                  AddressLiteral (Local res, BOOL_TYPE),
                                  LiteralFalse,
                                  Goto done_lab))
                 end))

           | SHORT_CIRCUIT OR =>
               (* a || b is like a ? true : !!b. *)
               transexp a bc
               (fn (av, at) =>
                implicit { src = at, dst = BOOL_TYPE, v = av }
                (fn (av, _) =>
                let
                  val res = newidstring "orr"
                  val resv = genvar "r"
                  val nresv = genvar "nr"
                  val nnresv = genvar "nnr"
                  val true_lab = BC.genlabel "ortrue"
                  val done_lab = BC.genlabel "ordone"
                in
                  BC.insert (bc, true_lab,
                             Store (BOOL_WIDTH,
                                    AddressLiteral (Local res, BOOL_TYPE),
                                    LiteralTrue,
                                    Goto done_lab));

                  BC.insert
                  (bc, done_lab,
                   Bind (resv, BOOL_TYPE,
                         Load (BOOL_WIDTH,
                               AddressLiteral (Local res, BOOL_TYPE)),
                         k (Var resv, BOOL_TYPE)));

                  GotoIf
                  (CNeq (BOOL_WIDTH, av, LiteralFalse), true_lab,
                   (* fall through to false branch *)
                   transexp b bc
                   (fn (bv, bt) =>
                    implicit { src = bt, dst = BOOL_TYPE, v = bv }
                    (fn (bv, _) =>
                     Bind (nresv, BOOL_TYPE,
                           Not (BOOL_WIDTH, bv),
                           Bind (nnresv, BOOL_TYPE,
                                 Not (BOOL_WIDTH, Var nresv),
                                 Store (BOOL_WIDTH,
                                        AddressLiteral (Local res, BOOL_TYPE),
                                        Var nnresv,
                                        Goto done_lab))))))
                end))

           | ASSIGNING bop =>
               translvalue a bc
               (fn (addr, width, ltyp) =>
                transexp b bc
                (fn (bv, bt) =>
                 let
                   (* if we have uchar += uint32, the operation is
                      an 8-bit one and the result is 8-bit. *)
                   val oldv = genvar "assopold"
                   val newv = genvar "assopnew"
                   val (ctor, rett) = opconstructor bop ltyp
                 in
                   implicit { src = bt, dst = ltyp, v = bv }
                   (fn (bv, _) =>
                    Bind (oldv, ltyp,
                          Load (width, addr),
                          Bind (newv, rett,
                                ctor (Var oldv, bv),
                                Store (width, addr,
                                       Var newv,
                                       k (Var newv, rett)))))
                 end))

           | ASSIGNING_SHIFT bop =>
               translvalue a bc
               (fn (addr, width, ltyp) =>
                transexp b bc
                (fn (bv, bt) =>
                 let
                   val oldv = genvar "ashiftold"
                   val newv = genvar "ashiftnew"
                   val ctor =
                     case bop of
                       LSHIFT => LeftShift
                     | RSHIFT => RightShift
                 in
                   implicit { src = bt, dst = Word8 Unsigned, v = bv }
                   (fn (bv, _) =>
                    Bind (oldv, ltyp,
                          Load (width, addr),
                          Bind (newv, ltyp,
                                ctor (width, Var oldv, bv),
                                Store (width, addr,
                                       Var newv,
                                       k (Var newv, ltyp)))))
                 end))

           | SHIFT bop =>
               transexp a bc
               (fn (av, at) =>
                transexp b bc
                (fn (bv, bt) =>
                 let
                   val v = genvar "shift"
                   val ctor =
                     case bop of
                       LSHIFT => LeftShift
                     | RSHIFT => RightShift
                 in
                   implicit { src = bt, dst = Word8 Unsigned, v = bv }
                   (fn (bv, _) =>
                    Bind (v, at, ctor (typewidth at, av, bv), k (Var v, at)))
                 end))

           | NORMAL bop =>
               transexp a bc
               (fn (av, at) =>
                transexp b bc
                (fn (bv, bt) =>
                 let
                   val targettype = jointypes at bt
                   val v = genvar "b"
                   val (ctor, rett) = opconstructor bop targettype
                 in
                   implicit { src = at, dst = targettype, v = av }
                   (fn (av, _) =>
                    implicit { src = bt, dst = targettype, v = bv }
                    (fn (bv, _) =>
                     Bind (v, rett, ctor (av, bv), k (Var v, rett))))
                 end)))

      | Ast.Unop (uop, a) =>
           (case uop of
              Ast.Uplus =>
                (* This does nothing.
                   (XXX technically are there integer promotions
                   or anything?) *)
                transexp a bc k
            | Ast.Not => transexp a bc
                (fn (av, at) =>
                 let val v = genvar "u"
                 in
                   implicit { src = at, dst = BOOL_TYPE, v = av }
                   (fn (av, _) =>
                    Bind (v, BOOL_TYPE,
                          Not (BOOL_WIDTH, av), k (Var v, BOOL_TYPE)))
                 end)
            | Ast.Negate => transexp a bc
                (fn (av, at) =>
                 let val v = genvar "u"
                 in Bind (v, at,
                          Negate (typewidth at, av), k (Var v, at))
                 end)
            | Ast.BitNot => transexp a bc
                (fn (av, at) =>
                 let
                   val v = genvar "u"
                 in Bind (v, at,
                          Complement (typewidth at, av), k (Var v, at))
                 end)
            | Ast.UnopExt _ => raise ToCIL "unop extensions unsupported"
            | Ast.PreInc =>
                translvalue a bc
                (fn (addr, width, ltyp) =>
                 let
                   val oldv = genvar "preincold"
                   val newv = genvar "preincnew"
                 in
                   Bind (oldv, ltyp,
                         Load (width, addr),
                         Bind (newv, ltyp,
                               Plus (width, Var oldv,
                                     case typesignedness ltyp of
                                       Unsigned => unsigned_literal width 1
                                     | Signed => signed_literal width 1),
                               Store (width, addr,
                                      Var newv,
                                      k (Var newv, ltyp))))
                 end)
            | Ast.PreDec =>
                translvalue a bc
                (fn (addr, width, ltyp) =>
                 let
                   val oldv = genvar "predecold"
                   val newv = genvar "predecnew"
                 in
                   Bind (oldv, ltyp,
                         Load (width, addr),
                         Bind (newv, ltyp,
                               Minus (width, Var oldv,
                                      case typesignedness ltyp of
                                        Unsigned => unsigned_literal width 1
                                      | Signed => signed_literal width 1),
                               Store (width, addr,
                                      Var newv,
                                      k (Var newv, ltyp))))
                 end)

            | Ast.PostInc =>
                translvalue a bc
                (fn (addr, width, ltyp) =>
                 let
                   val oldv = genvar "postincold"
                   val newv = genvar "postincnew"
                 in
                   Bind (oldv, ltyp,
                         Load (width, addr),
                         Bind (newv, ltyp,
                               Plus (width, Var oldv,
                                     case typesignedness ltyp of
                                       Unsigned => unsigned_literal width 1
                                     | Signed => signed_literal width 1),
                               Store (width, addr,
                                      Var newv,
                                      k (Var oldv, ltyp))))
                 end)
            | Ast.PostDec =>
                translvalue a bc
                (fn (addr, width, ltyp) =>
                 let
                   val oldv = genvar "postdecold"
                   val newv = genvar "postdecnew"
                 in
                   Bind (oldv, ltyp,
                         Load (width, addr),
                         Bind (newv, ltyp,
                               Minus (width, Var oldv,
                                      case typesignedness ltyp of
                                        Unsigned => unsigned_literal width 1
                                      | Signed => signed_literal width 1),
                               Store (width, addr, Var newv,
                                      k (Var oldv, ltyp))))
                 end))

      | Ast.StringConst s => raise ToCIL "unimplemented: string constants"
      | Ast.Call (f, args) =>
            transexp f bc
            (fn (fv, ft) =>
             case ft of
               Code (rett, argt) =>
                 transexplist args bc
                 (fn args =>
                  let
                    val retv = genvar "call"
                    fun implicitargs racc nil nil =
                      Bind (retv, rett,
                            Call (fv, rev racc), k (Var retv, rett))
                      | implicitargs racc (t :: trest) ((v, vt) :: vrest) =
                          implicit { src = vt, dst = t, v = v }
                          (fn (v, vt) =>
                           implicitargs (v :: racc) trest vrest)
                      | implicitargs _ _ _ =
                          raise ToCIL "argument number mismatch in Call"
                  in
                    implicitargs nil argt args
                  end)
             | _ => raise ToCIL ("tried to call value of non-function " ^
                                 "type " ^ typtos ft))

      | Ast.QuestionColon (cond, te, fe) =>
            transexp cond bc
            (fn (condv, condt) =>
             let
               (* Use a new local variable to store the result so that
                  variables don't have to span basic blocks. XXX need to
                  sort out whether this is necessary. *)
               val res = newidstring "ternr"

               (* Hack attack!
                  Do a throwaway translation of the true and false
                  branches, because we need to know types they'll produce,
                  unfortunately, ahead of time.

                  PERF Note that this results in exponential time
                  compilation for nested ?: expressions, but it doesn't
                  seem like an easy-to-fix problem. *)
               fun exptype e =
                 let
                   (* throw-away blockcollector. *)
                   val bc = BC.empty ()
                   val tr = ref ` Struct nil
                 in
                   ignore (transexp e bc
                           (fn (_, t) =>
                            let in
                              tr := t;
                              End
                            end));
                   !tr
                 end

               val synthtyp = jointypes (exptype te) (exptype fe)
               val width = typewidth synthtyp
               val resv = genvar "r"
               val true_lab = BC.genlabel "ternt"
               val done_lab = BC.genlabel "terndone"
             in
               BC.insert (bc, true_lab,
                          transexp te bc
                          (fn (tv, tt) =>
                           implicit { src = tt, dst = synthtyp, v = tv }
                           (fn (tv, _) =>
                            Store (width, AddressLiteral (Local res, synthtyp),
                                   tv,
                                   Goto done_lab))));
               BC.insert (bc, done_lab,
                          Bind (resv, synthtyp,
                                Load (width,
                                      AddressLiteral (Local res, synthtyp)),
                                k (Var resv, synthtyp)));

               implicit { src = condt, dst = BOOL_TYPE, v = condv }
               (fn (condv, _) =>
                GotoIf (CNeq (BOOL_WIDTH, condv, LiteralFalse), true_lab,
                        (* fall through to false branch *)
                        transexp fe bc
                        (fn (fv, ft) =>
                         implicit { src = ft, dst = synthtyp, v = fv }
                         (fn (fv, _) =>
                          Store (width, AddressLiteral (Local res, synthtyp),
                                 fv,
                                 Goto done_lab)))))
             end)

      | Ast.Assign (dst, rhs) =>
            translvalue dst bc
            (fn (dstaddr, dstwidth : width, dsttyp : typ) =>
             transexp rhs bc
             (fn (rhsv, rhst) =>
              implicit { src = rhst, dst = dsttyp, v = rhsv }
              (fn (rhsv, _) =>
               Store (dstwidth, dstaddr, rhsv,
                      k (rhsv, rhst)))))

      | Ast.Comma (a, b) => transexp a bc (fn _ => transexp b bc k)

      | Ast.Cast (ctype, e) =>
            transexp e bc
            (fn (ev, et) =>
             let
               val t = transtype ctype
               (* Anything that can be implicitly converted, so
                  floats should also be here? *)
               fun isnumeric (Word8 _) = true
                 | isnumeric (Word16 _) = true
                 | isnumeric (Word32 _) = true
                 | isnumeric _ = false
             in
               if isnumeric t andalso isnumeric et
               then
                 implicit { src = et, dst = t, v = ev } k
               else
                 raise ToCIL "non-numeric casts as yet unimplemented"
             end)
      | Ast.EnumId (m, n) => k (unsigned_literal Width32 (LargeInt.toInt n),
                                Word32 Unsigned)

      | Ast.AddrOf _ => raise ToCIL "unimplemented: AddrOf"
      | Ast.SizeOf _ => raise ToCIL "unimplemented: sizeof"
      | Ast.ExprExt _ => raise ToCIL "expression extensions not supported"
      | Ast.ErrorExpr => raise ToCIL "encountered ErrorExpr"
    end

  (* Typedecls ignored currently. *)
  fun transdecl (Ast.TypeDecl _) (bc : stmt bc) (k : unit -> stmt) : stmt = k ()
    (* Should probably still add it to a context? *)
    | transdecl (Ast.VarDecl (id, NONE)) bc k = k ()
    | transdecl (Ast.VarDecl (id as { ctype, global, ...}, SOME init)) bc k =
    (case init of
       Ast.Simple e =>
         let
           val addrkind = if global then Global else Local
           val vartyp = transtype ctype
         in
           transexp e bc
           (fn (v, t) =>
            implicit { src = t, dst = vartyp, v = v }
            (fn (v, _) =>
             Store (typewidth vartyp,
                    AddressLiteral (addrkind ` uidstring id, vartyp),
                    v, k ())))
         end
     | Ast.Aggregate _ =>
         raise ToCIL "aggregate initialization unimplemented (decl)")

  (* When translating a statement, the 'break' and 'continue' targets
     the label to jump to when seeing those statements. *)
  fun transstatementlist nil _ _ k : CIL.stmt = k ()
    | transstatementlist (s :: t)
                         (targs : { break : string option,
                                    continue : string option })
                         (bc : stmt bc)
                         (k : unit -> stmt) =
    transstatement s targs bc (fn () => transstatementlist t targs bc k)

  and transstatement (Ast.STMT (s, orig_id, orig_loc) : Ast.statement)
                     (targs : { break : string option,
                                continue : string option })
                     (bc : stmt bc)
                     (k : unit -> CIL.stmt) : CIL.stmt =
    case s of
      Ast.Expr NONE => k ()
    | Ast.Expr (SOME e) => transexp e bc (fn v => k ())
    | Ast.ErrorStmt => raise ToCIL "encountered ErrorStmt"
    | Ast.Compound (decls, stmts) =>
        let
          fun dodecls nil = transstatementlist stmts targs bc k
            | dodecls (h :: t) = transdecl h bc (fn () => dodecls t)
        in
          dodecls decls
        end

    (* Some statements ignore k because any following code is unreachable. *)
    | Ast.Return NONE => Return (unsigned_literal Width8 0)
    (* XXX need to coerce return type to the type the function wants? *)
    | Ast.Return (SOME e) => transexp e bc (fn (v, t) => Return v)
    | Ast.IfThen (e, body) =>
        transexp e bc
        (fn (cond, condt) =>
         let
           val true_label = BC.genlabel "if_t"
           val rest_label = BC.genlabel "if_r"
         in
           BC.insert (bc, true_label,
                      transstatement body targs bc
                      (fn () => Goto rest_label));
           BC.insert (bc, rest_label, k ());
           (* XXX cond may not be bool_width. can't just truncate
              because we could lose high one bits. So we should
              really be generating a 32-bit version in that
              case? *)
           GotoIf (CNeq (BOOL_WIDTH, cond, LiteralFalse), true_label,
                   Goto rest_label)
         end)
    | Ast.IfThenElse (e, true_body, false_body) =>
        transexp e bc
        (fn (cond, condt) =>
         let
           val true_label = BC.genlabel "if_t"
           val rest_label = BC.genlabel "if_r"
         in
           BC.insert (bc, true_label,
                      transstatement true_body targs bc
                      (fn () => Goto rest_label));
           BC.insert (bc, rest_label, k ());
           (* XXX as above. *)
           GotoIf (CNeq (BOOL_WIDTH, cond, LiteralFalse), true_label,
                   transstatement false_body targs bc
                   (fn () => Goto rest_label))
         end)
    | Ast.Goto lab => Goto (labstring lab)

    | Ast.While (cond, body) =>
        let
          val body_label = BC.genlabel "whilebody"
          val done_label = BC.genlabel "whiledone"
          val body_targs =
            { break = SOME done_label,
              continue = SOME body_label }
        in
          BC.insert (bc, done_label, k ());
          BC.insert (bc, body_label,
                     transexp cond bc
                     (fn (condv, condt) =>
                      (* XXX as above *)
                      (* condv == false, i.e., !condv *)
                      GotoIf (CEq (BOOL_WIDTH, condv, LiteralFalse),
                              done_label,
                              transstatement body body_targs bc
                              (fn () => Goto body_label))));
          Goto body_label
        end

    | Ast.Do (cond, body) =>
        let
          val test_label = BC.genlabel "dotest"
          val body_label = BC.genlabel "dobody"
          val done_label = BC.genlabel "dodone"
          val body_targs =
            { break = SOME done_label,
              continue = SOME test_label }
        in
          BC.insert (bc, done_label, k ());
          BC.insert (bc, test_label,
                     transexp cond bc
                     (fn (condv, condt) =>
                      (* XXX as above *)
                      GotoIf (CNeq (BOOL_WIDTH, condv, LiteralFalse),
                              body_label,
                              Goto done_label)));
          BC.insert (bc, body_label,
                     transstatement body body_targs bc
                     (fn () => Goto test_label));
          Goto body_label
        end

    | Ast.For (init, cond, inc, body) =>
        let
          val start_label = BC.genlabel "forstart"
          val inc_label = BC.genlabel "forinc"
          val done_label = BC.genlabel "fordone"

          val body_targs =
            { break = SOME done_label,
              continue = SOME inc_label }
        in
          (* Increment and then jump to the while condition.
             This is the target of a 'continue'.
             We can definitely produce simpler code when there
             is no increment, but this is rare and better handled
             in the optimizer anyway. *)
          BC.insert (bc, inc_label,
                     case inc of
                       NONE => Goto start_label
                     | SOME e =>
                         transexp e bc
                         (fn _ => Goto start_label));

          (* Test the while condition and either enter the
             loop or jump past it. *)
          BC.insert (bc, start_label,
                     case cond of
                       NONE => (transstatement body body_targs bc
                                (fn () => Goto inc_label))
                     | SOME c =>
                         transexp c bc
                         (fn (condv, condt) =>
                          (* XXX as above *)
                          (* cond == false, i.e. !cond *)
                          GotoIf (CEq (BOOL_WIDTH, condv, LiteralFalse),
                                  done_label,
                                  transstatement body body_targs bc
                                  (fn () => Goto inc_label))));

          (* When we're done, it's just the current continuation. *)
          BC.insert (bc, done_label, k ());

          case init of
            NONE => Goto start_label
          | SOME e => transexp e bc (fn _ => Goto start_label)
        end

    | Ast.Labeled (lab, s) =>
        let val l = labstring lab
        in BC.insert (bc, l, transstatement s targs bc k);
           Goto l
        end

    | Ast.Break =>
        (case targs of
           { break = SOME lab, ... } => Goto lab
         | _ => raise ToCIL "break statement without target")
    | Ast.Continue =>
        (case targs of
           { continue = SOME lab, ... } => Goto lab
         | _ => raise ToCIL "continue statement without target")

    | Ast.Switch (e, s) => raise ToCIL "unimplemented: switch"
    | Ast.CaseLabel (num, s) => raise ToCIL "unimplemented: case labels"
    | Ast.DefaultLabel s => raise ToCIL "unimplemented: default"

    | Ast.StatExt _ => raise ToCIL "statement extensions unsupported"

  fun tocil decls =
    let
      val bc : stmt bc = BC.empty ()

      val globals = ref nil
      val functions = ref nil
      fun onedecl (Ast.DECL (Ast.ExternalDecl decl, _, _)) =
        (case decl of
           Ast.TypeDecl { shadow = _, tid = _ } => ()
         | Ast.VarDecl (id as { ctype, ... }, init) =>
             let
               (* XXX "static" probably needs to be treated separately if
                  we have multiple translations units. But maybe ckit
                  already gives the identifiers different uids? *)
               val uid = uidstring id
               val t = transtype ctype
               val stmt = case init of
                 NONE => End
               | SOME ie =>
                   (case ie of
                      Ast.Simple e =>
                        transexp e bc
                        (fn (v : value, vt : typ) =>
                         implicit { v = v, src = vt, dst = t }
                         (fn (vv, vvt) =>
                          Store (ctypewidth ctype,
                                 AddressLiteral (Global ` uid, t),
                                 vv, End)))
                    | Ast.Aggregate _ =>
                        raise ToCIL "aggregate initialization unimplemented")
               val startlabel = BC.genlabel "init"
             in
               BC.insert (bc, startlabel, stmt);
               globals :=
               (uid, Glob { typ = t,
                            (* PERF: Some initializers can become bytes
                               directly, saving us some work later? *)
                            bytes = NONE,
                            init =
                            SOME { start = startlabel,
                                   blocks = BC.extract bc} }) :: !globals
             end)
        | onedecl (Ast.DECL (Ast.FunctionDef (id, args, body), _, _)) =
           (case id of
              { ctype = Ast.Function (ret, _), ... } =>
                let
                  val uid = uidstring id
                  val ret = transtype ret
                  val startlabel = BC.genlabel "start"
                  fun onearg id = (idstring id, transtype (#ctype id))
                in
                  BC.insert (bc, startlabel,
                             transstatement body { break = NONE,
                                                   continue = NONE } bc
                             (* Should support nullary return? *)
                             (fn () => Return (unsigned_literal Width8 0)));
                  (* (string * ((string * typ) list * typ * stmt)) list, *)
                  functions :=
                    (uid, Func { args = map onearg args,
                                 ret = ret, body = startlabel,
                                 blocks = BC.extract bc }) :: !functions
                end
            | _ => raise ToCIL "Expected FunctionDef to have Function type.\n")
        | onedecl (Ast.DECL _) =
              raise ToCIL "External declaration not supported.\n"

      in
        app onedecl decls;
        Program { main = "main",
                  functions = rev (!functions),
                  globals = rev (!globals) }
      end

end

