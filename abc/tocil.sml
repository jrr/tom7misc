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

  val POINTER_WIDTH = Width16
  val POINTER_INT_TYPE = Word16 Signed
  val INT_WIDTH = Width16
  val INT_TYPE = Word16 Signed

  (* Translation context. Not the same as CIL.context. *)
  datatype ctx = CTX of { tidtab: Bindings.tidBinding Tidtab.uidtab }

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
  fun memstring ({ name, ... } : Ast.member) = Symbol.name name
  (* For globals and functions, we just use the name. These must be
     globally unique and we could use them for linking (?). *)
  fun uidstring ({ name, ... } : Ast.id) = Symbol.name name
  fun newidstring s = s ^ "$$" ^ Pid.toString (Pid.new ())

  fun labstring ({ name, ... } : Ast.label) = Symbol.name name

  fun transtype (ctx as CTX { tidtab, ... } : ctx) (t : Ast.ctype) =
    case t of
      Ast.Error => raise ToCIL "Error cannot be translated"
    | Ast.Void => Struct []
    | Ast.Ellipses => raise ToCIL "... unsupported (as yet)"
    (* We ignore const (just affects error checking and optimization)
       and volatile (means nothing) *)
    | Ast.Qual (_, t) => transtype ctx t
    (* Array should be represented as pointer, right? *)
    | Ast.Array _ => raise ToCIL "Array types unimplemented."
    | Ast.Pointer t => Pointer (transtype ctx t)
    | Ast.Function (ret, args) =>
        Code (transtype ctx ret, map (transtype ctx o #1) args)
    | Ast.StructRef tid =>
        (case Tidtab.find (tidtab, tid) of
           NONE => raise ToCIL ("Bug? Reference to tid " ^ Tid.toString tid ^
                                " that was not in the tidtable?")
         | SOME ({ name, ntype, global, ... } : Bindings.tidBinding) =>
             (case ntype of
                NONE => raise ToCIL "Reference to anonymous struct? How?"
              | SOME (Bindings.Struct (_, fields : (Ast.ctype *
                                                    Ast.member option *
                                                    LargeInt.int option) list)) =>
                  Struct ` map (fn (ctype, SOME mem, NONE) =>
                                (memstring mem, transtype ctx ctype)
                                 | (_, _, SOME _) => raise ToCIL "Bit fields unsupported."
                                 | (_, NONE, _) => raise ToCIL "Unnamed members?") fields
              | SOME _ => raise ToCIL "Bug? Expected StructRef to name a struct."))
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
      Pointer _ => Width16
    | Code _ => Width16
    | Struct _ => raise ToCIL "unimplemented: struct lvalues"
    | Array _ => raise ToCIL "unimplemented: array lvalues"
    | Word32 _ => Width32
    | Word16 _ => Width16
    | Word8 _ => Width8

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


  fun get_builtin (Ast.EXPR (Ast.Id (id as { global = true,
                                             name = name,
                                             kind = Ast.FUNCTION _, ... }),
                             _, _)) =
    (case Symbol.name name of
       "_putc" => SOME B_PUTC
     | "_out8" => SOME B_OUT8
     | "_exit" => SOME B_EXIT
     | _ => NONE)
    | get_builtin _ = NONE

  (* Get the offset of a member field (and the type of that field)
     within a struct. We use a totally packed representation. *)
  fun get_member_offset (s : (string * typ) list) (mem : string) : int * typ =
    let
      fun gmo _ nil = raise ToCIL ("Member " ^ mem ^ " not found in struct!")
        | gmo b ((mm, t) :: rest) =
        if mm = mem then (b, t)
        else gmo (b + sizeof t) rest
    in
      gmo 0 s
    end

  (* Translate an expression as an lvalue. This means producing a value
     that is the lvalue's address. We also pass to the continuation the
     width of the lvalue, and the type of the lvalue (not the address;
     the contents of the address). *)
  fun translvalue (ctx : ctx)
                  (Ast.EXPR (e, _, _) : Ast.expression) (bc : stmt bc)
                  (k : value * typ -> stmt) : stmt =
   let
     (* continuation shared by Member and Arrow; needs a translated
        address value. *)
      fun k_transmemberlvalue (mem : string) =
        (fn (addr, typ) =>
           case typ of
             Struct s =>
               let
                 val (offset, memtyp) = get_member_offset s mem
                 val intv = genvar "addr"
                 val offv = genvar ("a_" ^ mem ^ "_off")
                 val ptrv = genvar "ptr"
               in
                 Bind (intv, POINTER_INT_TYPE,
                       (* Convert to int. *)
                       Cast { src = Pointer typ,
                              dst = POINTER_INT_TYPE,
                              v = addr },
                       Bind (offv, POINTER_INT_TYPE,
                             (* Add offset *)
                             Plus (POINTER_WIDTH, Var intv,
                                   Word16Literal ` Word16.fromInt offset),
                             (* Convert back to pointer. *)
                             Bind (ptrv, Pointer typ,
                                   Cast { src = POINTER_INT_TYPE,
                                          dst = Pointer memtyp,
                                          v = Var offv },
                                   k (Var ptrv, memtyp))))
               end
           | _ => raise ToCIL ("Member access on non-struct. Member: " ^
                               mem ^ " Type: " ^ typtos typ))
    in

      case e of
        Ast.Id (id as { global = false, kind = Ast.NONFUN, ctype, ... }) =>
          let val typ = transtype ctx ctype
          in k (AddressLiteral (Local ` idstring id, typ), typ)
          end
      | Ast.Id (id as { global = true, kind = Ast.NONFUN, ctype, ... }) =>
          let val typ = transtype ctx ctype
          in k (AddressLiteral (Global ` uidstring id, typ), typ)
          end

      | Ast.Id (id as { global = true, kind = Ast.FUNCTION _, ctype, ... }) =>
          raise ToCIL ("FUNCTION-kind id " ^ idstring id ^ " cannot be lvalue")

      | Ast.Sub (ptr, idx) =>
          transexp ctx ptr bc
          (fn (ptrv, ptrt) =>
           case ptrt of
             Pointer t =>
               let
                 val argwidth = typewidth t
                 val scaled_idx = genvar "sidx"
                 val addr = genvar "off"

                 (* Shifting here is better once we implement an optimization that
                    can turn it into repeated addition. But multiplication will
                    work. *)
                 fun scale Width32 v =
                   (* LeftShift (POINTER_WIDTH, v, Word8Literal 0w2) *)
                   Times (POINTER_WIDTH, v, Word16Literal ` Word16.fromInt 4)
                   | scale Width16 v =
                   (* LeftShift (POINTER_WIDTH, v, Word8Literal 0w1) *)
                   Times (POINTER_WIDTH, v, Word16Literal ` Word16.fromInt 2)
                   | scale Width8 v = Value v
               in
                 transexp ctx idx bc
                 (fn (idxv, idxt) =>
                  implicit { src = idxt, dst = POINTER_INT_TYPE, v = idxv }
                  (fn (idxv, _) =>
                   Bind (scaled_idx, POINTER_INT_TYPE,
                         scale argwidth idxv,
                         (* XXX note this treats pointer as int; we should
                            probably insert explicit Cast. *)
                         Bind (addr, ptrt,
                               Plus (POINTER_WIDTH, ptrv, Var scaled_idx),
                               k (Var addr, t)))))
               end
           | _ => raise ToCIL ("Attempt to subscript something of " ^
                               "non-array type: " ^ typtos ptrt))

      | Ast.Member (exp, mem) =>
          let val mem = memstring mem
          in
            translvalue ctx exp bc
            (k_transmemberlvalue mem)
          end

      | Ast.Arrow (exp, mem) =>
          let val mem = memstring mem
          in
            transexp ctx exp bc
            (fn (ptrv, ptrt) =>
             case ptrt of
               Pointer t => k_transmemberlvalue mem (ptrv, t)
             | _ => raise ToCIL "-> expression on non-pointer.")
          end

      | Ast.Deref ptr =>
      (* Be wary of the difference between transexp and translvalue in
         these recursive calls.

         Because we call transexp here, we get the value of the
         pointer expression (transexp calls lvalue to get the pointer's
         address, then loads from that); this is what we want to
         represent the lvalue. *)
          transexp ctx ptr bc
          (fn (ptrv, ptrt) =>
           case ptrt of
             Pointer t => k (ptrv, t)
           | _ => raise ToCIL ("Attempt to dereference non-pointer: " ^
                               typtos ptrt))
      | Ast.AddrOf _ => raise ToCIL "illegal lvalue AddrOf"
      | Ast.Binop _ => raise ToCIL "illegal lvalue Binop"
      | Ast.Unop _ => raise ToCIL "illegal lvalue Unop"
      | Ast.Cast _ => raise ToCIL "illegal lvalue Cast" (* XXX might be legal? *)
      | Ast.Id _ => raise ToCIL "illegal lvalue Id"
      | Ast.EnumId _ => raise ToCIL "illegal lvalue EnumId"
      | Ast.SizeOf _ => raise ToCIL "illegal lvalue SizeOf"
      | Ast.ExprExt _ => raise ToCIL "illegal lvalue ExprExt"
      | Ast.ErrorExpr => raise ToCIL "illegal lvalue ErrorExpr"
      | _ =>
          let in
            raise ToCIL "illegal/unimplemented lvalue"
          end
   end

  and transexplist (ctx : ctx) (es : Ast.expression list) (bc : stmt bc)
                   (k : (value * typ) list -> stmt) : stmt =
    let
      fun tel revl nil = k (rev revl)
        | tel revl (e :: rest) =
        transexp ctx e bc (fn (v, t) => tel ((v, t) :: revl) rest)
    in
      tel nil es
    end

  (* Normal translation of an expression (rvalue). *)
  and transexp (ctx : ctx)
               (orig_exp as Ast.EXPR (e, _, _) : Ast.expression) (bc : stmt bc)
               (k : value * typ -> stmt) : stmt =
    let
      fun as_lvalue s =
        translvalue ctx orig_exp bc
        (fn (addr, typ) =>
         let
           val width = typewidth typ
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

      (* Identifiers for functions are not lvalues. *)
      | Ast.Id (id as { global = true, kind = Ast.FUNCTION _, ctype, ... }) =>
        let val typ = transtype ctx ctype
        in
          case transtype ctx ctype of
            Code (ret, args) =>
              k (FunctionLiteral (uidstring id, ret, args), typ)
          | _ => raise ToCIL ("Alleged FUNCTION-kind id " ^ idstring id ^
                              " doesn't have function type??")
        end

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
               transexp ctx a bc
               (fn (av, at) =>
                implicit { src = at, dst = BOOL_TYPE, v = av }
                (fn (av, _) =>
                 let
                   val res = newidstring "andr"
                   val resv = genvar "r"
                   val nnresv = genvar "nnr"
                   val true_lab = BC.genlabel "andtrue"
                   val done_lab = BC.genlabel "anddone"
                 in
                   BC.insert
                   (bc, true_lab,
                    transexp ctx b bc
                    (fn (bv : value, bt : typ) =>
                     implicit { src = bt, dst = BOOL_TYPE, v = bv }
                     (fn (bv, _) =>
                      Bind (nnresv, BOOL_TYPE,
                            Yet (BOOL_WIDTH, bv),
                            Store (BOOL_WIDTH,
                                   AddressLiteral (Local res, BOOL_TYPE),
                                   Var nnresv,
                                   Goto done_lab)))));

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
               transexp ctx a bc
               (fn (av, at) =>
                implicit { src = at, dst = BOOL_TYPE, v = av }
                (fn (av, _) =>
                let
                  val res = newidstring "orr"
                  val resv = genvar "r"
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
                   transexp ctx b bc
                   (fn (bv, bt) =>
                    implicit { src = bt, dst = BOOL_TYPE, v = bv }
                    (fn (bv, _) =>
                     Bind (nnresv, BOOL_TYPE,
                           Yet (BOOL_WIDTH, bv),
                           Store (BOOL_WIDTH,
                                  AddressLiteral (Local res, BOOL_TYPE),
                                  Var nnresv,
                                  Goto done_lab)))))
                end))

           | ASSIGNING bop =>
               translvalue ctx a bc
               (fn (addr, ltyp) =>
                transexp ctx b bc
                (fn (bv, bt) =>
                 let
                   val width = typewidth ltyp
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
               translvalue ctx a bc
               (fn (addr, ltyp) =>
                transexp ctx b bc
                (fn (bv, bt) =>
                 let
                   val width = typewidth ltyp
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
               transexp ctx a bc
               (fn (av, at) =>
                transexp ctx b bc
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
               transexp ctx a bc
               (fn (av, at) =>
                transexp ctx b bc
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
                transexp ctx a bc k
            | Ast.Not => transexp ctx a bc
                (fn (av, at) =>
                 let val v = genvar "u"
                 in
                   implicit { src = at, dst = BOOL_TYPE, v = av }
                   (fn (av, _) =>
                    Bind (v, BOOL_TYPE,
                          Not (BOOL_WIDTH, av), k (Var v, BOOL_TYPE)))
                 end)
            | Ast.Negate => transexp ctx a bc
                (fn (av, at) =>
                 let val v = genvar "u"
                 in Bind (v, at,
                          Negate (typewidth at, av), k (Var v, at))
                 end)
            | Ast.BitNot => transexp ctx a bc
                (fn (av, at) =>
                 let
                   val v = genvar "u"
                 in Bind (v, at,
                          Complement (typewidth at, av), k (Var v, at))
                 end)
            | Ast.UnopExt _ => raise ToCIL "unop extensions unsupported"
            | Ast.PreInc =>
                translvalue ctx a bc
                (fn (addr, ltyp) =>
                 let
                   val width = typewidth ltyp
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
                translvalue ctx a bc
                (fn (addr, ltyp) =>
                 let
                   val width = typewidth ltyp
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
                translvalue ctx a bc
                (fn (addr, ltyp) =>
                 let
                   val width = typewidth ltyp
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
                translvalue ctx a bc
                (fn (addr, ltyp) =>
                 let
                   val width = typewidth ltyp
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

      | Ast.StringConst s =>
        let
          val bytes = Word8Vector.tabulate (size s,
                                            fn i =>
                                            Word8.fromInt ` ord `
                                            String.sub (s, i))
        in
          k (StringLiteral bytes, Pointer ` Word8 Unsigned)
        end
      | Ast.Call (f, args) =>
        let
          (* A call could either be a regular function call or a
             call-like invocation of a builtin like "_putc". In
             either case, we translate the arguments, coerce them to
             the expected types if necessary, and so on, so these
             share a bunch of code... *)
          fun implicitargs retv rett call argtypes args =
            let
              fun ia (racc, nil, nil) =
                Bind (retv, rett, call (rev racc),
                      k (Var retv, rett))
                | ia (racc, t :: trest, (v, vt) :: vrest) =
                implicit { src = vt, dst = t, v = v }
                (fn (v, vt) => ia (v :: racc, trest, vrest))
                | ia _ = raise ToCIL "argument number mismatch in Call"
            in
              ia (nil, argtypes, args)
            end
        in
          (case get_builtin f of
             SOME b =>
               transexplist ctx args bc
               (fn args =>
                let
                  val (rett, argt) = builtin_type b
                  val retv = genvar "builtin"
                in
                  implicitargs retv rett (fn a => Builtin (b, a)) argt args
                end)
           | NONE =>
              (* Normal function call. *)
              transexp ctx f bc
              (fn (fv, ft) =>
               case ft of
                 Code (rett, argt) =>
                   transexplist ctx args bc
                   (fn args =>
                    let val retv = genvar "call"
                    in implicitargs retv rett (fn a => Call (fv, a)) argt args
                    end)
               | _ => raise ToCIL ("tried to call value of non-function " ^
                                   "type " ^ typtos ft)))
        end

      | Ast.QuestionColon (cond, te, fe) =>
            transexp ctx cond bc
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
                   ignore (transexp ctx e bc
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
                          transexp ctx te bc
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
                        transexp ctx fe bc
                        (fn (fv, ft) =>
                         implicit { src = ft, dst = synthtyp, v = fv }
                         (fn (fv, _) =>
                          Store (width, AddressLiteral (Local res, synthtyp),
                                 fv,
                                 Goto done_lab)))))
             end)

      | Ast.Assign (dst, rhs) =>
            translvalue ctx dst bc
            (fn (dstaddr, dsttyp : typ) =>
             transexp ctx rhs bc
             (fn (rhsv, rhst) =>
              implicit { src = rhst, dst = dsttyp, v = rhsv }
              (fn (rhsv, rhst) =>
               let val dstwidth = typewidth dsttyp
               in Store (dstwidth, dstaddr, rhsv,
                         k (rhsv, rhst))
               end)))

      | Ast.Comma (a, b) => transexp ctx a bc (fn _ => transexp ctx b bc k)

      | Ast.Cast (ctype, e) =>
            transexp ctx e bc
            (fn (ev, et) =>
             let
               val t = transtype ctx ctype
               (* Anything that can be implicitly converted, so
                  floats should also be here? *)
               fun isnumeric (Word8 _) = true
                 | isnumeric (Word16 _) = true
                 | isnumeric (Word32 _) = true
                 | isnumeric _ = false

               (* Pointers and 16-bit words are freely interconvertible. *)
               fun convertible (Pointer _, Word16 _) = true
                 | convertible (Word16 _, Pointer _) = true
                 (* Same for any two pointer types. *)
                 | convertible (Pointer _, Pointer _) = true
                 (* TODO: code pointers *)
                 | convertible _ = false
             in
               if isnumeric t andalso isnumeric et
               then
                 implicit { src = et, dst = t, v = ev } k
               else
                 if convertible (et, t)
                 then
                   let val cvar = genvar "cast"
                   in Bind (cvar, t, Cast { src = et, dst = t, v = ev },
                            k (Var cvar, t))
                   end
                 else raise ToCIL ("Disallowed or unimplemented cast from " ^
                                   typtos et ^ " to " ^ typtos t)
             end)
      | Ast.EnumId (m, n) => k (unsigned_literal Width32 (LargeInt.toInt n),
                                Word32 Unsigned)

      | Ast.AddrOf e =>
        translvalue ctx e bc
        (fn (addr, typ) =>
         k (addr, Pointer typ))

      | Ast.SizeOf t =>
        let val result_typ = Word16 Unsigned
        in k (Word16Literal ` Word16.fromInt ` sizeof ` transtype ctx t, result_typ)
        end

      | Ast.ExprExt _ => raise ToCIL "expression extensions not supported"
      | Ast.ErrorExpr => raise ToCIL "encountered ErrorExpr"
    end

  (* Typedecls ignored currently. *)
  fun transdecl (ctx : ctx) (Ast.TypeDecl _) (bc : stmt bc) (k : unit -> stmt) : stmt = k ()
    (* Should probably still add it to a context? *)
    | transdecl ctx (Ast.VarDecl (id, NONE)) bc k = k ()
    | transdecl ctx (Ast.VarDecl (id as { ctype, global, ...}, SOME init)) bc k =
    (* XXX: If stClass = STATIC, then we should
       actually transform this into a global. (global will be false) *)
    (case init of
       Ast.Simple e =>
         let
           val mkaddr =
             if global
             then (fn id => Global `uidstring id)
             else (fn id => Local `idstring id)
           val vartyp = transtype ctx ctype
         in
           transexp ctx e bc
           (fn (v, t) =>
            implicit { src = t, dst = vartyp, v = v }
            (fn (v, _) =>
             Store (typewidth vartyp,
                    AddressLiteral (mkaddr id, vartyp),
                    v, k ())))
         end
     | Ast.Aggregate _ =>
         raise ToCIL "aggregate initialization unimplemented (decl)")

  (* When translating a statement, the 'break' and 'continue' targets
     the label to jump to when seeing those statements. *)
  fun transstatementlist ctx nil _ _ _ k : CIL.stmt = k ()
    | transstatementlist ctx
                         (s :: t)
                         rett
                         (targs : { break : string option,
                                    continue : string option })
                         (bc : stmt bc)
                         (k : unit -> stmt) =
     transstatement ctx s rett targs bc
     (fn () => transstatementlist ctx t rett targs bc k)

  and transstatement (ctx : ctx)
                     (Ast.STMT (s, orig_id, orig_loc) : Ast.statement)
                     (fnret : CIL.typ)
                     (targs : { break : string option,
                                continue : string option })
                     (bc : stmt bc)
                     (k : unit -> CIL.stmt) : CIL.stmt =
    case s of
      Ast.Expr NONE => k ()
    | Ast.Expr (SOME e) => transexp ctx e bc (fn v => k ())
    | Ast.ErrorStmt => raise ToCIL "encountered ErrorStmt"
    | Ast.Compound (decls, stmts) =>
        let
          fun dodecls nil = transstatementlist ctx stmts fnret targs bc k
            | dodecls (h :: t) = transdecl ctx h bc (fn () => dodecls t)
        in
          dodecls decls
        end

    (* Some statements ignore k because any following code is unreachable. *)
    | Ast.Return NONE =>
        (case fnret of
           Struct nil => Return NONE
         | _ => raise ToCIL ("return without value for function with " ^
                             "nontrivial return type " ^ typtos fnret))
    | Ast.Return (SOME e) =>
        transexp ctx e bc
        (fn (v, t) =>
         implicit { src = t, dst = fnret, v = v }
         (fn (v, _) =>
          Return (SOME v)))

    | Ast.IfThen (e, body) =>
        transexp ctx e bc
        (fn (cond, condt) =>
         let
           val true_label = BC.genlabel "if_t"
           val rest_label = BC.genlabel "if_r"
         in
           BC.insert (bc, true_label,
                      transstatement ctx body fnret targs bc
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
        transexp ctx e bc
        (fn (cond, condt) =>
         let
           val true_label = BC.genlabel "if_t"
           val rest_label = BC.genlabel "if_r"
         in
           BC.insert (bc, true_label,
                      transstatement ctx true_body fnret targs bc
                      (fn () => Goto rest_label));
           BC.insert (bc, rest_label, k ());
           (* XXX as above. *)
           GotoIf (CNeq (BOOL_WIDTH, cond, LiteralFalse), true_label,
                   transstatement ctx false_body fnret targs bc
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
                     transexp ctx cond bc
                     (fn (condv, condt) =>
                      (* XXX as above *)
                      (* condv == false, i.e., !condv *)
                      GotoIf (CEq (BOOL_WIDTH, condv, LiteralFalse),
                              done_label,
                              transstatement ctx body fnret body_targs bc
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
                     transexp ctx cond bc
                     (fn (condv, condt) =>
                      (* XXX as above *)
                      GotoIf (CNeq (BOOL_WIDTH, condv, LiteralFalse),
                              body_label,
                              Goto done_label)));
          BC.insert (bc, body_label,
                     transstatement ctx body fnret body_targs bc
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
                         transexp ctx e bc
                         (fn _ => Goto start_label));

          (* Test the while condition and either enter the
             loop or jump past it. *)
          BC.insert (bc, start_label,
                     case cond of
                       NONE => (transstatement ctx body fnret body_targs bc
                                (fn () => Goto inc_label))
                     | SOME c =>
                         transexp ctx c bc
                         (fn (condv, condt) =>
                          (* XXX as above *)
                          (* cond == false, i.e. !cond *)
                          GotoIf (CEq (BOOL_WIDTH, condv, LiteralFalse),
                                  done_label,
                                  transstatement ctx body fnret body_targs bc
                                  (fn () => Goto inc_label))));

          (* When we're done, it's just the current continuation. *)
          BC.insert (bc, done_label, k ());

          case init of
            NONE => Goto start_label
          | SOME e => transexp ctx e bc (fn _ => Goto start_label)
        end

    | Ast.Labeled (lab, s) =>
        let val l = labstring lab
        in BC.insert (bc, l, transstatement ctx s fnret targs bc k);
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

    | Ast.Switch (e, s) =>
      let
        val l = BC.genlabel "switchdone"
      in
        raise ToCIL "unimplemented: switch"
      end
    | Ast.CaseLabel (num, s) => raise ToCIL "unimplemented: case labels"
    | Ast.DefaultLabel s => raise ToCIL "unimplemented: default"

    | Ast.StatExt _ => raise ToCIL "statement extensions unsupported"

  fun tocil (tidtab, decls) =
    let
      val bc : stmt bc = BC.empty ()
      (* Maybe bc should go in context? *)
      val ctx = CTX { tidtab = tidtab }

      val globals = ref nil
      val functions = ref nil
      fun onedecl (Ast.DECL (Ast.ExternalDecl decl, _, _)) =
        (case decl of
           Ast.TypeDecl { shadow = _, tid = _ } => ()
         | Ast.VarDecl (id as { ctype, status, kind, ... }, init) =>
             (case (status, kind) of
                (_, Ast.NONFUN) =>
                  let
                    (* XXX "static" probably needs to be treated separately if
                       we have multiple translations units. But maybe ckit
                       already gives the identifiers different uids? *)
                    val uid = uidstring id
                    val t = transtype ctx ctype
                    val stmt = case init of
                      NONE => End
                    | SOME ie =>
                        (case ie of
                           Ast.Simple e =>
                             transexp ctx e bc
                             (fn (v : value, vt : typ) =>
                              implicit { v = v, src = vt, dst = t }
                              (fn (vv, vvt) =>
                               Store (typewidth ` transtype ctx ctype,
                                      AddressLiteral (Global ` uid, t),
                                      vv, End)))
                         | Ast.Aggregate _ =>
                             raise ToCIL ("aggregate initialization " ^
                                          "unimplemented"))
                    val startlabel = BC.genlabel "init"
                  in
                    BC.insert (bc, startlabel, stmt);
                    globals :=
                    (uid,
                     Glob { typ = t,
                            (* PERF: Some initializers can become bytes
                               directly, saving us some work later? *)
                            bytes = NONE,
                            init =
                            SOME { start = startlabel,
                                   blocks = BC.extract bc} }) :: !globals
                  end
              | _ =>
                  let in
                    (* Nothing to do function decls or extern declarations.
                       (XXX Though we should cleanly reject programs that
                       depend on a function / extern that is not provided,
                       since there is no possibility of linking in ABC.) *)
                    ()
                  end))
        | onedecl (Ast.DECL (Ast.FunctionDef (id, args, body), _, _)) =
           (case id of
              { ctype = Ast.Function (fnret, _), ... } =>
                let
                  val uid = uidstring id
                  val fnret = transtype ctx fnret
                  val startlabel = BC.genlabel "start"
                  fun onearg id = (idstring id, transtype ctx (#ctype id))
                in
                  BC.insert (bc, startlabel,
                             transstatement ctx body
                             fnret
                             { break = NONE,
                               continue = NONE } bc
                             (* XXX should maybe invent a value of the
                                correct size? *)
                             (fn () => Return NONE));
                  (* (string * ((string * typ) list * typ * stmt)) list, *)
                  functions :=
                    (uid, Func { args = map onearg args,
                                 ret = fnret, body = startlabel,
                                 blocks = BC.extract bc }) :: !functions
                end
            | _ => raise ToCIL "Expected FunctionDef to have Function type.\n")
        | onedecl (Ast.DECL _) =
              raise ToCIL "External declaration not supported.\n"

      val () = app onedecl decls

      (* We'll get internal type errors or mysterious failure if init
         tries to call this and it has the wrong type. *)
      val (main_ret, main_args) =
        case ListUtil.Alist.find op= (!functions) CIL.main_function of
          NONE => raise ToCIL "There is no function 'main'?"
        | SOME (Func { ret, args, ... }) =>
            case (ret, map #2 args) of
              (Word16 _,
               at as [Word16 _, Pointer (Pointer (Word8 _))]) => (ret, at)
             | _ =>
              raise ToCIL ("In ABC, main must have a declaration compatible " ^
                           "with:\n  int main(int argc, char **argv)")

      val init_fn = "__abc_init"
      val init_fn_label = CILUtil.BC.genlabel "__abc_init_start"
      val argc = CILUtil.genvar "argc"
      val argv = CILUtil.genvar "argv"
      val () = functions :=
        (init_fn,
         Func
         { args = nil,
           (* Doesn't return. *)
           ret = Word16 Unsigned,
           body = init_fn_label,
           blocks =
           [(init_fn_label,
             Bind
             (argc, Word16 Signed, Builtin (B_ARGC, nil),
              Bind
              (argv, Pointer (Pointer (Word8 Signed)),
               Builtin (B_ARGV, nil),
               Do (Call (FunctionLiteral (CIL.main_function,
                                          main_ret, main_args),
                         [Var argc, Var argv]),
                   Do (Builtin (B_EXIT, nil),
                       End)))))]
            }) :: !functions
    in
      Program { main = init_fn,
                functions = rev (!functions),
                globals = rev (!globals) }
    end

end

