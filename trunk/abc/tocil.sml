structure ToCIL :> TOCIL =
struct

  exception ToCIL of string
  open CIL

  (* For local variables, disambiguate the symbol with the uid. *)
  fun idstring ({ name, uid, ... } : Ast.id) =
    Symbol.name name ^ "$" ^ Pid.toString uid
  (* For globals and functions, we just use the name. These must be
     globally unique and we could use them for linking (?). *)
  fun uidstring ({ name, ... } : Ast.id) = Symbol.name name

  fun transtype (t : Ast.ctype) =
    case t of
      Ast.Error => raise ToCIL "Error cannot be translated"
    | Ast.Void => Struct []
    | Ast.Ellipses => raise ToCIL "... unsupported (as yet)"
    (* We ignore const (just affects error checking and optimization)
       and volatile (means nothing) *)
    | Ast.Qual (_, t) => transtype t
    | Ast.Array _ => raise ToCIL "Array types unimplemented."
    | Ast.Pointer t => Pointer (transtype t)
    | Ast.Function (ret, args) => raise ToCIL "Function types unimplemented."
    | Ast.StructRef tid => raise ToCIL "unimplemented: need to look up struct and inline it"
    | Ast.UnionRef tid => raise ToCIL "unions unimplemented"
    (* All enums represented as int32 *)
    | Ast.EnumRef _ => Word32
    | Ast.TypeRef tid => raise ToCIL "unimplemented: need to look up typedef and return it"
    | Ast.Numeric (_, _, signedness, intkind, _) =>
        (case intkind of
          Ast.CHAR => Word8
        | Ast.SHORT => Word16
        | Ast.INT => Word32
        | Ast.LONG => Word32
        (* Note that this could just be translated as Word32, but that would not
           be compatible with C99. *)
        | Ast.LONGLONG => raise ToCIL "unimplemented: long long"
        | _ => raise ToCIL "unimplemented: floating point (type)")

  (* XXX needs to check bounds, which depend on signedness? *)
  fun word32_literal i = Word32.fromLargeInt i

  fun transexp (Ast.EXPR (e, _, _) : Ast.expression) (k : value -> stmt) : CIL.stmt =
    case e of
      Ast.IntConst i => k (WordLiteral (word32_literal i))
    | Ast.RealConst r => raise ToCIL "unimplemented: floating point (literal)"
    | _ => raise ToCIL "unimplemented expression type (so many HERE)"

  (* Typedecls ignored currently. *)
  fun transdecl (Ast.TypeDecl _) (k : unit -> CIL.stmt) : CIL.stmt = k ()
    (* Should probably still add it to a context? *)
    | transdecl (Ast.VarDecl (id, NONE)) k = k ()
    | transdecl (Ast.VarDecl (id, SOME init)) k =
    (case init of
       Ast.Simple e => transexp e (fn v => Store (Global (uidstring id), v, k ()))
     | Ast.Aggregate _ => raise ToCIL "aggregate initialization unimplemented (decl)")


  (* XXX need to keep track of a 'break' label. *)
  fun transstatementlist nil (k : unit -> CIL.stmt) : CIL.stmt = k ()
    | transstatementlist (s :: t) k =
    transstatement s (fn () => transstatementlist t k)

  and transstatement (Ast.STMT (s, _, _) : Ast.statement) (k : unit -> CIL.stmt) : CIL.stmt =
    case s of
      Ast.Expr NONE => k ()
    | Ast.Expr (SOME e) => transexp e (fn v => k ())
    | Ast.ErrorStmt => raise ToCIL "encountered ErrorStmt"
    | Ast.Compound (decls, stmts) =>
        let
          fun dodecls nil = transstatementlist stmts k
            | dodecls (h :: t) = transdecl h (fn () => dodecls t)
        in
          dodecls decls
        end

    (* Some statements ignore k because any following code is unreachable. *)
    | Ast.Return NONE => Return (WordLiteral 0w0)
    | Ast.Return (SOME e) => transexp e (fn v => Return v)

    | _ => raise ToCIL "lots of unimplemented statements"
  (*
    | While of expression * statement
    | Do of expression * statement
    | For of expression option * expression option * expression option * statement
    | Labeled of label * statement
    | CaseLabel of LargeInt.int * statement
    | DefaultLabel of statement
    | Goto of label
    | Break
    | Continue
    | IfThen of expression * statement
    | IfThenElse of expression * statement * statement
    | Switch of expression * statement
    | StatExt of (expression, statement, binop, unop) AstExt.statementExt
*)

  fun tocil decls =
    let
      val globals = ref nil
      val functions = ref nil
      fun onedecl (Ast.DECL (Ast.ExternalDecl decl, _, _)) =
        (case decl of
           Ast.TypeDecl { shadow = _, tid = _ } => ()
         | Ast.VarDecl (id, init) =>
             let
               val uid = uidstring id
               val t = transtype (#ctype id)
               val stmt = case init of
                 NONE => End
               | SOME ie =>
                   (case ie of
                      Ast.Simple e =>
                        transexp e (fn (v : value) =>
                                    Store (Global uid, v, End))
                    | Ast.Aggregate _ => raise ToCIL "aggregate initialization unimplemented")
             in
               globals := (uid, (t, stmt)) :: !globals
             end)
        | onedecl (Ast.DECL (Ast.FunctionDef (id, args, body), _, _)) =
           (case id of
              { ctype = Ast.Function (ret, _), ... } =>
                let
                  val uid = uidstring id
                  val ret = transtype ret
                  fun onearg id = (idstring id, transtype (#ctype id))
                  val stmt = transstatement body
                    (* Should support nullary return? *)
                    (fn () => Return (WordLiteral 0w0))
                in
                  (* (string * ((string * typ) list * typ * stmt)) list, *)
                  functions := (uid, (map onearg args, ret, stmt)) :: !functions
                end
            | _ => raise ToCIL "Expected FunctionDef to have Function type.\n")
        | onedecl (Ast.DECL _) = raise ToCIL "External declaration not supported.\n"

    in
      app onedecl decls;
      Program { functions = rev (!functions), globals = rev (!globals) }
    end

end
