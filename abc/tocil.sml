structure ToCIL :> TOCIL =
struct

  exception ToCIL of string
  open CIL

  structure BC :>
  sig
    type 'a blockcollector
    type label = string
    val empty : unit -> 'a blockcollector
    val genlabel : string -> label
    val label : string -> label
    val insert : 'a blockcollector * label * 'a -> unit

    (* XXX tolist or whatever *)
  end =
  struct
    (* PERF could use unique ids, hashtable, etc. *)
    type label = string
    structure SM = SplayMapFn(type ord_key = string
                              val compare = String.compare)
    type 'a blockcollector = 'a SM.map ref
    val label_ctr = ref 0
    fun genlabel s =
      let in
        label_ctr := !label_ctr + 1;
        "l$" ^ s ^ "$" ^ Int.toString (!label_ctr)
      end
    fun label s = s ^ "$"
    fun empty () = ref SM.empty
    fun insert (r : 'a blockcollector, l, v : 'a) =
      r := SM.insert(!r, l, v)
  end

  (* Some derived forms *)
  fun Goto label = GotoIf (WordLiteral 0w1, label, End)

  (* For local variables, disambiguate the symbol with the uid. *)
  fun idstring ({ name, uid, ... } : Ast.id) =
    Symbol.name name ^ "$" ^ Pid.toString uid
  (* For globals and functions, we just use the name. These must be
     globally unique and we could use them for linking (?). *)
  fun uidstring ({ name, ... } : Ast.id) = Symbol.name name

  fun labstring ({ name, ... } : Ast.label) = Symbol.name name

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

  fun transexp (Ast.EXPR (e, _, _) : Ast.expression) (bc : stmt BC.blockcollector) (k : value -> stmt) : stmt =
    case e of
      Ast.IntConst i => k (WordLiteral (word32_literal i))
    | Ast.RealConst r => raise ToCIL "unimplemented: floating point (literal)"
    | _ => raise ToCIL "unimplemented expression type (so many HERE)"

  (* Typedecls ignored currently. *)
  fun transdecl (Ast.TypeDecl _) (bc : stmt BC.blockcollector) (k : unit -> stmt) : stmt = k ()
    (* Should probably still add it to a context? *)
    | transdecl (Ast.VarDecl (id, NONE)) bc k = k ()
    | transdecl (Ast.VarDecl (id, SOME init)) bc k =
    (case init of
       Ast.Simple e => transexp e bc (fn v => Store (Global (uidstring id), v, k ()))
     | Ast.Aggregate _ => raise ToCIL "aggregate initialization unimplemented (decl)")

  (* XXX need to keep track of a 'break' label. *)
  fun transstatementlist nil (bc : stmt BC.blockcollector) (k : unit -> CIL.stmt) : CIL.stmt = k ()
    | transstatementlist (s :: t) bc k =
    transstatement s bc (fn () => transstatementlist t bc k)

  and transstatement (Ast.STMT (s, _, _) : Ast.statement) (bc : stmt BC.blockcollector) (k : unit -> CIL.stmt) : CIL.stmt =
    case s of
      Ast.Expr NONE => k ()

    | Ast.Expr (SOME e) => transexp e bc (fn v => k ())
    | Ast.ErrorStmt => raise ToCIL "encountered ErrorStmt"

    | Ast.Compound (decls, stmts) =>
        let
          fun dodecls nil = transstatementlist stmts bc k
            | dodecls (h :: t) = transdecl h bc (fn () => dodecls t)
        in
          dodecls decls
        end

    (* Some statements ignore k because any following code is unreachable. *)
    | Ast.Return NONE => Return (WordLiteral 0w0)
    | Ast.Return (SOME e) => transexp e bc (fn v => Return v)
    | Ast.IfThen (e, body) =>
        transexp e bc (fn cond =>
                       let
                         val true_label = BC.genlabel "if_t"
                         val rest_label = BC.genlabel "if_r"
                       in
                         BC.insert (bc, true_label,
                                    transstatement body bc
                                    (fn () => Goto rest_label));
                         BC.insert (bc, rest_label, k ());
                         GotoIf (cond, true_label,
                                 Goto rest_label)
                       end)
    | Ast.IfThenElse (e, true_body, false_body) =>
        transexp e bc (fn cond =>
                       let
                         val true_label = BC.genlabel "if_t"
                         val rest_label = BC.genlabel "if_r"
                       in
                         BC.insert (bc, true_label,
                                    transstatement true_body bc
                                    (fn () => Goto rest_label));
                         BC.insert (bc, rest_label, k ());
                         GotoIf (cond, true_label,
                                 transstatement false_body bc
                                 (fn () => Goto rest_label))
                       end)
    | Ast.Goto lab => Goto (labstring lab)


    | Ast.While (e, s) => raise ToCIL "unimplemented: while"
    | Ast.Do (e, s) => raise ToCIL "unimplemented: do"
    | Ast.For (init, cond, inc, body) =>
        let
          (*
          this doesn't work because we don't have aid/locs for the made-up
          expressions
          fun maketrivial NONE = Value (WordLiteral 0w0) | maketrivial (SOME e) = e
          val init = maketrivial init
          val cond = maketrivial cond
          val inc = maketrivial inc
          *)
        in
          raise ToCIL "unimplemented: for"
        end

    | Ast.Labeled (lab, s) =>
        let val l = labstring lab
        in BC.insert (bc, l, transstatement s bc k);
           Goto l
        end

    | Ast.CaseLabel (num, s) => raise ToCIL "unimplemented: case labels"
    | Ast.DefaultLabel s => raise ToCIL "unimplemented: default"

    | Ast.Break => raise ToCIL "unimplemented: break"
    | Ast.Continue => raise ToCIL "unimplemented: continue"
    | Ast.Switch (e, s) => raise ToCIL "unimplemented: switch"
    | Ast.StatExt _ => raise ToCIL "statement extensions unsupported"


  fun tocil decls =
    let
      val bc : stmt BC.blockcollector = BC.empty ()

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
                        transexp e bc (fn (v : value) =>
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
                  val stmt = transstatement body bc
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
        (* XXX need to return blocks as well. *)
        Program { functions = rev (!functions), globals = rev (!globals) }
      end

end
