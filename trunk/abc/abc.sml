
structure ABC =
struct

  exception ABC of string

  fun parse f = ParseToAst.fileToAst f
  fun show f = ParseToAst.fileToC f

  fun go_internal f =
    case ParseToAst.fileToAst f of
      { errorCount = 0,
        (* the abstract syntax representation of a translation unit *)
        ast : Ast.ast,
        (* table of type identifiers *)
        tidtab : Bindings.tidBinding Tidtab.uidtab,
        warningCount : int,
        (* annotations and symbol table info *)
        auxiliaryInfo :
        {
         (* type annotation table *)
         aidtab : Tables.aidtab,
         (* types associated with implicit argument conversions.
            See, e.g. "usual unary" and "usual binary" conversions
            in Harbison & Steele *)
         implicits : Tables.aidtab,
         (* symbol table generated during elaboration *)
         env : State.symtab } } =>
      let
        val cil = ToCIL.tocil ast
        val () = print (CIL.progtos cil ^ "\n")
        val asm = ToASM.toasm cil
      in
        print "TODO: Rest of compiler\n"
      end
       | { errorCount, ... } =>
      raise ABC ("Parsing/elaboration failed with " ^
                 Int.toString errorCount ^ " error(s)");

  fun go f =
    go_internal f
    handle e =>
      let
        val s =
          case e of
            ABC s => "ABC: " ^ s ^ "\n"
          | ToCIL.ToCIL s => "ToCIL: " ^ s ^ "\n"
          | ToASM.ToASM s => "ToASM: " ^ s ^ "\n"
          | Tactics.Tactics s => "Tactics: " ^ s ^ "\n"
          | Acc.Acc s => "Acc: " ^ s ^ "\n"
          | X86.X86 s => "X86: " ^ s ^ "\n"
          | e => "Uncaught exception: " ^ exnName e
      in
        print s;
        raise e
      end
end
