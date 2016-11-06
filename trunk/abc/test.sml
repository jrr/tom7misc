
structure Test =
struct

  fun parse f = ParseToAst.fileToAst(f);
  fun show f = ParseToAst.fileToC(f);

  fun go f =
    let
      val {ast: Ast.ast,  (* the abstract syntax representation of a trans. unit *)
           tidtab: Bindings.tidBinding Tidtab.uidtab, (* table of type identifiers *)
           errorCount: int,  (* count of errors occuring during parsing and elaboration *)
           warningCount: int,(* count of warnings occuring during parsing and elaboration *)
           auxiliaryInfo:  (* annotations and symbol table info *)
           {aidtab: Tables.aidtab,  (* type annotation table *)
            (* types associated with implicit argument conversions.
               * See, e.g. "usual unary" and "usual binary" conversions
               * in Harbison & Steele *)
            implicits: Tables.aidtab,
            env: State.symtab}} (* symbol table generated during elaboration *)
        = ParseToAst.fileToAst f
    in

    end

end
