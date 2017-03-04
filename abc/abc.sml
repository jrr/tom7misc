structure ABC =
struct

  exception ABC of string

  fun parse f = ParseToAst.fileToAst f
  fun show f = ParseToAst.fileToC f

  fun splitext s =
    StringUtil.rfield (StringUtil.ischar #".") s

  fun go_internal { input = f, output } =
    case ParseToAst.fileToAst f of
      { errorCount = 0,
        (* the abstract syntax representation of a translation unit *)
        ast : Ast.ast,
        (* table of type identifiers *)
        tidtab : Bindings.tidBinding Tidtab.uidtab,
        warningCount : int,
        (* annotations and symbol table info *)
        auxiliaryInfo :
          { (* type annotation table *)
            aidtab : Tables.aidtab,
            (* types associated with implicit argument conversions.
               See, e.g. "usual unary" and "usual binary" conversions
               in Harbison & Steele *)
            implicits : Tables.aidtab,
            (* symbol table generated during elaboration *)
            env : State.symtab } } =>
      let
        val (basename, ext) = splitext output
        val () = if ext <> "exe"
                 then raise ABC "Expected output file to end with '.exe'"
                 else ()

        (* XXX flags to control printing of intermediate results *)
        val () = PPLib.ppToStrm (PPAst.ppAst () tidtab) TextIO.stdOut ast
        val () = print "\n\n"
        val cil = ToCIL.tocil ast
        val () = print ("\nToCIL:\n" ^ CIL.progtos cil ^ "\n")
        val cil = OptimizeCIL.optimize cil
        val cilstring = CIL.progtos cil
        val () = print ("\nOptimized:\n" ^ cilstring ^ "\n")
        val () = StringUtil.writefile (basename ^ ".cil") cilstring
        val asm = ToASM.toasm cil
        val asmstring = ASM.named_program_tostring asm
        val () = print ("\nToASM:\n" ^ asmstring ^ "\n")
        val () = StringUtil.writefile (basename ^ ".asm") asmstring
        val asm = AllocateTmps.allocate asm
        val asm = OptimizeAsm.optimize asm
        val esmstring = ASM.explicit_program_tostring asm
        val () = print ("\nAllocated & optimized:\n" ^ esmstring ^ "\n")
        val () = StringUtil.writefile (basename ^ ".esm") esmstring
        val x86 = ToX86.tox86 asm
        val { cs, ds, init_ip, codebytes } = x86
        (* For benchmarking size, etc. *)
        val () = StringUtil.writefile (basename ^ ".bytes")
          (Int.toString codebytes ^ "\n")
      in
        EXE.write_exe { init_ip = init_ip,
                        init_sp = ToX86.INIT_SP,
                        cs = cs,
                        ds = ds } output
      end
       | { errorCount, ... } =>
      raise ABC ("Parsing/elaboration failed with " ^
                 Int.toString errorCount ^ " error(s)");

  fun go args =
    go_internal args
    handle e =>
      let
        val s =
          case e of
            ABC s => "ABC: " ^ s ^ "\n"
          | Segment.Segment s => "Segment: " ^ s ^ "\n"
          | CIL.CIL s => "CIL: " ^ s ^ "\n"
          | ToCIL.ToCIL s => "ToCIL: " ^ s ^ "\n"
          | ToASM.ToASM s => "ToASM: " ^ s ^ "\n"
          | AllocateTmps.AllocateTmps s => "AllocateTmps: " ^ s ^ "\n"
          | OptimizeAsm.OptimizeAsm s => "OptimizeAsm: " ^ s ^ "\n"
          | ToX86.ToX86 s => "ToX86: " ^ s ^ "\n"
          | EncodeX86.EncodeX86 s => "EncodeX86: " ^ s ^ "\n"
          | Tactics.Tactics s => "Tactics: " ^ s ^ "\n"
          | Acc.Acc s => "Acc: " ^ s ^ "\n"
          | X86.X86 s => "X86: " ^ s ^ "\n"
          | EXE.EXE s => "EXE: " ^ s ^ "\n"
          | OptimizeCIL.OptimizeCIL s => "OptimizeCIL: " ^ s ^ "\n"
          | e => "Uncaught exception: " ^ exnName e
      in
        print s;
        OS.Process.exit OS.Process.failure
        (* raise e *)
      end
end
