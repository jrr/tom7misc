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
        val () = if !Flags.verbose
                 then
                   let in
                     PPLib.ppToStrm (PPAst.ppAst () tidtab) TextIO.stdOut ast;
                     print "\n\n"
                   end
                 else ()
        val cil = ToCIL.tocil ast
        val () = if !Flags.verbose
                 then print ("\nToCIL:\n" ^ CIL.progtos cil ^ "\n")
                 else ()
        val cil = OptimizeCIL.optimize cil
        val cilstring = CIL.progtos cil
        val () = if !Flags.verbose
                 then print ("\nOptimized:\n" ^ cilstring ^ "\n")
                 else ()
        val () = StringUtil.writefile (basename ^ ".cil") cilstring
        val asm = ToASM.toasm cil
        val asmstring = ASM.named_program_tostring asm
        val () = if !Flags.verbose
                 then print ("\nToASM:\n" ^ asmstring ^ "\n")
                 else ()
        val () = StringUtil.writefile (basename ^ ".asm") asmstring
        val asm = AllocateTmps.allocate asm
        val asm = OptimizeAsm.optimize asm
        val esmstring = ASM.explicit_program_tostring asm
        val () = if !Flags.verbose
                 then print ("\nAllocated & optimized:\n" ^ esmstring ^ "\n")
                 else ()
        val () = StringUtil.writefile (basename ^ ".esm") esmstring
        val x86 = ToX86.tox86 asm
        val { cs, ds, init_ip, debug, codebytes } = x86
        (* For benchmarking size, etc. *)
        val () = StringUtil.writefile (basename ^ ".bytes")
          (Int.toString codebytes ^ "\n")

        fun oneblock (idx, (lab, acc)) =
          let
            val insoffs = Acc.insns_offsets acc
            fun w16 x = StringUtil.padex #"0" ~4 (Word16.toString (Word16.fromInt x))
          in
            w16 idx ^ "  " ^ lab ^ ":\n" ^
            String.concat
            (map (fn (off, ins) =>
                  w16 (idx + off) ^ "      " ^ X86.insstring ins ^ "\n") insoffs)
          end
        val () = StringUtil.writefile (basename ^ ".debug")
          (String.concat (map oneblock debug))
      in
        EXE.write_exe { init_ip = init_ip,
                        init_sp = ToX86.INIT_SP,
                        include_psp = true,
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
          | Liveness.Liveness s => "Liveness: " ^ s ^ "\n"
          | AllocateTmps.AllocateTmps s => "AllocateTmps: " ^ s ^ "\n"
          | OptimizeAsm.OptimizeAsm s => "OptimizeAsm: " ^ s ^ "\n"
          | ToX86.ToX86 s => "ToX86: " ^ s ^ "\n"
          | EncodeX86.EncodeX86 s => "EncodeX86: " ^ s ^ "\n"
          | Tactics.Tactics s => "Tactics: " ^ s ^ "\n"
          | Acc.Acc s => "Acc: " ^ s ^ "\n"
          | X86.X86 s => "X86: " ^ s ^ "\n"
          | EXE.EXE s => "EXE: " ^ s ^ "\n"
          | OptimizeCIL.OptimizeCIL s => "OptimizeCIL: " ^ s ^ "\n"
          | e => "Uncaught exception: " ^ exnName e ^ "\n"
      in
        print s;
        OS.Process.exit OS.Process.failure
        (* raise e *)
      end
end
