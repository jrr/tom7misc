
structure Compile :> COMPILE =
struct

(* XXX hard coded paths and predictable /tmp filenames. Bad! *)

    val showil = Params.flag false
        (SOME ("-showil", 
               "Show internal language AST")) "showil"

    val symfile = Params.param "/tmp/hemlock-symbols.txt"
                      (SOME ("-symfile",
                             "File to write symbol information to (debugging)"))
                      "symfile"

    val showcps = Params.flag false (SOME ("-showcps", 
                                          "Show CPS language AST at each step")) "showcps"

    val optcps = Params.flag true (SOME ("-O", 
                                          "Optimize in CPS phase")) "optcps"

    val showcps2 = Params.flag false
        (SOME ("-showcps2", 
               "Show final closure/alloc converted CPS")) "showcps2"

    val showrtl = Params.flag false (SOME ("-showrtl", 
                                           "Show RTL AST")) "showrtl"

    val only_elab = Params.flag false 
        (SOME ("-only-elab",
               "Only compile through elaboration")) "only-elab"

    val skip_assemble = Params.flag false 
        (SOME ("-skip-assemble",
               "Do all compilation steps but skip assembly.")) "skip-assemble"

        
    (* get paths from environment variables, or use defaults. 
       XXX it would be better if this were integrated with the param system*)

    (* XXX 0 length strings *)
    fun add_trailing_slash s = 
      if String.sub(s, size s - 1) = #"/" 
      then s else s ^ "/"

    structure E = Posix.ProcEnv
    val conductor_dir =
      add_trailing_slash 
      (getOpt(E.getenv "CONDUCTOR", "/afs/cs/usr/tom7/concert/conductor/"))
    val hemlock_src_dir =
      add_trailing_slash
      (getOpt(E.getenv "HEMLOCK", "/usr0/src/hemlock/"))


    exception Compile of string

    fun skipassemble () =
        skip_assemble := true

    (* could also turn off CPSOpt.debug *)
    fun quiet () =
        let 
            val debugopt = Params.flag true NONE "debugopt"
        in
            showrtl := false;
            showil := false;
            showcps := false;
            showcps2 := false;
            debugopt := false
        end

    val pids ="FIXME" (* (Word.toString
                (Posix.Process.pidToWord
                 (Posix.ProcEnv.getpid ()))) *)
        
    fun execredir file (prog, args) =
        let 
            (* avoid /tmp symlink races *)
            val _ = (Posix.FileSys.unlink file) handle _ => ()
            val fd = Posix.FileSys.createf 
                (file, Posix.FileSys.O_RDWR,
                 Posix.FileSys.O.flags
                 [Posix.FileSys.O.excl],
                 Posix.FileSys.S.irwxu)
        in
            Posix.IO.dup2 { old = fd, new = Posix.FileSys.stdout };
            (* clear close-on-exec flag, if set *)
            Posix.IO.setfd ( Posix.FileSys.stdout, 
                             Posix.IO.FD.flags nil );
            Posix.Process.exec (prog, prog :: args)         
        end

    fun system (prog, args) =
        let val file = "/tmp/hemtmp_" ^ pids
          (* debug *)
          val _ = print (prog ^ " " ^ (StringUtil.delimit " " args) ^ "\n")

        in
        (case Posix.Process.fork () of
             NONE => execredir file (prog, args)
           | SOME pid =>
             (case Posix.Process.waitpid (Posix.Process.W_CHILD(pid), nil) of
                  (_, Posix.Process.W_EXITED) => NONE
                | (_, Posix.Process.W_EXITSTATUS w8) => SOME w8
                | _ => 
                      let in
                          (* XXX print contents of 'file', which
                             contains its stdout. *)
                          raise Compile (prog ^ " exited strangely")
                      end))
        end

    fun systeml nil = NONE
      | systeml ((p,a)::t) =
        (case system (p, a) of
             NONE => systeml t
           | fail => fail)

    exception Done of string

    fun getcps file =
        let

            fun tokenize s = 
                Parsing.transform Tokenize.token (Pos.markstream s)

            fun parseexpression G s = 
                Parsing.transform (Parse.exp G) (tokenize s)

            val parsed = Stream.toList 
                (parseexpression Initfix.initial 
                   (StreamUtil.ftostream file))

        in
          case parsed of
           [e] =>
             let
                 val (il, t) = 
                     (Elaborate.elab Initial.initial (Initial.wrap e))
                     handle Pattern.Pattern s =>
                          raise Compile ("pattern: " ^ s)

                 val _ = 
                 if (!showil)
                 then 
                     let in
                         print "il expression:\n";
                         Layout.print (ILPrint.etol il, print);
                         print "\n\nil type:\n";
                         Layout.print (ILPrint.ttol t, print);
                         print "\n\n"
                     end
                 else ()

                 val _ = if (!only_elab) 
                         then raise Done "only-elab"
                         else ()

                 val _ = ToCPS.clear ()
                 val cps = ToCPS.translate il

                 val _ =
                 if (!showcps)
                 then
                     let in
                         print "CPS expression:\n";
                         CPSPrint.printe cps;
                         print "\n\n"
                     end
                 else ()

                 val cps = (if !optcps
                            then CPSOpt.optimize cps
                            else cps) handle CPSOpt.CPSOpt s => 
                                 raise Compile ("CPS Optimizer: " ^ s)

                 val _ =
                 if !showcps andalso !optcps
                 then 
                     let in
                         print "\nCPS after optimization:\n";
                         CPSPrint.printe cps;
                         print "\n\n"
                     end
                 else ()

                 val closed = Closure.convert cps;

                 val _ =
                 if (!showcps)
                 then
                     let in
                         print "CPS after closure conversion:\n";
                         CPSPrint.printe closed;
                         print "\n\n"
                     end
                 else ()

                 val alloced = CPSAlloc.convert closed;

                 val _ =
                 if (!showcps2) orelse (!showcps)
                 then
                     let in
                         print "CPS after alloc conversion:\n";
                         CPSPrint.printe alloced;
                         print "\n\n"
                     end
                 else ()
             in
                 alloced
             end
         | nil => raise Compile "Parse error: no expression"
         | _ => raise Compile "Parse error: program must be single expression"
        end

    fun interpret file =
        let
            val _ = SymbolDB.clear ()
            val alloced = getcps file
        in
            CPSExec.exec alloced;
            print "Done.\n"
        end handle Compile s => print ("Compilation failed: " ^ s ^ "\n")
         | CPSExec.Exec s => print ("Execution error: " ^ s ^ "\n")

    fun compile file out =
        let
            val _ = SymbolDB.clear ()
            val alloced = getcps file
                
            val rtl = ToRTL.convert alloced
                handle ToRTL.ToRTL s => raise Compile ("To RTL: " ^ s)
                    
            val _ =
                if (!showrtl)
                then 
                    let in
                        print "RTL program:\n";
                        RTLPrint.printp rtl;
                        print "\n\n"
                    end
                else ()
                    
            val filename = "/tmp/hemlock" ^ pids
                
            val {entry, ...} = rtl
                
        in
            (* we might only want to generate the functions we actually use. *)
            TALRun.build out entry (hemlock_src_dir ^ "runtime/main/rinit.pop");
            
            (ToTAL.write rtl filename
             handle ToTAL.ToTAL s => raise Compile ("To TAL: " ^ s)
                  | TAL.TAL s => raise Compile ("TAL: " ^ s));

            if !symfile <> ""
            then SymbolDB.tofile (!symfile)
            else ();
            
            (* XXX hard-coded paths! 
               If anyone sees this... really, I meant to clean it up!
               *)
            
            if !skip_assemble
            then print "Skipping assembly due to skip-assemble flag.\n"
            else (
                  print ("Assembling... (" ^ filename ^ ")\n");
                  case system (conductor_dir ^ "/talc-3.0/build/talc.exe",
                               [filename ^ ".tal", 
                                "-c", 
                                "-o", filename ^ ".o"]) of
                      NONE => ()
                    | SOME w8 => raise Compile ("Talc returned status " ^ Word8.toString w8);
                          
                  print "\nLinking with client runtime...\n";
                  
                  case system (conductor_dir ^ "/talc-3.0/build/popcorn.exe",
                               ["-c", 
                                "-I", conductor_dir ^ "conductor2/poploader/",
                                conductor_dir ^ "conductor2/poploader/pophooks.pop",
                                hemlock_src_dir ^ "runtime/main/main.pop",
                                hemlock_src_dir ^ "runtime/main/poprunt.pop",
                                hemlock_src_dir ^ "runtime/main/rinit.pop"]) of
                      NONE => ()
                    | SOME w8 => raise Compile 
                          ("Unable to compile runtime/main: status " ^ 
                           Word8.toString w8);

                  case system (conductor_dir ^ "/talc-3.0/build/talc.exe", 
                               [
                                "--verify-link",
                                "--verify-program",
                                "--register-trusted-syms",
                                "--register-syms", "prelude",
                                "--std-lib", "pop_runtime",
                                "--std-lib", "stdlib",
                                "--std-lib", "loader",
                                filename ^ ".to" ] @
                               (* why do I need all this crap? With just
                                  prelude and core and runtime, it complains
                                  about dlopen, which cascades to this... 
                                  (I can popcorn.exe just my .to and runtime.pop
                                  and get a much smaller exe, but then I don't
                                  know how to link in hemrunt)
                                  *)
                               map (fn s =>
                                    conductor_dir ^
                                    "talc-3.0/popcorn/lib/" ^ s)
                                   ["string.to",
                                    "char.to",
                                    "dlpop.to",
                                    "poploader.to",
                                    "filename.to",
                                    "hashtable.to",
                                    "list.to",
                                    "core.to"] @
                                   [
                                    conductor_dir ^ "talc-3.0/" ^
                                    "runtime/prelude.to",
                                    conductor_dir ^ "conductor2/" ^
                                    "poploader/pophooks.to",
                                    hemlock_src_dir ^ "runtime/main/rinit.to",
                                    hemlock_src_dir ^ "runtime/main/poprunt.to",
                                    hemlock_src_dir ^ "runtime/main/main.to",
                                    (* socket stuff *)
                                    "-T", "poplib.a",
                                    (* hemlock runtime library *)
                                    "-T", "hemrunt.a",
                                    "-o", out]) of
                      NONE => ()
                    | SOME w8 => 
                          raise Compile ("Popcorn returned status " ^ Word8.toString w8);

                  print "\nAssembling (2)...\n";
                        
                  case systeml
                      [("/bin/mv", [filename ^ "_e.tali",
                                    filename ^ "_e.tali_bye"]),
                       ("/bin/mv", [filename ^ "_x.tali",
                                    filename ^ "_e.tali"]),
                       (conductor_dir ^ "talc-3.0/build/talc.exe",
                        [filename ^ ".tal", 
                         "-c", 
                         "-o", filename ^ ".o"])] of
                      NONE => ()
                    | SOME w8 => raise Compile ("Talc returned status " ^ Word8.toString w8);
                          
                          
                  print "\nPreparing cord code...\n";
                  
                  (* would be better if we could ask tar to just rename the
                     files as it places them into the archive. Using specific
                     predictable filenames in tmp is pretty unsatisfactory for
                     a number of reasons; better fix this! *)
                  
                  case systeml
                      [("/bin/mv",
                        [filename ^ ".o",
                         "/tmp/main.o"]),
                       ("/bin/mv",
                        [filename ^ ".to",
                         "/tmp/main.to"]),
                       ("/bin/mv",
                        [filename ^ "_i.tali",
                         "/tmp/main_i.tali"]),
                       ("/bin/mv",
                        [filename ^ "_e.tali",
                         "/tmp/main_e.tali"])] of
                      NONE => ()
                    | SOME w8 => raise Compile ("Failed to move files to 'main' names: " ^
                                                Word8.toString w8);
                          
                  case system ("/bin/bash",
                               ["-c",
                                "cd /tmp ; tar -cz main.o main.to main_i.tali main_e.tali > "
                                ^ conductor_dir ^ "cordcode/" ^ out ^
                                ".tar.gz"]) of
                      NONE => ()
                    | SOME w8 => raise Compile ("tar/gzip failed: " ^ 
                                                Word8.toString w8);
                                      
                          print "Compilation succeeded!\n"

                          )

        end handle Compile s =>
            print ("\n\nCompilation failed:\n    " ^ s ^ "\n")
                 | Done s =>
            print ("\n\nStopped early due to " ^ s ^ " flag.\n")

end
