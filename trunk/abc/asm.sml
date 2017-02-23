(* XXX This is still in progress; almost nonsensical now; beware!
   TODO: Clean up wrong/misleading documentation!

   Very low-level language, but not quite X86 opcodes.
   Here we have:
      - DATA layout is explicit, but not CODE layout.
      - Globals and global initialization are no longer
        special.
      - labels, symbolic execution points that we can
        branch to. We don't have "JMP", so these aren't
        code addresses; they're numbered sequentially with
        small integers.
      - conditional jumps to computed destinations
      - "macro" instructions that can be implemented in
        printable x86, but that require significant
        expansion.

   But compared to CIL:
      - We've compiled away call and return (see below).
      - We don't have variables any more. We have temporaries,
        which can be modified.
      - multiplication, division, etc.?

   There are at least two issues that make this harder
   than a typical assembly task:
      - Loading an immediate value takes a variable-length
        number of opcodes. So it doesn't simply work to leave
        placeholder values and then do a second pass to reify
        labels.
      - In order to maintain the jump ladder, a "basic
        block" cannot be more than 126(?) bytes, because
        we need to be able to jump over it. It's not
        hard to split a block up, but we may not be able
        to preserve all state (e.g. flag values!) when
        doing so.

   The goal of the ASM language is to be semantically
   clean and robust (allowing for optimization),
   while being close to the actual instructions available
   in printable X86. *)

(*

   ** NEW! **
   ====== Layout strategy and calling convention. ======
   We need to represent:
     - locals, globals, arguments,
     - temporaries,
     - return address stack,
     - return address value.
   We have access to several segments using the override prefixes:
   ES, CS, SS, DS, FS, GS.
   However, we can't change the values of these (all MOV instructions
   are outside gamut, but no other instructions can even modify these
   so we can't do ADD/XOR tricks) and only a few of them have
   predictable values when we start the EXE.
     - SS is set up to a dedicated 64k segment. We need this for
       the machine stack (accessed with PUSH/POP), but we can also put
       temporaries here.
     - CS is set up to some 64k block in our EXE; we'll just use this
       for code; code size will be one of our major limitations.
     - DS is set up to 64k where the first 256 bytes are the Program
       Segment Prefix and the rest is from our EXE (not sure if this
       is guaranteed?). We assume all C (data) addresses are in DS.
     - ES, FS, GS are probably not reliable. (In DOSBox, ES=DS, and
       FS=GS=0.) Conceivably we could use FS and GS for additional
       temporaries, but if they overlap one of the other segments,
       we'd be toast. The interrupt-vector rewrite trick does
       currently rely on FS being 0 at startup.

   * Globals. *
   Anything that's a C variable (locals, globals, arguments) must be
   addressable with & in the general case, which means it must be laid
   out in the same segment. We use DS. (An earlier phase can rewrite
   locals that don't need to be addressed to CIL vars, which can be
   tmps here.)

   Globals are just allocated into fixed positions in DS. These go
   near the beginning of DS, starting at offset 256 (before that is
   the PSP).

   * Arguments and locals. *
   Arguments and locals are basically the same, except that arguments
   must be accessed by both the caller and callee. Because there can
   be an arbitrary (well, limited by segment size) number of functions
   currently active, these need to be allocated with a stack discipline.
   Because they need to be addressable, we don't put them in the machine
   stack (SS); rather, we keep a parallel stack in DS. We'll call it
   the "frame stack."

   We start the frame stack in DS, right after the last global. Unlike
   the machine stack, it grows upward (larger addresses). We maintain
   that EBX points to the beginning of our current frame (to the
   return value, if any, followed by arguments and locals); each
   argument and local is at a fixed known offset from this. We don't
   change the frame for blocks/loops/etc. within a function; all
   locals are hoisted to the function level. A function may have a
   0-byte frame if it has a void return type[1], no locals, and no
   arguments; the return addresses are not stored in the frame stack.

   [1] if compiled as 0-bytes, which it may not be.

   * Temporaries. *
   In a traditional C compiler, temporaries are basically either local
   variables or registers. It works differently in ABC because:
    - Due to segmented memory, we want to store as little as possible in
      DS (basically, just addressable things).
    - Since we can only use a handful addressing modes in Printable X86,
      and they are annoying, we don't actually want intermediate values
      in registers most of the time.
   Temporaries also live in a stack, because each function activation
   needs an arbitrary number of temporaries to do its computations, and
   these might need to live across a child function call. (It would be
   possible to distinguish caller and callee-save, but we don't bother
   with this now.) This stack is stored in the SS segment (same as the
   machine stack). The machine stack grows downward. In a normal compiler,
   the stack would start at 0xFFFF, but the start address must be printable
   (part of the EXE header), so it actually starts at 0x7E7E. Therefore,
   we start the temporary stack at 0x7E80. It grows upward towards 0xFFFF.
   We maintain that EBP points to the beginning of our current temporary
   frame; EBP's default segment is SS. [Note: For efficiency, we may
   actually want EBP to be 0x20 bytes shy of the actual frame at all
   times, so that we can use the EBP+disp8 addressing mode.]

   * Return addresses. *
   (on machine stack)
   We don't need random access to them.
   The machine stack can also be used by ASM code, and is used in the
   implementation of various tactics (e.g., loading a 32-bit immediate).
   DOS will also use the stack if it interrupts us. Of course, all of
   this should preserve the stack such that when we want to return,
   the return address is at the top of the stack.

   * Return value. *
   The return value is treated sort of like an implicit first argument;
   it appears right before the arguments, and the called function writes
   any return value to that slot. Upon return, the callee can copy
   the return value into a temporary before restoring its locals.

   This doesn't account for returns of structs (though the same technique
   would apply, as long as we had temporaries of arbitrary size?) -- we
   may need to compile that away earlier, or just not support it.

   * Calling convention. *
   When a function receives control, EBP is pointing to unused space
   that it can use for saving its temporaries however it likes (but
   space beneath EBP should be preserved).
   The caller saves temporaries with SaveTemps and restores them on
   return with RestoreTemps.

   The same is true for EBX, which points to the locals frame. The
   caller advances this past its own frame, then writes any arguments
   into the argument slots, and then jumps.

   The return address is at the top of the machine stack upon a call.


   X86 registers reserved for compiler use:
    - EBX points to beginning of local frame (in DS).
    - EBP points to beginning of temporary frame (in SS).
    - ESP is the top of the machine stack (in SS)
   ...

*)

structure ASM =
struct

  datatype sz = S8 | S16 | S32

  (* Temporary, which will be implemented with a slot in our temporary
     frame (i.e. SS:[EBP+disp8]) and perhaps in the future, registers
     or stack slots. We start by assuming an arbitrary number of them.
     Though we can only use a certain subset of x86 (e.g. exactly one
     operand must be a EBX+disp8) in the output, translation to x86
     takes care of this for us. So something like Add tmp1 <- tmp2
     will often become multiple x86 instructions.

     Every temporary knows its size. If a function has a same-named
     temporary at two different sizes, this is an error.

     Loads and stores commute with temporary accesses; these things
     represent intermediate expressions that are inaccessible from
     C semantics (undefined behavior can still mess them up, possibly).

     The ASM language is used with two different representations for
     temporaries, so its datatypes are parameterized by the type
     of temporaries, 'tmp. Initially we have named_tmps and then
     allocate them, giving explicit_tmps. *)

  (* named_tmps are the output of ToAsm. These are related to CIL vars
     plus anything else we needed to invent to linearize into ASM.
     We aren't concerned with lifetime or layout yet.

     A named temporary also knows what context it lives in. The
     context is the function that it's part of; this name is used to
     coalesce temporaries in AllocateTmps (only tmps in the same
     context can have overlapping lifetimes) and to associate the
     temporaries with the named context in SaveTempsNamed and
     RestoreTempsNamed, so that we compute a frame of the correct
     size. *)
  datatype named_tmp = N of { func : string,
                              name : string,
                              size : sz }

  (* After AllocateTmps, temporaries are just offsets from EBP,
     along with their size.

     XXX decide on type; document.
     - should maybe have some name hint, just for sanity?
     - should have the possibility of using a register? *)
  type explicit_tmp = int * sz

  (* X86 offers both "A < B" and "B > A", which have equivalent
     meaning, but are encoded differently (CMP (A, B) vs CMP (B, A))
     and leave flags in different states. Here we normalize the
     operators to always "face less", because this simplifies the
     language and because we don't actually have access to JG (0x7F).

     In "ASM," the comparison is always paired with jump, because this
     means that we can semantically ignore flags, and can avoid
     splitting blocks between a comparison and jump. I think this
     makes the JO, JS, JP (overflow, sign, parity) family weird,
     so they are omitted here. *)
  datatype 'tmp cond =
    (* true if A < B, treated as unsigned *)
    Below of 'tmp * 'tmp
    (* true if A <= B, treated as unsigned *)
  | BelowEq of 'tmp * 'tmp
    (* true if A < B, treated as signed *)
  | Less of 'tmp * 'tmp
  | LessEq of 'tmp * 'tmp
  | Eq of 'tmp * 'tmp
  | NotEq of 'tmp * 'tmp
  (* These avoid us having to load literal zero, which is
     very common. JZ is actually the same instruction as JE
     (and JNZ is JNE), but without a CMP -- we just need the
     zero flag to reflect the value being tested. One way
     to ensure this is to XOR tmp <- 0x20 twice.
     (Alternately we can XOR once and then just reverse the
     sense of the jmp.) *)
  | EqZero of 'tmp
  | NeZero of 'tmp
  (* Unconditional jump *)
  | True

  (* Destination comes first.

     The size of the temporaries defines the operation being
     performed, but of course not all combinations are supported.
     TODO: Document what is allowed.
     *)
  datatype 'tmp cmd =
  (* Advance EBP (pointer to the beginning of the temporary stack)
     beyond this function's temporary frame. This is done before
     making a call so that the called function has room for its
     temporaries, and the current values of the temporaries are
     restored.

     There are two versions: The first is for when temps are named
     (then the string is the function name); the second is for
     explicit temps (then it is a number of bytes). This could be
     enforced with another type parameter, but it's probably simpler
     to just keep it as an invariant. *)
    SaveTempsNamed of string
  | SaveTempsExplicit of int
  (* The counterpart to SaveTemps; reduces EBP so that it points
     to the beginning of this function's temporary frame again. *)
  | RestoreTempsNamed of string
  | RestoreTempsExplicit of int
  (* Expand or shrink the (locals) frame by moving the base pointer.
     Each function knows how much space it needs for its locals,
     and expands the frame before making a function call, in order
     to save the values of its locals and make room for the called
     function's. *)
  | ExpandFrame of int
  | ShrinkFrame of int
  (* Assign the address of the current frame (EBX) plus the
     given offset to the 16-bit temporary. *)
  | FrameOffset of 'tmp * Word16.word
  (* In-line label, used in a bit of a hack for the translation
     of function calls. Should be rewritten to a block label. *)
  | Label of string
  (* Load (dst, addr). Loads and stores are always to the
     data segment. *)
  | Load8 of 'tmp * 'tmp
  | Load16 of 'tmp * 'tmp
  (* Store (addr, src) *)
  | Store8 of 'tmp * 'tmp
  | Store16 of 'tmp * 'tmp
  | Immediate8 of 'tmp * Word8.word
  | Immediate16 of 'tmp * Word16.word
  | Immediate32 of 'tmp * Word32.word
  (* PERF allow tmp * literal? It is only more efficient
     if the literal is printable... *)
  | Add of 'tmp * 'tmp
  | Sub of 'tmp * 'tmp
  | Complement of 'tmp
  | Mov of 'tmp * 'tmp
  | Xor of 'tmp * 'tmp
  | Push of 'tmp
  | Pop of 'tmp
  (* Name of code block. 16 bits. *)
  | LoadLabel of 'tmp * string
  (* General jump to 16-bit code pointer, which is popped from
     the stack. Used for returning from functions and for C function
     pointers. *)
  | PopJumpInd
  (* Conditional jumps all take a literal label. *)
  | JumpCond of 'tmp cond * string
  (* Program initialization code. *)
  | Init
  (* TODO: inc, etc. *)

  type 'tmp block = string * 'tmp cmd list
(*
  and 'tmp proc = Proc of
    { name: string,
      (* Size of stack frame for arguments. *)
      argbytes: int,
      (* Size of stack frame for local variables,
         excluding the function arguments. *)
      localbytes: int,
      (* Map local variables (and arguments) to
         byte offsets from base (beginning of arguments),
         once both the arguments and local frame have been
         set up. *)
      offsets: (string * int) list,
      (* Every block belongs to a single procedure, and its
         references to local variables are relative to that.

         The order of this list is significant; execution
         begins at the first block and falls through to the
         next (unless of course there's an unconditional
         jump). Exection must not "fall off the end." *)
      blocks: 'tmp block list }
    *)

  (* XXX data... *)
  datatype 'tmp program =
    Program of
    { (* The order of this list is significant; execution begins at the
         first block and falls through to the next (unless following a
         jump). Exection must not "fall off the end." *)
      blocks: 'tmp block list,
      (* Always 65536 bytes; printable. *)
      datasegment: Word8Vector.vector }

  fun condtos ts c =
    case c of
      Below (a, b) => "jb " ^ ts a ^ ", " ^ ts b
    | BelowEq (a, b) => "jbe " ^ ts a ^ ", " ^ ts b
    | Less (a, b) => "jl " ^ ts a ^ ", " ^ ts b
    | LessEq (a, b) => "jle " ^ ts a ^ ", " ^ ts b
    | Eq (a, b) => "je " ^ ts a ^ ", " ^ ts b
    | NotEq (a, b) => "jne " ^ ts a ^ ", " ^ ts b
    | EqZero a => "jz " ^ ts a
    | NeZero a => "jnz " ^ ts a
    | True => "jmp"

  fun cmdtos ts c =
    case c of
      SaveTempsNamed f => "savetemps " ^ f
    | SaveTempsExplicit n => "savetemps " ^ Int.toString n
    | RestoreTempsNamed f => "restoretemps " ^ f
    | RestoreTempsExplicit n => "restoretemps " ^ Int.toString n
    | ExpandFrame n => "expandframe " ^ Int.toString n
    | ShrinkFrame n => "shrinkframe " ^ Int.toString n
    | FrameOffset (dst, off) => "frameoffset " ^ ts dst ^ " <- frame+" ^
        Word16.toString off
    | Load8 (dst, addr) => "load8 " ^ ts dst ^ " <- [" ^ ts addr ^ "]"
    | Load16 (dst, addr) => "load16 " ^ ts dst ^ " <- [" ^ ts addr ^ "]"
    | Store8 (addr, src) => "store8 [" ^ ts addr ^ "] <- " ^ ts src
    | Store16 (addr, src) => "store16 [" ^ ts addr ^ "] <- " ^ ts src
    | Push tmp => "push " ^ ts tmp
    | Pop tmp => "pop " ^ ts tmp
    | LoadLabel (tmp, lab) => "loadlabel " ^ ts tmp ^ " <- &" ^ lab
    | PopJumpInd => "pop_jmp_ind"
    | JumpCond (cond, lab) => condtos ts cond ^ " " ^ lab
    | Immediate8 (tmp, w8) => "imm8 " ^ ts tmp ^ " <- 0x" ^ Word8.toString w8
    | Immediate16 (tmp, w16) => "imm16 " ^ ts tmp ^ " <- 0x" ^ Word16.toString w16
    | Immediate32 (tmp, w32) => "imm32 " ^ ts tmp ^ " <- 0x" ^ Word32.toString w32
    | Init => "init"
    | Label lab => "(LABEL " ^ lab ^ ")"
    | _ => "unimplemented cmdtos cmd"
(*
  (* PERF allow tmp * literal? It is only more efficient
     if the literal is printable... *)
  | Add of tmp * tmp
  | Sub of tmp * tmp
  | Complement of tmp
  | Mov of tmp * tmp
  | Xor of tmp * tmp
*)
  fun blocktos ts (name, cmds) =
    "  " ^ name ^ ":\n" ^
    String.concat (map (fn cmd => "    " ^ cmdtos ts cmd ^ "\n") cmds)

    (*
  fun proctos ts (Proc { name, argbytes, localbytes, offsets, blocks }) =
    let val offsets = ListUtil.sort (ListUtil.bysecond Int.compare) offsets
    in
      "PROC " ^ name ^ " (argbytes " ^ Int.toString argbytes ^
      " localbytes " ^ Int.toString localbytes ^ "):\n  {\n" ^
      String.concat (map (fn (s, i) => "    @" ^ Int.toString i ^
                          "\t" ^ s ^ "\n") offsets) ^ "  }\n" ^
      StringUtil.delimit "\n" (map (blocktos ts) blocks)
    end
    *)

  fun progtos ts (Program { blocks, datasegment }) =
    (* XXX print data segment? *)
    "DATA (.. 64kb ..)\n" ^
    "PROGRAM:\n" ^
    String.concat (map (blocktos ts) blocks) ^ "\n"

  fun named_tmptos (N { func, name, size }) =
    func ^ "." ^ name ^
    (case size of
       S8 => "[8]"
     | S16 => ""
     | S32 => "[32]")

  fun explicit_tmptos (i, s) = "@" ^ Int.toString i ^
    (case s of
       S8 => "[8]"
     | S16 => ""
     | S32 => "[32]")

  val named_program_tostring = progtos named_tmptos
  val explicit_program_tostring = progtos explicit_tmptos

end
