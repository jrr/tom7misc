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
      - We've compiled away call:
          * Push label of return block
          * Push arguments
          * (create space for local variables?)
          * Do absolute jump to function label
      - and return:
          * Destroy local space / arguments
          * Put return value in register (?)
          * Pop label of return block
          * Do absolute jump to return label
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

   Globals are just allocated into fixed positions in DS.
   These go at the beginning of DS, starting at offset 256 (before that
   is the PSP).

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
   that EBX points to the beginning of our current frame (to the first
   local); each argument and local is at a fixed offset from this. We
   don't change the frame for blocks/loops/etc. within a function; all
   locals are hoisted to the function level. A function may have a
   0-byte frame if it has no locals nor arguments; the return addresses
   are not stored in the frame stack.

   * Temporaries. *
   In a traditional C compiler, temporaries are basically either a local
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
   we put the temporary stack at 0x7E80. It grows upward towards 0xFFFF.
   We maintain that EBP points to the beginning of our current temporary
   frame; EBP's default segment is SS.

   * Return addresses. *
   (on machine stack)
   We don't need random access to them.
   The machine stack can also be used by ASM code, and is used in the
   implementation of various tactics (e.g., loading a 32-bit immediate).

   * Return value. *
   TODO

   X86 registers reserved for compiler use:
    - EBX points to beginning of local frame (in DS).
    - EBP points to beginning of temporary frame (in SS).
   ...

*)

structure ASM =
struct

  datatype sz = S8 | S16 | S32

  (* Temporary, which can be implemented with a register or
     DS:[EBX+disp8] (or even stack?), so we have a lot of them (in
     fact we start by assuming an arbitrary number). Though we can
     only use a certain subset of x86 (e.g. exactly one operand must
     be a EBX+disp8) in the output, translation to x86 takes care of
     this for us.

     Every temporary knows its size and this is part of its identity.

     Loads and stores commute with temporary accesses; these things
     represent intermediate expressions that are inaccessible from
     C semantics (undefined behavior can still mess them up, possibly).

     The lifetime of a temporary is explicit, or ...?
     *)
  type tmp = string * sz

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
  datatype cond =
    (* true if A < B, treated as unsigned *)
    Below of tmp * tmp
    (* true if A <= B, treated as unsigned *)
  | BelowEq of tmp * tmp
    (* true if A < B, treated as signed *)
  | Less of tmp * tmp
  | LessEq of tmp * tmp
  | Eq of tmp * tmp
  | NotEq of tmp * tmp
  (* These avoid us having to load literal zero, which is
     very common. JZ is actually the same instruction as JE
     (and JNZ is JNE), but without a CMP -- we just need the
     zero flag to reflect the value being tested. One way
     to ensure this is to XOR tmp <- 0x20 twice.
     (Alternately we can XOR once and then just reverse the
     sense of the jmp.) *)
  | EqZero of tmp
  | NeZero of tmp
  (* Unconditional jump *)
  | True

  (* Destination comes first.

     The size of the temporaries defines the operation being
     performed, but of course not all combinations are supported.
     TODO: Document what is allowed.
     *)
  datatype cmd =
  (* XXX I think Prepare/Destroy are actually the same as
     expand/shrink *)
  (* Advance the base pointer to make
     for this funtion. This is done by the caller, since the
     arguments need to then be set up in this frame. *)
    PrepareArgs of string
  (* Remove the frame. This could be done by the caller or
     callee, but we do it in the callee so that we have a
     good chance of merging the ShrinkFrame and DestroyArgs,
     which both just modify the base pointer. *)
  | DestroyArgs of string
  (* Expand or shrink the frame by moving the base pointer.
     A function expands in its header to make room for its
     local variables, and shrinks before returning. This has
     to be done by the function itself, because the type of
     a function pointer does not indicate how much local
     space it needs. *)
  | ExpandFrame of int
  | ShrinkFrame of int
  (* Assign the address of the current frame (EBX) plus the
     given offset to the 16-bit temporary. *)
  | FrameOffset of tmp * Word16.word
    (* Load (dst, addr). Loads and stores are always to the
       data segment. *)
  | Load8 of tmp * tmp
  | Load16 of tmp * tmp
    (* Store (addr, src) *)
  | Store8 of tmp * tmp
  | Store16 of tmp * tmp
  | Immediate8 of tmp * Word8.word
  | Immediate16 of tmp * Word16.word
  | Immediate32 of tmp * Word32.word
  (* PERF allow tmp * literal? It is only more efficient
     if the literal is printable... *)
  | Add of tmp * tmp
  | Sub of tmp * tmp
  | Complement of tmp
  | Mov of tmp * tmp
  | Xor of tmp * tmp
  | Push of tmp
  | Pop of tmp
    (* Name of code block. 16 bits. *)
  | LoadLabel of tmp * string
    (* General jump to 16-bit code pointer. Used for
       returning from functions and for C function
       pointers. *)
  | JumpInd of tmp
    (* Conditional jumps all take a literal label. *)
  | JumpCond of cond * string
    (* TODO: inc, etc. *)

  datatype block = Block of
    { name: string,
      cmds: cmd list }
  and proc = Proc of
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
      blocks: block list }

  (* XXX data... *)
  datatype program = Program of { procs: proc list,
                                  main: string }

  fun tmptos (v, s) = v ^
    (case s of
       S8 => "[8]"
     | S16 => ""
     | S32 => "[32]")

  fun condtos c =
    case c of
      Below (a, b) => "jb " ^ tmptos a ^ ", " ^ tmptos b
    | BelowEq (a, b) => "jbe " ^ tmptos a ^ ", " ^ tmptos b
    | Less (a, b) => "jl " ^ tmptos a ^ ", " ^ tmptos b
    | LessEq (a, b) => "jle " ^ tmptos a ^ ", " ^ tmptos b
    | Eq (a, b) => "je " ^ tmptos a ^ ", " ^ tmptos b
    | NotEq (a, b) => "jne " ^ tmptos a ^ ", " ^ tmptos b
    | EqZero a => "jz " ^ tmptos a
    | NeZero a => "jnz " ^ tmptos a
    | True => "jmp"

  fun cmdtos c =
    case c of
      ExpandFrame n => "expandframe " ^ Int.toString n
    | ShrinkFrame n => "shrinkframe " ^ Int.toString n
    | FrameOffset (dst, off) => "frameoffset " ^ tmptos dst ^ " <- frame+" ^
        Word16.toString off
    | Load8 (dst, addr) => "load8 " ^ tmptos dst ^ " <- [" ^ tmptos addr ^ "]"
    | Load16 (dst, addr) => "load16 " ^ tmptos dst ^ " <- [" ^ tmptos addr ^ "]"
    | Store8 (addr, src) => "store8 [" ^ tmptos addr ^ "] <- " ^ tmptos src
    | Store16 (addr, src) => "store16 [" ^ tmptos addr ^ "] <- " ^ tmptos src
    | Push tmp => "push " ^ tmptos tmp
    | Pop tmp => "pop " ^ tmptos tmp
    | LoadLabel (tmp, lab) => "loadlabel " ^ tmptos tmp ^ " <- &" ^ lab
    | JumpInd tmp => "jmp_ind " ^ tmptos tmp
    | JumpCond (cond, lab) => condtos cond ^ " " ^ lab
    | Immediate8 (tmp, w8) =>
        "imm8 " ^ tmptos tmp ^ " <- " ^ Word8.toString w8
    | Immediate16 (tmp, w16) =>
        "imm16 " ^ tmptos tmp ^ " <- " ^ Word16.toString w16
    | Immediate32 (tmp, w32) =>
        "imm32 " ^ tmptos tmp ^ " <- " ^ Word32.toString w32
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
  fun blocktos (Block { name, cmds }) =
    "  " ^ name ^ ":\n" ^
    String.concat (map (fn cmd => "    " ^ cmdtos cmd ^ "\n") cmds)

  fun proctos (Proc { name, argbytes, localbytes, offsets, blocks }) =
    let val offsets = ListUtil.sort (ListUtil.bysecond Int.compare) offsets
    in
      "PROC " ^ name ^ " (argbytes " ^ Int.toString argbytes ^
      " localbytes " ^ Int.toString localbytes ^ "):\n  {\n" ^
      String.concat (map (fn (s, i) => "    @" ^ Int.toString i ^
                          "\t" ^ s ^ "\n") offsets) ^ "  }\n" ^
      StringUtil.delimit "\n" (map blocktos blocks)
    end

  fun progtos (Program { procs, main }) =
    "PROGRAM (main = " ^ main ^ "):\n" ^
    StringUtil.delimit "\n" (map proctos procs) ^ "\n"

end
