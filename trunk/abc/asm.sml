(* XXX This is still in progress; almost nonsensical now...

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
   in printable X86.

   XXX Maybe local frames need to be in DS because how else
   do we represent pointers to them?
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
    (* Load (dst, addr) *)
  | Load8 of tmp * tmp
  | Load16 of tmp * tmp
    (* Store (addr, src) *)
  | Store8 of tmp * tmp
  | Store16 of tmp * tmp
  | Immediate8 of tmp * Word8.word
  | Immediate16 of tmp * Word16.word
  (* PERF allow tmp * literal? It is only more efficient
     if the literal is printable... *)
  | Add of tmp * tmp
  | Sub of tmp * tmp
  | Complement of tmp
  | Mov of tmp * tmp
  | Xor of tmp * tmp
  | Push of tmp
  | Pop of tmp
    (* Name of block. 16 bits. *)
  | LoadLabel of tmp * string
    (* General jump to 16-bit code pointer. Used for
       returning from functions and for C function
       pointers. *)
  | JumpInd of tmp
    (* Conditional jumps all take a literal label. *)
  | JumpCond of cond * string

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
    | Load8 (dst, addr) => "load8 " ^ tmptos dst ^ " <- [" ^ tmptos addr ^ "]"
    | Load16 (dst, addr) => "load16 " ^ tmptos dst ^ " <- [" ^ tmptos addr ^ "]"
    | Store8 (addr, src) => "store8 [" ^ tmptos addr ^ "] <- " ^ tmptos src
    | Store16 (addr, src) => "store16 [" ^ tmptos addr ^ "] <- " ^ tmptos src
    | Push tmp => "push " ^ tmptos tmp
    | Pop tmp => "push " ^ tmptos tmp
    | LoadLabel (tmp, lab) => "loadlabel " ^ tmptos tmp ^ " <- &" ^ lab
    | JumpInd tmp => "jmp_ind " ^ tmptos tmp
    | JumpCond (cond, lab) => condtos cond ^ " " ^ lab
    | _ => "unimplemented cmdtos cmd"
(*
  | Immediate8 of tmp * Word8.word
  | Immediate16 of tmp * Word16.word
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
