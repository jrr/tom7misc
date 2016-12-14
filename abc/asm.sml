(* XXX This is still in progress; almost nonsensical now...

   Very low-level language, but not quite X86 opcodes.
   Here we have:
      - labels, symbolic execution points that we can
        branch to. We don't have "JMP", so these aren't
        code addresses; they're numbered sequentially with
        small integers.
      - conditional jumps to computed destinations
      - "macro" instructions that can be implemented in
        printable x86, but that require require significant
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
      - Loads of immediate values are variable-length.
        So it doesn't simply work to leave placeholder
        values and then do a second pass to reify labels.
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

  (* Temporary, which can be implemented with a register or
     DS:[EBX+disp8] (or even stack?), so we have a lot of them. Though
     we can only use a certain subset of x86 (e.g. exactly one operand
     must be a EBX+disp8) in the output, translation to x86 takes
     care of this for us.

     The lifetime of a temporary is explicit, or ...?
     *)
  type tmp = string

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

  (* Destination comes first. *)
  datatype cmd =
    (* Advance the base pointer to make room for the locals
       for the given procedure. *)
    CreateLocals of string
  | DestroyLocals of string
  | Load8T of tmp * Word8.word
  | Load16T of tmp * Word16.word
  (* PERF allow tmp * literal? It is only more efficient
     if the literal is printable... *)
  | Add16 of tmp * tmp
  | Sub16 of tmp * tmp
  | Complement16 of tmp
  | Mov16 of tmp * tmp
  | Xor16 of tmp * tmp
  | Push16 of tmp
  | Pop16 of tmp
    (* Name of block. 16 bits. *)
  | LoadLabel of tmp * string
    (* General jump to 16-bit code pointer. Used for
       returning from functions and for C function
       pointers. *)
  | JumpInd of tmp
    (* Unconditional jump to literal label. *)
  | Jump of string
    (* Conditional jumps all take a literal label. *)
  | JumpCond16 of cond * string

  datatype block = Block of
    { name: string,
      cmds: cmd list }
  and proc = Proc of
    { name: string,
      (* Size of stack frame for local variables,
         including the function arguments. *)
      localbytes: int,
      (* Map local variables (and arguments) to
         byte offsets from base. *)
      locals: (string * int) list,
      (* Every block belongs to a single procedure,
         and its references to local variables
         are relative to that. *)
      blocks: block list }

  (* XXX data... *)
  datatype program = Program of { procs: proc list,
                                  main: string }

end
