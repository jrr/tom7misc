(* And at long last, conversion to x86.

   If we had the full range of x86 opcodes, conversion from
   the ASM language to x86 would be pretty straightforward.
   There are a few big issues working against us:

    - The only jump construct we have is the family of relative short
      jumps, like JNZ, with a fixed offset. This offset is a single
      byte. Since it's printable, we can only jump from 0x20 (+32
      bytes) to 0x7e (+126 bytes). Note that there is no backward
      jump!

    - But of course our program needs to have backward jumps in order
      to perform loops. The only way I could even figure out to make
      the instruction pointer decrease is to overflow it.
      Unfortunately it doesn't simply work to let execution run past
      CS:FFFF, but it does work to perform a short forward jump (like
      the onese we have access to) that crosses CS:FFFF. These appear
      to be performed in on the 16-bit word, and thus wrap around to
      CS:0000.

    - All this really lets us do is get back to the beginning of the
      segment, with very little control other than that. In order to
      actually get us to an arbitrary label, we need to perform a
      series of jumps. Since the maximum displacement of any jump is
      126 bytes, we create a "ladder" of connected jumps throughout
      the code segment. Long jumps are performed by getting on this
      ladder and jumping until the desired destination is reached.
      Since the ladder "wraps around," it's possible to get anywhere.

    - The ladder is inefficient to travel along, and takes space, so
      we want it to be small. Each rung looks like this:

         rung_n:
           DEC si                     48
           JNZ rung_n+1               75 xx
         code-block:

      The register si holds the number of rungs left to traverse; when
      it reaches 0 then we enter the code block. This register can be
      16 bit, because of course there cannot be more than 64k rungs in
      the 64k code segment.

    - Note that the ladder above uses relative addressing, but all of the
      jumps in ASM are absolute! We could also do an absolute version:

          CMP_IMM AX, 0x3055          66 3d 55 30
          JNE next                    75 xx

      which isn't so bad, but is 6 bytes vs 3, requires us to use AX
      for the address (maybe fine).

      We can of course compute a relative address from an absolute one,
      because we know what address (block) we're currently at. So if
      we're currently in block src,

      JumpCond cc dst

      is

      rel = dst - src + 1 (mod the total number of blocks;
                           this can't be negative)
      si <- rel
      jmp next_rung

      (Note that a rung does DEC first, so if you want to execute that
       block, the register should be 1 when you enter the rung. This
       usually means just adding one to the register before starting.)

      and PopJumpInd is
      pop si
      sub si, src
      inc si
      jmp next_rung

      .. the belief is that this overhead (once per jump) is worth the
      increase in speed on the ladder (once per rung). Note that most
      jumps are through JumpCond, because only function pointers and
      return addresses need the complexity of PopJumpInd. (Though this
      optimization is not yet implemented.)

    - Using the ladder to jump like this does trash SI, (but can we
      make use of the fact that it is 0 upon normal entry to a block?)
      and also messes up flags. ASM doesn't need these to be preserved
      across jumps, so nor should our translation to x86.

    - Complication: Jumps can only go 126 bytes.

      A block could easily be longer than 126 bytes when assembled.
      This is not a big problem on its own; once we reach 126 bytes
      during assembly, we could insert the rung, and the too-big block
      could either jump over it or just hop onto the ladder and
      immediately off it.
         - Jumping over the rung is probably bad, because the minimum
           printable jump is 32 bytes.
         - Entering the ladder at least requires us to set the
           register correctly so that we immediately exit (probably
           means setting it to 1).
         - However, this means that if we're in the middle of
           something, we may have messed up our state. And we can't
           easily do something like push/pop, unless we can guarantee
           that nobody else will try to exit on this rung (might be
           true?).

      A reasonably simple way to deal with this is to just try
      translating a block, and if it's longer than the maximum, split
      it into two blocks (the first ending with a non-conditional jump
      to the second), and then try again. Our layout strategy will
      be based a greedy optimistic-try-then-split approach.

    - Complication: Even encoding literals takes an unpredictable
      amount of space, so splitting a block can in some cases make the
      block longer! We could bound this, but also, in the limit, we
      just need to ensure that no ASM instruction exceeds the maximum
      size for a block on its own (plus overhead). That should be easy
      to ensure.

    - Complication: Jumps must go at least 32 bytes. This means that
      we will need to pad some blocks so that the ladder can jump over
      them.

      Worse, a jump near the end of a block (and by their nature, most
      blocks have jumps near the end) must not be *too close* to the
      ladder that follows, because we must jump at leat 32 bytes. If
      we get too close, we probably should just continue *into* the
      ladder. At that point, only non-conditional jumps will work,
      unless we can manage to conditionally move a destination address
      into the reg or something fancy like that. It may occasionally
      be possible to conditionally jump to the *next* rung, if the
      next block is short enough.

    - Between the minimum and maximum sizes, there's some tradeoff,
      because we can fit more small blocks (and depending on how we
      exit them, we don't want to be executing padding...), but any
      backwards jump needs to go all the way to the end of the segment
      and so having more rungs to touch on the way is worse.

    - One additional constraint is that the program needs to start
      at some printable (actual) address, because that address needs
      to be specified in the header.

    - Note that we can pretty easily insert rungs that DON'T decrement
      SI, like they could just non-conditionally jump. We could use
      this to fill out the code segment and get to the end so that we
      can overflow. It is still usually better to stretch out the
      existing ladder, because fewer jumps to get around is faster.

    - Optimization: It could be possible to interleave some blocks, or
      have rungs that skip. This seems a bit more complexity than it
      works, but could improve code density.
    - Optimization: Some jumps will just be in range?
    - Optimization: Block layout order?
    - Optimization: Branching is pretty expensive. Computing a
      destination block with a jump table or something (for if..else)
      may be a better strategy in many cases.
*)

structure ToX86 :> TOX86 =
struct
  infixr 9 `
  fun a ` b = a b

  exception ToX86 of string

  structure A = ASM
  structure M = Machine

  structure IIM = ImperativeMapFn(type ord_key = int
                                  val compare = Int.compare)


  (* val INIT_SP = Word16.fromInt 0x7e7e *)
  val INIT_SP = Word16.fromInt 0x6e69

  val ASSEMBLY_CTX = X86.CTX { default_32 = false }
  val RUNG_SIZE =
    EncodeX86.encoded_size ASSEMBLY_CTX (X86.DEC X86.SI) +
    EncodeX86.encoded_size ASSEMBLY_CTX (X86.JNZ 0w0)

  (* This is the best printable technique I know to preserve all
     register values but perform an unconditional short jump. XOR
     always clears the overflow flag. *)
  (* PERF: Can do JO disp; JNO (disp-2), but that means two destinations to
     manage. *)
  val JMP_SIZE =
    EncodeX86.encoded_size ASSEMBLY_CTX (X86.XOR_A_IMM (X86.I8 0wx20)) +
    EncodeX86.encoded_size ASSEMBLY_CTX (X86.XOR_A_IMM (X86.I8 0wx20)) +
    EncodeX86.encoded_size ASSEMBLY_CTX (X86.JNO 0w0)

  (* Actual start position of temporary frame. Note that BP will always point
     32 bytes before the frame. *)
  val TMP_FRAME_START = Word16.fromInt 0x7e80

  open X86
  infix <- <~

  open Acc
  infix // ?? ++ --

  (* xblock is encoded x86 instructions, starting with its rung.
     It shares the name of the ASM block it came from.
     It also has the offsets (within the vector) of displacements
     that must be filled in to jump to the next block.

     Also, now, debug messages to be printed if desired. *)
  type xblock = (string * Word8Vector.vector * int list * Acc.acc * string list)

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  fun getlabelnum labelnum label =
    case SM.find (labelnum, label) of
      NONE => raise ToX86 ("bug: label " ^ label ^ " not in map?")
    | SOME i => i

  (* Do layout of a specific block, returning the x86 bytes and
     some metadata.

     A block's code is position-indepenent, and even moreso can be
     moved relative to the next rung as long as we can modify the
     jumps to the rung. All jumps are a JCC opcode (70-7E) followed
     by a printable displacement (20-7E), so we can modify the
     displacement after the fact without changing the layout. (Note
     however that changing the order or number of blocks does change
     layout in general, because we load absolute and relative block
     numbers, and the loading of these numbers is not constant size.

     So, when we produce code for a block, we record the positions
     where we want a "jump to the next rung" displacement, and can
     move around the blocks a little bit after the fact. There's
     always at least one of these, for the rung at the head of the
     block. *)
  fun toxblock { labelnum : int SM.map,
                 num_labels : int,
                 frame_stack_start : int,
                 is_initial : bool,
                 block = A.Block { name = current_lab,
                                   tmp_frame = A.Explicit tmp_frame_size,
                                   cmds : A.explicit_cmd list } } =
    let
      (* Rewrite displacements for next rail, as discussed above.
         Byte offset from the beginning of the block. *)
      val next_jumps : int list ref = ref nil

      fun offset (A.E { offset = off, ... }) = off

      datatype cmdres =
        (* Control flow cannot reach after the last instruction
           of the accumulator, for example after translating Exit
           or a non-conditional jump. *)
          Finished of Acc.acc
        | Continue of Acc.acc

      (* XXX hoist? *)
      fun onecmd acc cmd : cmdres =
        let
          fun assert16 (t as A.E { offset, size, comment }) =
            if size = A.S16
            then ()
            else raise ToX86 ("Command requires 16-bit temporary; got " ^
                              A.explicit_tmptos t ^ " in cmd:\n" ^
                              A.explicit_cmdtos cmd)
        in
          case cmd of
            A.Label _ => raise ToX86 "bug: unexpected Label"
          | A.Nop => Continue acc
          | A.Init =>
              let
                val () =
                  (* Sanity check... *)
                  if is_initial then ()
                  else raise ToX86 "bug: init instruction outside initial block?";

              (* If init_sp is odd, INC it so that it's aligned. This
                 allegedly gives much better performance. *)
                val acc =
                  if Word16.andb (INIT_SP, Word16.fromInt 1) =
                     Word16.fromInt 0
                  then acc
                  else if INIT_SP = Word16.fromInt 0xFFFF
                       then acc // DEC SP
                       else acc // INC SP
              in
                (* We actually point both EBP and EBX 32 bytes before the
                   temporary and local frames, respectively. This is because
                   the smallest printable disp8 we can actually represent
                   is 0x20 = 32.

                   Whenever we access these through [EBP+disp8] (etc.), we have
                   to be careful about the difference between a tmp frame
                   offset (starts at 0) and actual displacement (starts
                   at 0x20). It's also necessary that tmp frame and
                   frame stack both start beyond 0x20, since these values
                   won't wrap around. *)
                if Word16.< (TMP_FRAME_START, Word16.fromInt 0x20)
                then raise ToX86 ("TMP_FRAME_START must be at least 0x20 " ^
                                  "so that EBP can point 0x20 bytes before " ^
                                  "it.")
                else ();
                if frame_stack_start < 0x20
                then raise ToX86 ("frame_stack_start must be at least 0x20 " ^
                                  "so that EBP can point 0x20 bytes before " ^
                                  "it.")
                else ();
                if Word16.>= (INIT_SP, TMP_FRAME_START)
                then raise ToX86 ("initial stack pointer should be less than " ^
                                  "temporary frame start, or they'll interfere")
                else ();
                Continue `
                Tactics.initialize (acc,
                                    Word16.-(TMP_FRAME_START,
                                             Word16.fromInt 0x20),
                                    Word16.fromInt (frame_stack_start - 0x20))
              end
          | A.Exit => Finished ` Tactics.exit acc
          | A.SaveTemps (A.Explicit n) =>
              (* Add n to EBP. We know that it's in 0x00000000-0x0000FFFF,
                 so we just modify the 16-bit part, BP. *)
              Continue `
              Tactics.add_bp acc tmp_frame_size (Word16.fromInt n)
          | A.RestoreTemps (A.Explicit n) =>
              Continue `
              Tactics.sub_bp acc tmp_frame_size (Word16.fromInt n)
          | A.ExpandFrame n =>
              Continue `
              Tactics.add_bx acc tmp_frame_size (Word16.fromInt n)
          | A.ShrinkFrame n =>
              Continue `
              Tactics.sub_bx acc tmp_frame_size (Word16.fromInt n)

          | A.Push tmp =>
              let in
                assert16 tmp;
                Continue `
                Tactics.push_tmp16 acc (offset tmp)
              end
          | A.Pop tmp =>
              let in
                assert16 tmp;
                Continue `
                Tactics.pop_tmp16 acc (offset tmp)
              end

          | A.Immediate16 (tmp, w16) =>
              let in
                assert16 tmp;
                Continue `
                Tactics.imm_tmp16 acc (offset tmp) w16
              end
          | A.LoadLabel (tmp, other_lab) =>
              let
                val idx = getlabelnum labelnum other_lab
              in
                assert16 tmp;
                Continue `
                Tactics.imm_tmp16 acc (offset tmp) (Word16.fromInt idx)
              end

          | A.JumpCond (cond, dst_lab) =>
              let
                (* First, always put the label in SI. *)
                val current_idx = getlabelnum labelnum current_lab
                val dst_idx = getlabelnum labelnum dst_lab
                val rel_offset : int = dst_idx - current_idx
                (* Can't jump 0; that would be a backwards jump. *)
                val rel_offset = if rel_offset <= 0
                                 then rel_offset + num_labels
                                 else rel_offset
                val () = if rel_offset <= 0
                         then raise ToX86 "bug: backwards jump after wrap?"
                         else ()

                val acc = acc ++ AX ++ SI
                val acc = Tactics.imm_reg16 acc DH_SI
                  (Word16.fromInt rel_offset) -- AX

                (* For a simple binary CMP (a, b); JCC lab.
                   Upon entry, SI is claimed and has the relative offset
                   in it. Should work fine if a = b, though in that
                   case all of these become unconditional. *)
                fun binary JCC (a, b) =
                  let
                    (* XXX allow 8/32 *)
                    val () = (assert16 a; assert16 b)
                    val acc = acc ++ CX
                    val acc =
                      Tactics.mov16ind8 acc
                         (C <- Tactics.EBP_TEMPORARY (offset a)) ??
                      forget_reg16 M.ECX //
                      CMP (S16, C <- Tactics.EBP_TEMPORARY (offset b)) ??
                      forget_reg16 M.ECX -- CX //
                      (* Do release SI after the jump, since we continue... *)
                      JCC 0w0 -- SI
                  in
                    (* This is always a jump to the next rung; we need to
                       update the displacement later. *)
                    next_jumps := (Acc.insbytes acc - 1) :: !next_jumps;
                    Continue acc
                  end

                (* As though TEST a; JCC lab.
                   We don't have the TEST instruction, but simply moving
                   the value into the register will work, since we use
                   XOR to get it there. *)
                fun unary JCC a =
                  let
                    (* val () = raise ToX86 "implmeneted :)" *)
                    (* XXX allow 8/32 *)
                    val () = assert16 a
                    val acc = acc ++ AX
                    val acc =
                      Tactics.imm_ax16 acc (Word16.fromInt 0) //
                      XOR (S16, A <- Tactics.EBP_TEMPORARY (offset a)) ??
                      forget_reg16 M.EAX -- AX //
                      JCC 0w0 -- SI
                      (* PERF after JNZ, we know A is zero, which is
                         often useful *)
                  in
                    next_jumps := (Acc.insbytes acc - 1) :: !next_jumps;
                    Continue acc
                  end

                fun unconditional () =
                  let
                    (* To make an unconditional jump, we can clear
                       the overflow flag with any XOR. Since we just
                       used AX, just xor it with some printable value. *)
                    val acc = acc ++ AX //
                      XOR_A_IMM (I8 ` Word8.fromInt ` ord #"U") ??
                      (* We probably still know it, actually, but
                         its value is not going to be useful here *)
                      forget_reg16 M.EAX -- AX //
                      JNO 0w0 -- SI
                  in
                    (* We could try just falling through to the next label,
                       but it would be better to just do that optimization
                       in OptimizeAsm where it could apply more broadly. *)
                    next_jumps := (Acc.insbytes acc - 1) :: !next_jumps;
                    Finished acc
                  end
              in
                (* Now we just need to issue the appropriate sequence
                   of instructions (e.g. CMP then JNZ). *)
                (case cond of
                   A.Below (a, b) => binary JB (a, b)
                 | A.BelowEq (a, b) => binary JBE (a, b)
                 | A.Less (a, b) => binary JL (a, b)
                 | A.LessEq (a, b) => binary JLE (a, b)
                 | A.Eq (a, b) => binary JZ (a, b)
                 | A.NotEq (a, b) => binary JNZ (a, b)
                 | A.EqZero a => unary JZ a
                 | A.NeZero a => unary JNZ a
                 | A.True => unconditional ())
              end

          | A.PopJumpInd =>
              (* Pop the 16-bit label off the stack.
                 Compute a relative offset to that; put in SI.
                 Jump to next rung. *)
              let
                val current_idx = getlabelnum labelnum current_lab
                (* If we jump to the next rung with a value of 1, we DEC and
                   then SI will be 0. So, putting aside the modulus, we're
                   looking for exactly the distance dest_idx - current_idx. *)

                (* We'd like to compute (dest_idx - current_idx % num_labels)
                   with some caveats: A jump of 0 actually needs to be
                   represented as num_labels, not 0, because we're going to
                   start on the next rung (which would already be past the
                   current block). Worse, computing mod num_labels is way
                   too complicated without access to instructions like IDIV.

                   (Note: If we knew the number of labels was a power of two,
                   we could do this with AND_AX_IMM.)

                   Instead, we just compute (dest - current) naively, and
                   then add num_labels. This means that we *always* do at
                   least a full trip around (combination safe style!), but
                   that's probably cheaper than trying to calculate the
                   modulus in the first place, and much less code. *)

                (* Goal is to set SI = (stack_top - current_idx) + num_labels,
                   which is the same as
                   stack_top - (current_idx - num_labels)

                   So we generate
                   tmp <- immediate (current_idx - num_labels)
                   POP SI
                   SUB SI <- tmp
                   JMP to next rail *)
                (* XXX note that in a Call, we did a save_tmps right
                   before this. So our temp frame is not really set up
                   correcty. But in normal uses, it's safe to use
                   basically any temporary offset here, since it's
                   either a fresh frame (call) or dying one (return). *)
                val tmp = tmp_frame_size
                val subtractand = Word16.-(Word16.fromInt current_idx,
                                           Word16.fromInt num_labels)
                val () = Acc.assert_unclaimed acc SI
                val acc = acc ++ SI // POP SI ?? forget_reg16 M.ESI
                val acc = Tactics.imm_tmp16 acc tmp subtractand
                val acc = acc //
                  SUB (S16, DH_SI <- Tactics.EBP_TEMPORARY tmp) ??
                  forget_reg16 M.ESI
                (* We know that the quantity has to be positive, by
                   construction; The smallest (dest - cur) can be is
                   0 - (num_labels - 1) = 1 - num_labels, and since we add
                   num_labels, we'd get 1. So we know the processor flags
                   reflect a non-zero result. We can therefore accomplish
                   a non-conditional jump here with JNZ (or others).

                   (Note that sometimes we want to just NOP into the rung...
                   we might want to record that this is unconditional, then?) *)
                val acc = acc // JNZ 0w0
                (* And record that we need to update the displacement. *)
                val () = next_jumps := (Acc.insbytes acc - 1) :: !next_jumps
              in
                (* Don't relinquish SI, though moot since this is the end
                   of the block. *)
                Finished `
                acc
              end

          | A.Xor (a, b) =>
              let in
                (* XXX allow 32 *)
                assert16 a; assert16 b;
                Continue `
                Tactics.xor_tmp16 acc (offset a) (offset b)
              end

          | A.And (a, b) =>
              let in
                (* XXX allow 32 *)
                assert16 a; assert16 b;
                Continue `
                Tactics.and_tmp16 acc (offset a) (offset b)
              end

          | A.Sub (a, b) =>
              let in
                (* XXX allow 32 *)
                assert16 a; assert16 b;
                Continue `
                Tactics.sub_tmp16 acc (offset a) (offset b)
              end

          | A.Add (a, b) =>
              let in
                (* XXX allow 32 *)
                assert16 a; assert16 b;
                Continue `
                Tactics.add_tmp16 acc (offset a) (offset b)
              end

          | A.Mov (a, b) =>
              let in
                (* XXX allow 32 *)
                assert16 a; assert16 b;
                Continue `
                Tactics.mov_tmp16_to_tmp16 acc (offset a) (offset b)
              end

          | A.FrameOffset (tmp, off) =>
              let
                val () = assert16 tmp
                (* We just want tmp <- ebx + off + 0x20.
                   (Addresses point to the actual object, whereas
                   ebx is offset -0x20 for efficiency.)
                   Since we sub, compute the literal as the
                   negation of off + 0x20 *)
                val subtractand =
                  Word16.~ (Word16.+ (off, Word16.fromInt 0x20))

                (* First we'll set tmp = ebx,
                   then SUB the appropriate literal. *)
                val acc = Tactics.mov16ind8 acc
                  (Tactics.EBP_TEMPORARY (offset tmp) <~ B)
              in
                Continue `
                Tactics.sub_tmp16_lit acc (offset tmp) subtractand
              end

          | A.Putc tmp =>
              let in
                (* XXX allow 8-bit? *)
                assert16 tmp;
                Continue `
                Tactics.putc16 acc (offset tmp)
              end

          | A.Out8 (port, byte) =>
              let in
                assert16 port; assert16 byte;
                Continue `
                Tactics.out8_16 acc (offset port) (offset byte)
              end

          | A.Load16 (dst, addr) =>
              let in
                assert16 dst; assert16 addr;
                Continue `
                Tactics.load16 acc (offset dst) (offset addr)
              end

          | A.Load8 (dst, addr) =>
              let in
                assert16 dst; assert16 addr;
                Continue `
                Tactics.load8 acc (offset dst) (offset addr)
              end

          | A.Store16 (addr, src) =>
              let in
                assert16 addr; assert16 src;
                Continue `
                Tactics.store16 acc (offset addr) (offset src)
              end

          | A.Store8 (addr, src) =>
              let in
                assert16 addr; assert16 src;
                Continue `
                Tactics.store8 acc (offset addr) (offset src)
              end

          (*
          | A.Immediate32 (tmp, w32) =>
          | A.Complement a =>
            *)
          | _ =>
              let in
                (* Continue `
                (acc // COMMENT ("TODO " ^ ASM.explicit_cmdtos cmd)) *)
                raise ToX86 ("Unimplemented cmd: " ^
                             ASM.explicit_cmdtos cmd)
              end
        end

      val acc =
        if is_initial
        then
          (* If we're just starting, we know nothing. *)
          Acc.empty ASSEMBLY_CTX M.all_unknown
        else
          (* For a normal block, we know that EBX and EBP are claimed
             for frame pointers and shouldn't be disturbed. We could
             also add other invariants here if we know them. *)
          Acc.empty ASSEMBLY_CTX M.all_unknown ++
          EBX ++
          EBP

      (* val () = print ("Starting acc: " ^ Acc.debug_string acc ^ "\n") *)

      (* All blocks have a rung at the front, even the initial one.
         (In the case of the initial one, we set the initial IP so
         that it starts right after the rung, since we can't control
         our registers such that we actually end up entering it. No
         block should try to jump to the init block again, but it would
         work.) *)
      val acc = acc ++ SI
      val acc = acc // DEC SI // JNZ 0w0
      val () =
        (* Sanity check. Alternatively, we could return this for each
           block. *)
        if Acc.insbytes acc = RUNG_SIZE then ()
        else raise ToX86 "bug: expected rung to be a specific size"

      (* Record the offset of that 0w0 to be rewritten to a real jump
         to the next rail. *)
      val () = next_jumps := (Acc.insbytes acc - 1) :: !next_jumps
      val acc = acc -- SI

      (* Note: This assumes we never enter blocks other than through
         the rung, which is true today but could be broken by reasonable
         optimizations (e.g. direct forward jumps). Knowing we have
         some register with 0 in it is pretty useful. *)
      val acc =
        if is_initial
        then acc
        else acc ?? learn_reg16 M.ESI (Word16.fromInt 0)

      fun docmds (acc, nil) =
        (* If we got here, then execution continued to the end
           of the block, so we need to explicitly jump to the
           next one.

           PERF: We can usually fall through to the next block
           (e.g., not execute the JNZ) instead. (Note that the
           very last block must explicitly jump in order to
           overflow the IP.) *)
        let
          val acc = acc // COMMENT "fallthrough"
          val acc = acc ++ EAX ++ ESI
          val acc = Tactics.imm_ax16 acc (Word16.fromInt 0) //
            PUSH AX //
            POP SI ?? learn_reg16 M.ESI (Word16.fromInt 0) //
            (* We specifically zero it then INC so that we know
               the zero flag is clear here. *)
            INC SI ?? learn_reg16 M.ESI (Word16.fromInt 1) //
            (* Keep SI claimed, though it is probably moot. *)
            JNZ 0w0 -- EAX
        in
          next_jumps := (Acc.insbytes acc - 1) :: !next_jumps;
          acc
        end

        | docmds (acc, cmd :: cmds) =
        let
          val acc = acc // COMMENT (ASM.explicit_cmdtos cmd)
        in
          (case onecmd acc cmd of
             Finished acc => acc
           | Continue acc => docmds (acc, cmds))
        end

      val acc = docmds (acc, cmds)

      fun debug_print () =
        let val insns = Acc.insns acc
        in
          print ("#" ^ Int.toString (getlabelnum labelnum current_lab) ^ ". " ^
                 current_lab ^ ":\n");
          (* XXX print next jumps, etc. *)
          app (fn ins => print ("   " ^ insstring ins ^ "\n")) insns;
          print "\n"
        end

      (* Use the .debug file written by the compiler. *)
      val () = if false andalso !Flags.verbose then debug_print ()
               else ()

      (* This is the fast way to do it. *)
      (* val vec = Acc.encoded acc *)

      (* This way checks printability, cuz it's easy to mess up. *)
      val messages = ref (nil : string list)
      fun encode offset nil = nil
        | encode offset (ins :: rest) =
        let
          val v = EncodeX86.encode ASSEMBLY_CTX ins
          (* Anything in next_jumps is updated later, so exempt from this. *)
          fun in_next_jumps idx = List.exists (fn i => i = idx) (!next_jumps)
          fun vlist v = Word8Vector.foldr op:: nil v
        in
          Word8Vector.appi
          (fn (i, b) =>
           if Tactics.printable b orelse in_next_jumps (offset + i)
           then ()
           else
             messages :=
             ("!!! Non-printable byte @" ^
              Int.toString (offset + i) ^ " encoding " ^
              X86.insstring ins ^ "   [" ^
              StringUtil.delimit " "
              (map Word8.toString (vlist v)) ^ "]") :: !messages) v;
          v :: encode (offset + Word8Vector.length v) rest
        end
      val vec = Word8Vector.concat (encode 0 (Acc.insns acc))
    in
      (current_lab, vec, !next_jumps, acc, rev (!messages))
    end

  (* Convert the assembly program to x86. The labels in the order
     they appear become the blocks (each one is a rung). It may not
     be possible to place these xblocks into the code segment (e.g.
     because jumps are too far), so the driver needs to check for several
     problems that could occur, and then call toxblocks_round again
     with an adjusted program. *)
  fun toxblocks_round (blocks : A.explicit_block list,
                       frame_stack_start : int,
                       initial_label : string) =
    let
      (* Each block will have a rung of the ladder precede it. We
         give each of these a number, in sequence, so that we
         can use these numbers as absolute addresses, and compute
         relative addresses by subtracting the current address. *)
      val labels =
        Vector.fromList (map (fn (A.Block { name, ... }) => name) blocks)
      val labelnum = Vector.foldli (fn (i, lab, m) =>
                                    SM.insert (m, lab, i)) SM.empty labels
      val num_labels = Vector.length labels

      val xblocks = map (fn (block as A.Block { name, ... }) =>
                         toxblock { labelnum = labelnum,
                                    num_labels = Vector.length labels,
                                    frame_stack_start = frame_stack_start,
                                    is_initial = (name = initial_label),
                                    block = block }) blocks
    in
      xblocks
    end

  fun w8vtos v : string =
    CharVector.tabulate (Word8Vector.length v,
                         fn i => chr (Word8.toInt (Word8Vector.sub (v, i))))

  (* If we fail to fit an ASM block into an x86 xblock (the main limit being
     the displacement of the jump from the rung over the xblock itself) *)
  fun split (A.Program { blocks, frame_stack_start, datasegment },
             problem_blocks : xblock SM.map) =
    let
      fun oneblock (orig as A.Block { name, tmp_frame, cmds }) =
        case SM.find (problem_blocks, name) of
          NONE => [orig]
        | SOME _ =>
            (* Currently we blindly split in the middle of the command
               space. This is definitely suboptimal because:
                - Some cmds take many more x86 bytes to implement
                  than others, so we may significantly miss the middle
                  of the x86 code this way.
                - Splitting is particularly sensitive to jumps, mainly
                  because of the minimum displacement. It's usually
                  more efficient to split before a jump than after.

               PERF: We could maybe improve this if toxblocks_round returned a
               an alignment between the cmd offsets and the x86 offsets. *)
            let
              val num_cmds = length cmds
              val (first, second) = ListUtil.cleave (num_cmds div 2) cmds
              (* Could base this on 'name', but we don't want to keep appending
                 "_split_split_split" if we split multiple times... *)
              val splitlab = CILUtil.newlabel "tox86_split_"
            in
              if !Flags.verbose
              then print ("Split " ^ name ^ " to " ^ splitlab ^ "\n")
              else ();
              if num_cmds <= 1
              then raise ToX86 ("block " ^ name ^ " can't be split further??")
              else [A.Block { name = name,
                              tmp_frame = tmp_frame, cmds = first },
                    (* Just falls through to the second half... *)
                    A.Block { name = splitlab,
                              tmp_frame = tmp_frame, cmds = second }]
            end

      val blocks = List.concat (map oneblock blocks)
    in
      A.Program { blocks = blocks,
                  frame_stack_start = frame_stack_start,
                  datasegment = datasegment }
    end

  fun tox86 (program as A.Program { blocks, frame_stack_start, datasegment }) =
    let
      (* We need to find this later so that we can return its start
         position. *)
      val initial_label =
        case blocks of
          nil => raise ToX86 "ASM program is empty? Impossible!"
        | A.Block { name, ... } :: _ => name

      val xblocks : xblock list =
        toxblocks_round (blocks, frame_stack_start, initial_label)

      val total_size = ref 0
      (* Set of the blocks that need to be proactively split. *)
      val problem_blocks = ref (SM.empty : xblock SM.map)
      fun oneblock (xb as (lab, vec, jmps, _, _)) =
        let val len = Word8Vector.length vec
        in
          total_size := !total_size + len;
          (* We need a jump from right after this block's rung
             to the next block. If the current block is too big
             for that jump, we won't succeed. *)
          (* XXX maybe cleaner to find the earliest jump in the
             jump list? *)
          (* XXX 0x7e - 0x20 is very conservative, because there
             may not be a jump at the very end (in fact we should
             try to avoid such a thing and just use fallthrough,
             because the current approach makes 32 bytes of padding
             between each block.)

             Instead, we should use 0x7e here (that is truly an
             upper bound) and fail below if the specific jumps
             end up non-printable. *)
          (* PERF Also: We can sometimes have code blocks larger than
             127 bytes, by having the rung's jump target a jump that
             exists within the code block. This works for an
             unconditional jump (if we jump to the prelude that makes
             it unconditional) and it works for a JNZ instruction
             (true because we just succeeded at a JNZ) and probably
             others. This is a small code size improvement and reduces
             the total number of blocks. *)
          if len - RUNG_SIZE > (0x7e - 0x20)
          then problem_blocks := SM.insert (!problem_blocks, lab, xb)
          else ()
        end
    in
      app oneblock xblocks;
      if not (SM.isempty (!problem_blocks))
      then tox86 (split (program, !problem_blocks))
      else
        let
          val () = print ("No early problem blocks!\n")
          val () = print ("Total code bytes: " ^
                          Int.toString (!total_size) ^ "\n")
          val () =
            if !Flags.verbose
            then app (fn (l, v, _, _, _) =>
                      print (l ^ ": " ^ w8vtos v ^ "\n")) xblocks
            else ()

          val () = app (fn (l, _, _, _, msgs) =>
                        let in
                          if List.null msgs
                          then ()
                          else print ("** " ^ l ^ ":\n");
                          app (fn m => print ("  " ^ m ^ "\n")) msgs
                        end) xblocks

          val cs = Segment.empty ()
          val () = Segment.set_repeating_string cs 0 65536 " " (* "(CODE SEGMENT)" *)

          (* For debugging, each of the blocks, ordered by start index. *)
          val addresses = IIM.empty ()

          (* OK, now we want to insert our blocks into the code segment and
             patch up the jumps. This may not be possible, in which
             case we'll make some adjustment (like splitting a block)
             and then start again.

             - The blocks must appear in the order they occur, since addressing
               is relative.
             - We have to fill in the jump offsets. These currently always
               go to the beginning of the next block.
             - If a jump offset can't be output because it's not printable,
               then we'll have to try again.

             Note that for the jump instructions, the displacement
             is measured from the address of the next instruction,
             so for example JMP 0 just executes the next
             instruction. *)

          (* This is filled in with the actual start of the initial block
             (not its rung), which we'll use to kick off the execution. *)
          val init_addr = ref (NONE : Word16.word option)
          (* Current offset within code segment; this is the first
             address where we can acceptably place the next block.

             We start this at some value like 0x2020 in order to ensure
             that init has a printable address. *)
          val start_ip = 0x2020

          (* XXX could easily pass this instead of it being a ref *)
          val cur = ref start_ip
          (* XXX also, some better check that after we've wrapped
             around, we can't start writing over start_ip (this probably
             means we have too much code to fit) *)

          (* Fill the rest of the code segment, such that when control
             ends up on !cur, it makes its way back to the beginning of
             cs, and update cur so that it's near the beginning of the
             segment. This may involve filling the remainder of the
             segment with unconditional jumps. *)
          fun fill_rest () =
            (* We need at least enough space to write a single jump.
               XXX This is possible; should call fill_rest
               before getting ourselves stuck here! *)
            if !cur + JMP_SIZE > 65536
            then raise ToX86 ("Not enough space for fill_rest: cur = " ^
                              Int.toString (!cur))
            else
              let
                (* Consider placing the wraparound jump right here. *)
                val srcip = !cur + JMP_SIZE
                (* Displacement to reach 65536=0. *)
                val min_disp = 65536 - srcip
              in
                (* Can we jump that far? *)
                if min_disp > 0x7e
                then
                  (* PERF we can cut it much closer. *)
                  if min_disp > 0x7e * 2
                  then
                    let
                      val jmp =
                        EncodeX86.encodelist ASSEMBLY_CTX
                        (* XOR with 'L' twice. *)
                        [XOR_A_IMM (I8 0wx4C),
                         XOR_A_IMM (I8 0wx4C),
                         (* Overflow flag is definitely clear now. *)
                         JNO (Word8.fromInt 0x7e)]
                    val () = if JMP_SIZE <> Word8Vector.length jmp
                             then raise ToX86 "bug: JMP_SIZE wrong"
                             else ()
                    val next_cur = !cur + JMP_SIZE + 0x7e
                  in
                    Segment.set_vec cs (!cur) jmp;
                    Segment.lock_range cs (!cur) JMP_SIZE;
                    cur := next_cur;
                    fill_rest ()
                  end
                  else
                    let
                      val padding =
                        EncodeX86.encodelist ASSEMBLY_CTX
                        (* XOR with 'p' twice, but no jump. *)
                        [XOR_A_IMM (I8 0wx70),
                         XOR_A_IMM (I8 0wx70)]
                      val padding_length = Word8Vector.length padding
                    in
                      (*
                      print ("fill_rest can't jump far enough: cur = " ^
                             Int.toString (!cur) ^ " srcip = " ^
                             Int.toString srcip ^ " min_disp = " ^
                             Int.toString min_disp ^ "\n"); *)
                      (* PERF jump closer, of course! *)
                      Segment.set_vec cs (!cur) padding;
                      Segment.lock_range cs (!cur) padding_length;
                      cur := !cur + padding_length;
                      fill_rest ()
                    end
                else
                  let
                    val () = print ("fill_rest generating wrapping jump " ^
                                    "cur = " ^
                                    Int.toString (!cur) ^ " srcip = " ^
                                    Int.toString srcip ^ " min_disp = " ^
                                    Int.toString min_disp ^ "\n");
                    (* Great, we can jump past 65536.
                       We may need to jump farther if the displacement
                       would be less than 0x20. *)
                    val disp = Int.max (min_disp, 0x20)
                    val jmp =
                      EncodeX86.encodelist ASSEMBLY_CTX
                      (* XOR with 'w' twice. *)
                      [XOR_A_IMM (I8 0wx77),
                       XOR_A_IMM (I8 0wx77),
                       (* Overflow flag is definitely clear now. *)
                       JNO (Word8.fromInt disp)]
                    val () = if JMP_SIZE <> Word8Vector.length jmp
                             then raise ToX86 "bug: JMP_SIZE wrong"
                             else ()
                    val next_cur = !cur + JMP_SIZE + disp
                  in
                    if next_cur < 65536
                    then raise ToX86 ("bug: expected cur to wrap, got " ^
                                      Int.toString next_cur)
                    else ();
                    Segment.set_vec cs (!cur) jmp;
                    Segment.lock_range cs (!cur) JMP_SIZE;
                    cur := next_cur - 65536
                  end
              end

          (* Control flow is at !cur, and we want to execute the
             block at the head of this list. The previous block
             has already been patched to jump to !cur. *)
          fun place ((xb as (lab, vec, jmps, acc, _)) :: rest) : unit =
            let val len = Word8Vector.length vec
            in
              if !Flags.verbose
              then print ("place w/ cur=" ^ Int.toString (!cur) ^ " block " ^
                          lab ^ " of length " ^ Int.toString len ^ "\n")
              else ();
              if !cur + len > 65536
              then
                let in
                  print " ... would overflow.\n";
                  fill_rest ();
                  place (xb :: rest)
                end
              else
                let
                  val abs_jmps = map (fn j => !cur + j) jmps
                  (* Earliest we could place the next one is right after
                     this block ends. *)
                  val min_pos = ref (!cur + len)
                  (* But it also has to be far enough for the jumps to
                     reach it with printable displacements. *)
                  val () = app (fn j =>
                                let
                                  val srcip = j + 1
                                  val disp = !min_pos - srcip
                                in
                                  (* XXX check that this logic is correct!
                                     6 Mar 2017
                                     0x20 *)
                                  if disp < 0x20
                                  then min_pos := !min_pos - (disp - 0x20)
                                  else ()
                                end) abs_jmps
                  val next_cur = !min_pos
                in
                  (* Can treat this like the !cur + len test above failing. *)
                  if next_cur >= 65536
                  then raise ToX86 ("need to handle the case that " ^
                                    "next_cur overflows")
                  else ();

                  if !Flags.verbose
                  then print ("write " ^ lab ^ " to " ^ Int.toString (!cur) ^ ".\n")
                  else ();
                  (if lab = initial_label
                   then
                     case !init_addr of
                       NONE => init_addr :=
                         SOME (Word16.fromInt (!cur + RUNG_SIZE))
                     | SOME _ => raise ToX86 "initial label emitted twice?!"
                   else ());
                  IIM.insert (addresses, !cur, (lab, acc));
                  Segment.set_vec cs (!cur) vec;
                  (* Lock everything except for the locations we need to
                     update (jumps) *)
                  Segment.lock_range cs (!cur) len;
                  app (Segment.unlock_idx cs) abs_jmps;

                  (* Fill in my jumps to point to next_cur. *)
                  app (fn j =>
                       let
                         val srcip = j + 1
                         val disp = next_cur - srcip
                       in
                         if disp > 0x7e
                         then raise ToX86 ("In 'place', needed to write " ^
                                           "displacement larger than 0x7e: " ^
                                           Int.toString disp)
                         else if disp < 0x20
                              then raise ToX86 ("In 'place', displacement " ^
                                                "is too small to be " ^
                                                "printable: " ^
                                                Int.toString disp)
                              else
                                let in
                                  Segment.set_idx cs j (Word8.fromInt disp);
                                  Segment.lock_idx cs j
                                end
                       end) abs_jmps;

                  (* Now control flow will be at next_cur. *)
                  cur := next_cur;
                  place rest
                end
            end
            | place nil =
            let in
              (* print ("Out of blocks with cur=" ^ Int.toString (!cur) ^
                     " and start_ip=" ^ Int.toString start_ip ^ "\n"); *)
              (* We've used up all the blocks and execution is at !cur.
                 Need to get control back to start_ip. *)
              if !cur <= start_ip
              then
                let
                  val disp = start_ip - !cur
                in
                  if disp = 0
                  then print " .. exact hit!\n"
                  else
                    (* PERF: We can also do shorter jumps when we
                       get close! *)
                    (* Jump the maximum amount if we can.
                       PERF: We could have two parallel tracks
                       of JO/JNO here (since once we do one jump,
                       we know the value). Alternatively, we could
                       just clear the overflow flag once at the
                       beginning? *)
                    if disp >= 0x7e + JMP_SIZE
                    then
                      let
                        val jmp =
                          EncodeX86.encodelist ASSEMBLY_CTX
                          (* XOR with 'G' twice. *)
                          [XOR_A_IMM (I8 0wx47),
                           XOR_A_IMM (I8 0wx47),
                           (* Maximum displacement. *)
                           JNO (Word8.fromInt 0x7e)]
                        val () = if JMP_SIZE <> Word8Vector.length jmp
                                 then raise ToX86 "bug: JMP_SIZE wrong"
                                 else ()
                        val next_cur = !cur + JMP_SIZE + 0x7e
                      in
                        (* print ("Write jmp to " ^ Int.toString (!cur) ^ "...\n"); *)
                        Segment.set_vec cs (!cur) jmp;
                        Segment.lock_range cs (!cur) JMP_SIZE;
                        cur := next_cur;
                        place nil
                      end
                    else
                    (* Pad with "INC AX".

                       Assumes AX is not holding any interesting
                       values between blocks.

                       A better one-byte padding might be the
                       segment prefix bytes, which don't do anything
                       if the next instruction is DEC SI. *)
                    let in
                      Segment.set_idx cs (!cur) 0wx40;
                      Segment.lock_idx cs (!cur);
                      cur := !cur + 1;
                      place nil
                    end
                end
              else
                let in
                  print " .. gotta wrap\n";
                  fill_rest ();
                  place nil
                end
            end

          val () = place xblocks

          val init_ip =
            case !init_addr of
              NONE => raise ToX86 ("The initial block " ^ initial_label ^
                                   "was never written to the code segment!?")
            | SOME ia => ia
        in
          if Tactics.printable16 init_ip
          then ()
          else raise ToX86 ("The init_ip is not printable: " ^
                            Word16.toString init_ip);
          { cs = Segment.extract cs,
            codebytes = !total_size,
            debug = IIM.listItemsi addresses,
            init_ip = init_ip,
            ds = datasegment }
        end
    end

end
