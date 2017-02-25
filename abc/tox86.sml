(* And at long last, conversion to x86.

   If we had the full range of x86 opcodes, conversion from
   the ASM language to x86 would be pretty straightforward.
   There are a few big issues working against us:

    - The only jump construct we have is the family of relative short
      jumps, like JNZ, with a fixed offset. This offset is a single
      byte. Since it's printable, we can only jump from 0x20 (+32
      bytes) to 0x7e (+126 bytes). Note that there is no backward
      jump!

    - Our program needs to have backward jumps in order to perform
      loops. The only way I could even figure out to make the
      instruction pointer decrease is to overflow it. Unfortunately it
      doesn't simply work to let execution run past CS:FFFF, but it
      does work to perform a short forward jump (like the onese we
      have access to) that crosses CS:FFFF. These appear to be
      performed in on the 16-bit word, and thus wrap around to
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

      rel = dst - src + 1 (mod the total number of blocks; this can't be negative)
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

  val INIT_SP = Word16.fromInt 0x7e7e
  (* XXX doubt this can really be a constant *)
  val INIT_IP = Word16.fromInt 0x2020

  val INIT_BP = Word16.fromInt 0x7e80

  open X86
  infix <- <~

  open Acc
  infix // ?? ++ --

    (* bad idea?
  datatype problem =
    (* The block with the given label was bigger than the maximum
       block size. This includes the case that there was a conditional
       jump too close to the end of the block. *)
    BlockTooBig of string
  | NotEnoughBlocks
    *)

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
  fun layout_block (labelnum : int SM.map,
                    frame_stack_start : int,
                    is_initial : bool,
                    lab : string,
                    cmds : A.explicit_tmp A.cmd list) =
    let
      (* Rewrite displacements for next rail, as discussed above.
         Byte offset from the beginning of the block. *)
      val next_jumps : int list ref = ref nil

      (* XXX hoist? *)
      fun onecmd acc cmd =
        case cmd of
          A.Label _ => raise ToX86 "bug: unexpected Label"
        | A.SaveTempsNamed _ => raise ToX86 "bug: unexpected SaveTempsNamed"
        | A.RestoreTempsNamed _ => raise ToX86 "bug: unexpected RestoreTempsNamed"
        | A.Init =>
            let in
              (* Sanity check... *)
              if is_initial then ()
              else raise ToX86 "bug: init instruction outside initial block?";
              Tactics.initialize (acc, INIT_BP, Word16.fromInt frame_stack_start)
            end
        | A.ExpandFrame n => raise ToX86 "unimplemented expandframe"
(*
        | A.ShrinkFrame n =>
        | A.FrameOffset (dst, off) =>
        | A.Load8 (dst, addr) =>
        | A.Load16 (dst, addr) =>
        | A.Store8 (addr, src) =>
        | A.Store16 (addr, src) =>
        | A.Push tmp =>
        | A.Pop tmp =>
        | A.LoadLabel (tmp, lab) =>
        | A.PopJumpInd =>
        | A.JumpCond (cond, lab) =>
        | A.Immediate8 (tmp, w8) =>
        | A.Immediate16 (tmp, w16) =>
        | A.Immediate32 (tmp, w32) =>
        | A.Add (a, b) =>
        | A.Sub (a, b) =>
        | A.Complement a =>
        | A.Mov (a, b) =>
        | A.Xor (a, b) =>
*)
        | _ => raise ToX86 ("Unimplemented cmd: " ^
                            ASM.cmdtos ASM.explicit_tmptos cmd)

      val acc =
        if is_initial
        then
          (* If we're just starting, we know nothing. *)
          Acc.empty (X86.CTX { default_32 = false }) M.all_unknown
        else
          (* For a normal block, we know that EBX and EBP are claimed
             for frame pointers and shouldn't be disturbed. We could
             also add other invariants here if we know them. *)
          Acc.empty (X86.CTX { default_32 = false }) M.all_unknown ++
          EBX ++
          EBP

      (* All blocks have a rung at the front, even the initial one.
         (In theT case of the initial one, we set the initial IP so
         that it starts right after the rung, since we can't control
         our registers such that we actually end up entering it. No
         block should try to jump to the init block again, but it would
         work.) *)
      val acc = acc ++ SI
      val acc = acc // DEC SI // JNZ 0w0
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

      fun docmds (acc, nil) = acc
        | docmds (acc, cmd :: cmds) =
        docmds (onecmd acc cmd, cmds)

      val acc = docmds (acc, cmds)
    in
      (Acc.encoded acc, !next_jumps)
    end

  (* Attempt to lay out the assembly program. The labels in the order
     they appear become the blocks (each one is a rung). The layout
     may not be valid, so the driver needs to check for several
     problems that could occur, and then call layout_round again
     with an adjusted program. *)
  fun layout_round (blocks : A.explicit_tmp A.block list,
                    frame_stack_start : int,
                    initial_label : string) =
    let
      (* Each block will have a rung of the ladder precede it. We
         give each of these a number, in sequence, so that we
         can use these numbers as absolute addresses, and compute
         relative addresses by subtracting the current address. *)
      val labels = Vector.fromList (map #1 blocks)
      val labelnum = Vector.foldli (fn (i, lab, m) =>
                                    SM.insert (m, lab, i)) SM.empty labels

      val xblocks = map (fn (lab, cmds) =>
                         layout_block (labelnum, frame_stack_start,
                                       lab = initial_label,
                                       lab,
                                       cmds)) blocks

    in
      xblocks
    end

  fun tox86 (A.Program { blocks, frame_stack_start, datasegment }) =
    let
      (* XXX probably need to move initial block into the middle,
         like repetedly advance it (or binary search) until it's printable? *)
      val initial_label =
        case blocks of
          nil => raise ToX86 "ASM program is empty? Impossible!"
        | (init, _) :: _ => init

      val xblocks = layout_round (blocks, frame_stack_start, initial_label)

      val cs = raise ToX86 "still need to do various things to construct cs"
    in
      (* XXX also return initial instruction pointer? *)
      { cs = cs,
        ds = datasegment }
    end

end