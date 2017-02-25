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
           DEC reg                    48
           JNZ rung_n+1               75 xx
         code-block:

      (TBD which register this is, but DEC is one byte no matter
      what). The register holds the number of rungs left to traverse;
      when it reaches 0. This register can be 16 bit, because of
      course there cannot be more than 64k rungs in the 64k code
      segment.

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
      reg <- rel
      jmp next_rung

      (Note that a rung does DEC first, so if you want to execute that
       block, the register should be 1 when you enter the rung. This
       usually means just adding one to the register before starting.)

      and PopJumpInd is
      pop reg
      sub reg, src
      inc reg
      jmp next_rung

      .. the belief is that this overhead (once per jump) is worth the
      increase in speed on the ladder (once per rung). Note that most
      jumps are through JumpCond, because only function pointers and
      return addresses need the complexity of PopJumpInd. (Though this
      optimization is not yet implemented.)

    - Using the ladder to jump like this does trash whatever register
      we're using, (but can we make use of the fact that it is 0 upon
      normal entry to a block?) and also messes up flags. ASM doesn't
      need these to be preserved across jumps, so nor should our
      translation to x86.

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
  (* XXX dunno if this can really be a constant *)
  val INIT_IP = Word16.fromInt 0x2020

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
  fun layout_block (labelnum : int SM.map) (is_initial : bool,
                                            lab : string,
                                            cmds : A.explicit_tmp A.cmd list) =
    let
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
    in
      raise ToX86 "unimplemented layout_block"
    end

  (* Attempt to lay out the assembly program. The labels in the order
     they appear become the blocks (each one is a rung). The layout
     may not be valid, so the driver needs to check for several
     problems that could occur, and then call layout_round again
     with an adjusted program. *)
  fun layout_round (blocks : A.explicit_tmp A.block list,
                    initial : string) =
    let
      (* Each block will have a rung of the ladder precede it. We
         give each of these a number, in sequence, so that we
         can use these numbers as absolute addresses, and compute
         relative addresses by subtracting the current address. *)
      val labels = Vector.fromList (map #1 blocks)
      val labelnum = Vector.foldli (fn (i, lab, m) =>
                                    SM.insert (m, lab, i)) SM.empty labels

      val xblocks = map (fn (lab, cmds) =>
                         layout_block labelnum (lab = initial, lab, cmds)) blocks

    in
      raise ToX86 "unimplemented layout_round"
    end

  fun tox86 asm = raise ToX86 "unimplemented: tox86"

end