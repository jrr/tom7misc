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

      (Note that a rung does DEC first, so if you want to execute that block,
       the register should be 1 when you enter the rung. This usually means
       just adding one to the register before starting.)

      and PopJumpInd is
      pop reg
      sub reg, src
      inc reg
      jmp next_rung

      .. the belief is that this overhead (once per jump) is worth the increase
      in speed on the ladder (once per rung). Note that most jumps are through
      JumpCond, because only function pointers and return addresses need the
      complexity of PopJumpInd. (Though this optimization is not yet implemented.)

    - Using the ladder to jump like this does trash whatever register we're using,
      (but can we make use of the fact that it is 0 upon normal entry to a block?)
      and also messes up flags. ASM doesn't need these to be preserved across
      jumps, so nor should our translation to x86.

    - Complication: Long blocks (maximum displacement)
           ... bisect the block; try again (changes number of blocks, so changes
                        the code, so changes code size...)
           ... can be dumb about bisection by just treating it as unconditional
               jump to next block, but this can often be smarter by either
               jumping over the rung, or just entering the rung with the reg
               set to 1 (this is basically the same as doing a normal jump
               except that we don't actually do the jmp next_rung).
    - Complication: Short blocks (min displacement)
           ... just gotta pad
           ... could possibly interleave??
    - Optimization: Some jumps will just be in range?
    - Optimization: Block layout order?
*)

structure ToX86 :> TOX86 =
struct

  exception ToX86 of string

  val INIT_SP = Word16.fromInt 0x7e7e
  val INIT_IP = Word16.fromInt 0x2020

  fun tox86 asm = raise ToX86 "unimplemented: tox86"

end