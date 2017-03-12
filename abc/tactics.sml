structure Tactics :> TACTICS =
struct

  exception Tactics of string
  structure M = Machine
  datatype slot = datatype M.slot
  type mach = M.mach

  infixr 9 `
  fun a ` b = a b

  open X86
  infix <- <~

  open Acc
  infix // ?? ++ --

  fun w16tow8s (w : Word16.word) : Word8.word * Word8.word =
    let
      val wh = Word8.fromInt ` Word16.toInt `
        Word16.andb(Word16.>>(w, 0w8), Word16.fromInt 0xFF)
      val wl = Word8.fromInt ` Word16.toInt `
        Word16.andb(w, Word16.fromInt 0xFF)
    in
      (wh, wl)
    end

  fun w8stow16 (wh : Word8.word, wl : Word8.word) =
    Word16.orb(Word16.<<(Word16.fromInt (Word8.toInt wh), 0w8),
               Word16.fromInt (Word8.toInt wl))

  (* PERF: If we allow CR/LF, they need to be included here.
     Note that there are many loops from low to high, as well. *)
  val PRINT_LOW : Word8.word = 0wx20
  val PRINT_HIGH : Word8.word = 0wx7e
  fun printable p = p >= PRINT_LOW andalso p <= PRINT_HIGH
  fun printable16 w =
    let val (a, b) = w16tow8s w
    in printable a andalso printable b
    end

  fun dprint (f : unit -> string) = ()
  (* fun dprint f = print (f ()) *)

  fun ftos f = Real.fmt (StringCvt.FIX (SOME 2)) f

  fun for8 (lo : Word8.word) (hi : Word8.word) f =
    if lo > hi then ()
    else (ignore (f lo); for8 (Word8.+(lo, 0w1)) hi f)

  fun repeated 0 acc i = acc
    | repeated n acc i = repeated (n - 1) (acc // i) i


  (* Push the 16-bit value; if the high byte is zero then we can do
     it in one fewer byte. *)
  fun push16 (v : Word16.word) =
    let val (vh, vl) = w16tow8s v
    in
      if vh = 0w0
      then PUSH_IMM ` I8 vl
      else PUSH_IMM ` I16 v
    end

  (* Find printable C such that AND (AL, C) = VL; of course not always
     possible. *)
  fun inverse_and (al, vl) =
    let
      (* PERF -- don't need to search all chars. *)
      fun r 0wx7f = NONE
        | r c = if Word8.andb(al, c) = vl
                then SOME c
                else r (Word8.+(c, 0w1))
    in
      r 0wx20
    end

  infix ||
  fun acc1 || acc2 =
    if Acc.insbytes acc1 <= Acc.insbytes acc2
    then acc1
    else acc2

  (* Sometimes we have more than one strategy for emitting
     instructions, and the shortest one will depend on the
     machine state or specific values being used.

     This tries every strategy in the list, and returns
     the result of the one with the shortest instruction bytes.
     It doesn't take into account cycle count, different
     resulting machine states or claim lists, etc..

     A strategy can fail (give NONE) but if all fail, this
     aborts. *)
  fun multistrategy (aol : acc option list) : acc =
    let
      fun f (best, NONE) = best
        | f (NONE, SOME a) = SOME a
        | f (SOME a, SOME b) = SOME (a || b)
    in
      case foldl f NONE aol of
        NONE => raise Tactics "multistrategy failed!"
      | SOME best => best
    end

  (* XXX save_and_claim can cause many problems:
      - perturbs the stack; the continuation cannot do PUSH/POP safely
      - the caller may have claimed the register we're saving, and
        want to use it in the continuation...

     Probably we should just avoid using this, except when the
     continuation can be carefully constructed. Current code generation
     strategy usually results in a lot of unclaimed registers. *)

  (* Claim the multireg for the duration of the function call,
     saving the register (or friend) if necessary to enable this. *)
  fun save_and_claim acc (mr : multireg) (f : acc -> acc) : acc =
    case blocking_claim acc mr of
      NONE =>
        let
          val acc = acc ++ mr
          val acc = f acc
        in
          acc -- mr
        end
    | SOME old =>
        let
          (* PERF we should restore knowledge of the register
             we're re-claiming, but need to do it for the
             corresponding slots depending on whether it is
             16 or 32-bit. *)
          val acc = acc //
            PUSH old
            -- old
            ++ mr
          val acc = f acc
        in
          acc
          -- mr
          ++ old //
          POP old ??
          forget_multireg old
        end


  (* PERF: Maybe avoid claiming registers whose values we know? Those
     often help us generate shorter code. *)
  (* If all are used, then just pick one and save it to the stack, restoring
     it at the end. *)
  fun claim_reg16_stackok acc (l : reg list) (f : acc * reg -> acc) : acc =
    let
      (* n.b., when this calls save_and_claim, we know whether the claim
         will be blocked or not. *)
      fun get_unclaimed nil =
          (* No registers are unclaimed. *)
          (case l of
             nil => raise Tactics ("claim_reg16_stackok needs at least one " ^
                                   "register in the list")
           | r :: _ => save_and_claim acc (reg_to_multireg16 r)
                       (fn a => f (a, r)))
        | get_unclaimed (r :: rest) =
           let val mr = reg_to_multireg16 r
           in if can_be_claimed acc mr
              then save_and_claim acc mr (fn a => f (a, r))
              else get_unclaimed rest
           end
    in
      get_unclaimed l
    end

  (* Claim one of the registers in the list. If none are available, aborts. *)
  fun claim_reg16 acc (l : reg list) (f : acc * reg -> acc) : acc =
    let
      (* n.b., when this calls save_and_claim, we know whether the claim
         will be blocked or not. *)
      fun get_unclaimed nil =
        raise Tactics ("no unclaimed regs in claim_reg16. options were:\n" ^
                       StringUtil.delimit " " (map X86.regtos l) ^
                       "\nand acc was:\n" ^
                       Acc.debug_string acc)
        | get_unclaimed (r :: rest) =
           let val mr = reg_to_multireg16 r
           in if can_be_claimed acc mr
              then
                let val acc = acc ++ mr
                in f (acc, r) -- mr
                end
              else get_unclaimed rest
           end
    in
      get_unclaimed l
    end

  (* XXX to utils? *)
  structure FixedStack :>
  sig
    exception FixedStack of string
    type 'a fixedstack
    (* Give a sentinel value and maximum capacity. *)
    val empty : int * 'a -> 'a fixedstack
    val capacity : 'a fixedstack -> int
    val size : 'a fixedstack -> int
    (* Raises fixedstack if full. *)
    val push : 'a fixedstack * 'a -> unit
    val isempty : 'a fixedstack -> bool
    val popopt : 'a fixedstack -> 'a option
    (* Raises FixedStack if empty. *)
    val pop : 'a fixedstack -> 'a
  end =
  struct
    exception FixedStack of string
    type 'a fixedstack = int ref * 'a Array.array
    fun empty (cap, a) = (ref 0, Array.array (cap, a))
    fun capacity (_, arr) = Array.length arr
    fun size (s, _) = !s
    fun push ((s, arr), item) =
      let in
        Array.update (arr, !s, item);
        s := !s + 1
      end handle General.Subscript => raise FixedStack "full"
    fun isempty (ref 0, _) = true
      | isempty _ = false
    fun popopt (s, arr) =
      let in
        s := !s - 1;
        SOME (Array.sub (arr, !s))
      end handle General.Subscript => NONE
    fun pop (s, arr) =
      let in
        s := !s - 1;
        Array.sub (arr, !s - 1)
      end handle General.Subscript => raise FixedStack "empty"
  end

  local
    (* Table of the best way to transform a known value in AL to a target
       value (source-major index). Can't depend on other machine
       state, and can't change the values other than AL. (Arithmetic
       flags may change.) *)
    val byte_table = Array.array (256 * 256, (nil, ~1) : ins list * int)
    (* For any destination byte (index), what printable source would
       we like to start with in AL? (e.g. s with the smallest instruction
       bytes for byte_table[256 * s + index] *)
    val best_printable_src = Array.array (256, PRINT_LOW : Word8.word)

    fun populate_best_printable () =
      Util.for 0 255
      (fn index =>
       let
         fun get src = case Array.sub (byte_table, src * 256 + index) of
           (_, ~1) => raise Tactics "pbp: incomplete"
         | (_, b) => b
         val bestsrc = ref ` Word8.toInt PRINT_LOW
         val bestlen = ref ` get ` !bestsrc
       in
         Util.for (Word8.toInt PRINT_LOW) (Word8.toInt PRINT_HIGH)
         (fn src =>
          let val v = get src
          in
            if get src < !bestlen
            then
              let in
                bestsrc := src;
                bestlen := v
              end
            else ()
          end);
         Array.update (best_printable_src, index, Word8.fromInt (!bestsrc))
       end)

    fun tablesize () =
      let
        val ctr = ref 0
      in
        Array.app (fn (_, ~1) => raise Tactics "size: incomplete"
                    | (_, b) => ctr := !ctr + b) byte_table;
        !ctr
      end

    fun prtable () =
      Util.for 0 255
      (fn srci =>
       let in
         print ("\nSource: " ^ Int.toString srci ^ "\n");
         Util.for 0 255
         (fn dsti =>
          let in
            print ("  " ^ Int.toString dsti ^ ": ");
            (case Array.sub (byte_table, srci * 256 + dsti) of
               (_, ~1) => print "NONE\n"
             | (ins, b) => print ("[" ^ Int.toString b ^ "] " ^
                                  StringUtil.delimit "  "
                                  (map X86.insstring ins) ^ "\n"))
          end)
       end)

    fun savetableimage () =
      let
        val f = TextIO.openOut "bytetable.ppm"
      in
        TextIO.output (f, "P3 256 256 255\n");
        Util.for 0 255
        (fn row =>
         let in
           Util.for 0 255
           (fn col =>
            let
              val b =
                case Array.sub (byte_table, row * 256 + col) of
                  (_, ~1) => 255
                | (_, b) => if b > 255 then 255 else b
            in
              TextIO.output(f,
                            Int.toString b ^ " " ^
                            Int.toString b ^ " " ^
                            Int.toString b ^ " ")
            end);
           TextIO.output (f, "\n")
         end);
        TextIO.closeOut f;
        print "Wrote bytetable.ppm\n"
      end

    fun savetableimageascii () =
      let
        val f = TextIO.openOut "paper/bytetable.txt"
      in
        Util.for 128 255
        (fn row =>
         let in
           Util.for 0 159
           (fn col =>
            let
              val b = String.sub (" .-%@",
                                  #2 (Array.sub (byte_table, row * 256 + col)))
            in
              TextIO.output(f, implode [b])
            end);
         (* TextIO.output (f, "\n") *)
           ()
         end);
        TextIO.closeOut f;
        print "Wrote paper/bytetable.txt\n"
      end

    fun populate_byte_table () =
      let
        val start_time = Time.now ()

        (* Since a cell could be pushed more than once, this is actually
           an "empirical" value (the procedure below is deterministic...) *)
        val todo = FixedStack.empty (65536, (0, 0))

        fun edge_inc acc 0wxFF = NONE
          | edge_inc acc al =
          SOME (acc // INC AX, Word8.+ (al, 0w1))
        fun edge_dec acc 0wx00 = NONE
          | edge_dec acc al =
          SOME (acc // DEC AX, Word8.- (al, 0w1))
        fun edge_xor b acc al =
          SOME (acc // XOR_A_IMM ` I8 b, Word8.xorb (al, b))
        fun edge_and b acc al =
          SOME (acc // AND_A_IMM ` I8 b, Word8.andb (al, b))
        fun edge_sub b acc al =
          SOME (acc // SUB_A_IMM ` I8 b, Word8.- (al, b))

        (* XXX take context from somewhere?
           It doesn't affect correctness but would cause us to
           make mistakes about the size of some ops. *)
        val empty_acc = Acc.empty (CTX { default_32 = false }) M.all_unknown

        (* Loop over the todo stack until we're done. *)
        fun loop () =
          case FixedStack.popopt todo of
            NONE => ()
          | SOME (cell, expected) =>
          (* A cell could be improved before we even get to it. If this
             happens, we know that this cell will be explored because of
             the time it was pushed on the stack with this lower value.
             So just skip. *)
          if #2 ` Array.sub (byte_table, cell) < expected
          then loop ()
          else
          let
            (* This is a cell that has had its score improved. So
               look at all the cells we have edges to, and see if we
               have a faster path there via this cell now. *)
            val srci = cell div 256
            val dsti = cell mod 256
            val src = Word8.fromInt srci
            val dst = Word8.fromInt dsti

            (* So best takes us from src to dst in best_size
               bytes. Can we extend that to improve our path
               to (src, X) for some X? *)
            val (best, best_size) = Array.sub (byte_table, cell)
            val () = if best_size = ~1
                     then raise Tactics "stack contained uninitialized val?"
                     else ()

            val acc = empty_acc ?? learn_slot M.EAX ---@ src
            val acc = acc ++ AX

            (* Use the result of an edge to update the
               table if it's an improvement. *)
            fun try NONE = ()
              | try (SOME (acc, new_dst)) =
              let
                val new_dsti = Word8.toInt new_dst
                val newcell = srci * 256 + new_dsti
                fun isbetter new_bytes =
                  case Array.sub (byte_table, newcell) of
                    (* first path here is always better *)
                    (_, ~1) => true
                  | (_, old_bytes) =>
                      new_bytes + best_size < old_bytes
              in
                if isbetter ` Acc.insbytes acc
                then
                  let
                    val newbytes = best_size + Acc.insbytes acc
                  in
                    (*
                    print
                    ("Improved " ^ Int.toString srci ^
                     " to " ^ Int.toString dsti ^
                     " (" ^ Int.toString best_size ^
                     " + " ^ Int.toString (Acc.insbytes acc) ^
                     ")\n");
                    *)
                    (* Save the value we think this cell has,
                       so that we don't end up visiting it
                       multiple times without new information. *)
                    FixedStack.push (todo, (newcell, newbytes));
                    Array.update
                    (byte_table, newcell,
                     (best @ Acc.insns acc, newbytes))
                  end
                else ()
              end
          in
            (* Try different edges out of this node. *)
            (* Since we use a stack, this visits the INC/DEC
               edges last. This is good because you can always
               form long chains of INC/DEC to get between numbers,
               but we can almost always do better. *)
            try ` edge_inc acc dst;
            try ` edge_dec acc dst;
            for8 PRINT_LOW PRINT_HIGH
            (fn b =>
             let in
               try ` (edge_xor b acc dst);
               try ` (edge_and b acc dst);
               try ` (edge_sub b acc dst)
             end);
            loop ()
          end
      in
        (* First, fill in the diagonal to get started. We can go
           from a value to itself with no instructions; 0 bytes. *)
        Util.for 0 255
        (fn b =>
         let
           val cell = b * 256 + b
         in
           Array.update (byte_table, cell, (nil, 0));
           FixedStack.push (todo, (cell, 0))
         end);

        loop ();
        (* prtable (); *)
        (* savetableimage (); *)
        (* savetableimageascii (); *)
        print ("Total byte table size (opcode bytes): " ^
               Int.toString ` tablesize () ^ "\n" ^
               "Computed in " ^
               Time.toString (Time.-(Time.now (), start_time)) ^
               " sec.\n");
        Array.app (fn (_, b) =>
                   if b = ~1 then raise Tactics "failed to complete byte table"
                   else ()) byte_table
      end
    val () = populate_byte_table ()
      handle Tactics s =>
        (print ("Initializing byte table failed: " ^ s ^ "\n");
         raise (Tactics s))
    val () = populate_best_printable ()

    (* May not change AH, since we use this in 16-bit immediates. We could
       be more aggressive (IMUL, etc.) if we did't care about preserving AH. *)
    fun imm_al_known acc al vl : acc =
      let
        val () = dprint (fn () =>
                         "imm_al_known have: " ^ Word8.toString al ^
                         " v: " ^ Word8.toString vl ^ " with machine:\n" ^
                         M.debugstring (mach acc) ^ "\n")
        val () =
          case M.slot (mach acc) M.EAX ---@ of
            NONE => raise Tactics "must really know al in imm_al_known"
          | SOME al' => if al <> al'
                        then raise Tactics "wrong known al in imm_al_known?"
                        else ()
        val () = assert_claimed acc AX
        val srci = Word8.toInt al
        val dsti = Word8.toInt vl
        val (ins, bytes) = Array.sub (byte_table, srci * 256 + dsti)
        fun ap acc nil = acc
          | ap acc (i :: rest) = ap (acc // i) rest
      in
        ap acc ins ??
        learn_slot M.EAX ---@ vl
      end

    (* Load an arbitrary value into AH and optionally a printable one
       into AL.

       Loading something into AH. One reasonably brief way to do this is to
       misalign the stack.
       Load the desired value into AL, so that we have AH=ah?, AL=vh.
           PUSH AX, giving
                    AL AH
                     |  |
                     v  v
              SP -> vh ah? ?? ?? ?? ...
           PUSH abcd, giving    (8-bit push is 0 padded; no-good)
              SP -> cd ab vh ah? ?? ?? ?? ...
           INC SP, giving
              SP -> ab vh ah? ?? ?? ?? ...
           POP AX, giving
              SP -> ab vh ah? ?? ?? ?? ...
                     |  |
                     v  v
                    AL AH
           INC SP (keep stack aligned; avoid unbounded growth)

         PERF: IMUL may be good for this too.

         Instead of PUSHing an immediate, we can also just DEC SP,
         which leaves AL unknown at the end, but is shorter. If
         vlo is NONE, then do this. *)
    fun imm_ah_known acc (ah, al) vlo vh : acc =
      let
        (* Push two bytes on the stack, where the top must be
           the given one. Can do anything it wants to AX.

           PERF if we have the value in the low byte of any register,
           we could push it here... *)
        fun pushbyte b =
          let
            val via_ax = SOME (imm_al_known acc al b // PUSH AX)
            val imm = if printable b
                      then SOME (acc // (PUSH_IMM ` I8 b))
                      else NONE
          in
            multistrategy [via_ax, imm]
          end
      in
        case vlo of
          NONE =>
            if ah = vh
            then acc
            else
              let val acc = pushbyte vh
              in
                acc //
                (* put trash at top of stack *)
                DEC SP //
                POP AX //
                INC SP ??
                (* ESP stays the same, but AL is lost because we don't
                   know what was on the stack. *)
                forget_slot M.EAX ---@ ??
                learn_slot M.EAX --@- vh
              end
          | SOME vl =>
            let val acc = pushbyte vh
            in
              acc //
              (* put VL VL at top of stack *)
              push16 (w8stow16 (vl, vl)) //
              (* discard second VL *)
              INC SP //
              POP AX //
              INC SP ??
              (* ESP stays the same. *)
              learn_slot M.EAX ---@ vl ??
              learn_slot M.EAX --@- vh
            end
      end

    (* Set AL to the value, without touching AH or anything else. *)
    fun imm_al acc vl : acc =
      case M.slot (mach acc) M.EAX ---@ of
        SOME al => imm_al_known acc al vl
      | NONE =>
          let
            val acc = acc //
              (AND_A_IMM ` I8 0wx40) //
              (AND_A_IMM ` I8 0wx20) ??
              learn_slot M.EAX ---@ 0w0
          in
            imm_al_known acc 0w0 vl
          end

    (* Set AX to the immediate value, knowing that it currently contains some
       value. *)
    fun imm_ax16_known (acc : acc) (a : Word16.word) (v : Word16.word) : acc =
      let
        val (ah, al) = w16tow8s a
        val (vh, vl) = w16tow8s v

        fun bytewise_hint () =
          let
            (* Hint must be printable. This covers the case where the
               target al is already printable. *)
            val alhint = Array.sub (best_printable_src,
                                    Word8.toInt al)
            val acc = imm_ah_known acc (ah, al) (SOME alhint) vh
            val acc = imm_al_known acc alhint vl
          in
            SOME acc
          end

        fun bytewise_blind () =
          let
            val acc = imm_ah_known acc (ah, al) NONE vh
            val acc = imm_al acc vl
          in
            SOME acc
          end

        (* PERF: This should consider multi-instruction sequences. *)

        fun xor () =
          let
            val xh = Word8.xorb (ah, vh)
            val xl = Word8.xorb (al, vl)
          in
            if printable xh andalso printable xl
            then
              let
                val xx = w8stow16 (xh, xl)
              in
                SOME (acc // (XOR_A_IMM ` I16 xx) ?? learn_reg16 M.EAX v)
              end
            else NONE
          end

         fun sub () =
           let
             val s = Word16.-(a, v)
             val (sh, sl) = w16tow8s s
           in
             if printable sh andalso printable sl
             then SOME (acc // (SUB_A_IMM ` I16 s) ?? learn_reg16 M.EAX v)
             else NONE
           end

         fun and16 () =
           case inverse_and (ah, vh) of
             NONE => NONE
           | SOME ch =>
               (case inverse_and (al, vl) of
                  NONE => NONE
                | SOME cl =>
                    SOME (acc // (AND_A_IMM ` I16 ` w8stow16 (ch, cl)) ??
                          learn_reg16 M.EAX v))

        fun incdec () =
          let
            val inc_distance = Word16.toInt ` Word16.-(v, a)
            val dec_distance = Word16.toInt ` Word16.-(a, v)

            val _ = (inc_distance >= 0 andalso dec_distance >= 0)
              orelse raise Tactics "impossible!"

            (* Just time/quality tradeoff since we check for the shortest
               sequence below. *)
            val MAX_DISTANCE = 10
          in
            (* inc/dec_distance 0 covers the case that they are equal. *)
            if inc_distance <= MAX_DISTANCE
            then SOME
              (repeated inc_distance acc (INC AX) ?? learn_reg16 M.EAX v)
            else if dec_distance <= MAX_DISTANCE
                 then SOME
                   (repeated dec_distance acc (DEC AX) ?? learn_reg16 M.EAX v)
                 else NONE
          end
      in
        multistrategy [incdec (), xor (), sub (), and16 (),
                       bytewise_hint (), bytewise_blind ()]
      end
  in
    (* TODO: Easy to set any reg to 32-bit value by doing two imm_ax16s
       (the second with a known value), and pushing them on the stack,
       then popping into a 32-bit register. (Or four single-byte loads?) *)

    (* Set AX to the 16-bit value.
       The register should already be claimed by the caller. *)
    fun imm_ax16 acc (w : Word16.word) : acc =
      let
        val () = assert_claimed acc AX

        val known =
          case M.reg16 (mach acc) M.EAX of
            SOME ax => SOME ` imm_ax16_known acc ax w
          | NONE => NONE

        (* PERF: Case where we know one of AH or AL, just not the other one *)

        (* PERF: Delete it? two ANDs is 6 bytes, and via_20 is 3.
           Would probably be better to spend the cycles trying to
           find a better starting value. *)
        val via_zero =
          let
            val acc = acc //
              (AND_A_IMM ` I16 ` Word16.fromInt 0x4040) //
              (AND_A_IMM ` I16 ` Word16.fromInt 0x2020) ??
              learn_reg16 M.EAX (Word16.fromInt 0)
          in
            SOME ` imm_ax16_known acc (Word16.fromInt 0) w
          end

        (* Loading a single byte into AX is only 3 bytes,
           but it must be printable! *)
        val via_20 =
          let
            val acc = acc //
              PUSH_IMM ` I8 0wx20 //
              POP AX ??
              learn_reg16 M.EAX (Word16.fromInt 0x20)
          in
            SOME ` imm_ax16_known acc (Word16.fromInt 0x20) w
          end

        (* PERF should consider loading values that are close
           (via xor, sub, inc, dec, and) to the one we want. *)
        val (wh, wl) = w16tow8s w
        val best_lsrc = Array.sub (best_printable_src,
                                   Word8.toInt wl)
        val pushpop =
          (* We can push an 8- or 16-bit printable immediate, then pop it into
             AX (4 bytes). If the high byte is printable (or zero), then we do
             that, and try to load the best printable src value for
             the low byte. This covers the case that the low byte is
             already printable.

             PERF: If we are only pushing one byte, a good precursor to
             WH might be the right choice, since we'll often do the multibyte
             load which starts with a load of ah. *)
          if printable wh orelse wh = 0w0
          then
            let
              val wp = w8stow16 (wh, best_lsrc)
              val acc = acc //
                push16 wp // POP AX ??
                learn_reg16 M.EAX wp
            in
              (* Now try to load from this known value. *)
              SOME ` imm_ax16_known acc wp w
            end
          else NONE

      in
        multistrategy [known, via_zero, via_20, pushpop]
      end handle e => raise e

  end

  fun reg_to_machreg r =
    case r of
      A => M.EAX
    | C => M.ECX
    | D => M.EDX
    | B => M.EBX
    | AH_SP => M.ESP
    | CH_BP => M.EBP
    | DH_SI => M.ESI
    | BH_DI => M.EDI

  (* Set a 16-bit register to the specified value.
     Must claim the target register and AX before calling.
     May use stack but restores it.
     Can trash flags.
     Modifies AX. *)
  fun imm_reg16 (acc : acc) (r : reg) (v : Word16.word) : acc =
    let
      fun general () =
        let
          val via_a =
            case r of
              A => SOME ` imm_ax16 acc v
            | _ => SOME (imm_ax16 acc v //
                         PUSH AX //
                         POP (reg_to_multireg16 r) ??
                         learn_reg16 (reg_to_machreg r) v)
          val (vh, vl) = w16tow8s v
          val imm =
            if printable vh andalso printable vl
            then SOME (acc //
                       PUSH_IMM ` I16 v //
                       POP (reg_to_multireg16 r) ??
                       learn_reg16 (reg_to_machreg r) v)
            else NONE
        in
          multistrategy [via_a, imm]
        end
    in
      assert_claimed acc (reg_to_multireg16 r);
      assert_claimed acc AX;

      (* PERF see if we have the value in any reg, then PUSH/POP *)
      (* PERF use temporaries *)
      case M.reg16 (mach acc) ` reg_to_machreg r of
        (* just in case we already have it... *)
        SOME oldv => if v = oldv then acc else general ()
      | _ => general ()
    end handle e => raise e

  fun check_printable_modrm (reg, modrm) =
    let
      fun assert w =
        if w >= 0wx20 andalso w <= 0wx7e then ()
        else raise Tactics "disp8 is non-printable, contrary to assertion"
      fun checkreg () =
        case reg of
          AH_SP => ()
        | CH_BP => ()
        | DH_SI => ()
        | BH_DI => ()
        | _ => raise Tactics ("in [reg] mod/rm, only certain other " ^
                              "registers result in printable instructions; " ^
                              "got " ^ regtos reg)
    in
        case modrm of
          IND_EAX_DISP8 w => assert w
        | IND_ECX_DISP8 w => assert w
        | IND_EDX_DISP8 w => assert w
        | IND_EBX_DISP8 w => assert w
        | IND_SIB_DISP8 _ => raise Tactics "unimplemented"
        | IND_EBP_DISP8 w => assert w
        | IND_ESI_DISP8 w => assert w
        | IND_EDI_DISP8 w => assert w

        | IND_EAX => checkreg ()
        | IND_ECX => checkreg ()
        | IND_EDX => checkreg ()
        | IND_EBX => checkreg ()
        | IND_SIB sib => raise Tactics "unimplemented"
        | DISP32 w32 => raise Tactics "unimplemented"
        | IND_ESI => checkreg ()
        | IND_EDI => checkreg ()

        | IND_SI => checkreg ()
        | IND_DI => checkreg ()

        (* This is not necessarily a bug; we might just need to add
           support for the mod/rm type here. *)
        | _ => raise Tactics ("got mod/rm that isn't [REG]+disp8 " ^
                              "when checking for printable mod/rm...")
    end

  fun check_not_stack16 AH_SP =
    raise Tactics ("can't use SP (probably because the tactic " ^
                   "needs to use the stack)")
    | check_not_stack16 _ = ()

  (* Load the register with the immediate value.
     Only need to claim the target register; may trash AX if unclaimed.
     *)
  fun imm_reg16only acc A v = imm_ax16 acc v
    | imm_reg16only acc r v =
    save_and_claim acc AX
    (fn acc =>
     let in
       check_not_stack16 r;
       (* PERF -- we don't need to save AX if value is printable. *)
       imm_reg16 acc r v
       end)

  (* XXXXXX PERF!  I think I can use IND_BP_DISP8 and avoid the
     prefix byte? *)
  (* mod/rm macro that generates IND_EBP_DISP8 as though it starts at 0,
     and checks that the result is printable. *)
  fun EBP_TEMPORARY (tmp : int) =
    if tmp < 0 then raise Tactics ("Illegal negative temporary offset? " ^
                                   Int.toString tmp)
    else
      let val real_offset = tmp + 0x20
      in
        if real_offset > 0x7e
        then raise Tactics ("Ut oh, temporary offset out of range: " ^
                            Int.toString tmp ^
                            " yields real offset of " ^
                            Int.toString real_offset)
        else ();
        IND_EBP_DISP8 (Word8.fromInt real_offset)
      end

  (* As if the MOV instruction with [REG]+disp8 addressing.
     We first do AND with the destination to make it zero.
     We then XOR the destination with the source.

     Note that it does not work to do something like DI <- [DI], even
     if this is a legal printable addressing mode. It works in an
     atomic x86 MOV instruction, but since we perform multiple steps,
     the address in the register gets overwritten before we can
     perform the load.
     (XXX reject calls that try to do this!)

     Avoid using reg=A, since we then need to save and restore it.

     Uses the stack temporarily and trashes flags, but nothing else
     is modified. *)
  (* PERF in both cases, if we can find a register that already contains 0,
     use it instead of loading that into A *)
  fun mov16ind8 acc (modrm <~ reg) : acc =
    let in
      check_printable_modrm (reg, modrm);
      check_not_stack16 reg;
      assert_claimed acc (reg_to_multireg16 reg);
      save_and_claim acc AX
      (fn acc =>
       imm_ax16 acc (Word16.fromInt 0) //
       AND (S16, modrm <~ A)) //
      XOR (S16, modrm <~ reg)
      (* XXX this should track the value of the temporary too. *)
    end

   | mov16ind8 acc (reg <- modrm) =
    let in
      check_printable_modrm (reg, modrm);
      check_not_stack16 reg;
      assert_claimed acc (reg_to_multireg16 reg);

      imm_reg16only acc reg (Word16.fromInt 0) //
      XOR (S16, reg <- modrm) ??
      forget_reg16 (reg_to_machreg reg)
      (* XXX this should track the value of the temporary too. *)
    end handle e => raise e

  fun mov_tmp16_to_tmp16 acc dst src : acc =
    (* This would NOT work if dst and src are the same,
       but fortunately we can just do nothing in that case. *)
    if dst = src then acc
    else
    (* PERF if we already have 0 in an unclaimed register,
       we can use that. *)
    save_and_claim acc AX
    (fn acc =>
     (* First, zero the destination. *)
     imm_ax16 acc (Word16.fromInt 0) //
     AND (S16, EBP_TEMPORARY dst <~ A) //
     (* A is still zero; replace it with src. *)
     XOR (S16, A <- EBP_TEMPORARY src) ??
     forget_reg16 M.EAX //
     (* Now write to dst. *)
     XOR (S16, EBP_TEMPORARY dst <~ A))

  (* PERF: Should try to select a register that has 0 in it
     already. Blocks start knowing that SI is 0, for example.

     XXX! PERF: If we claim A, then do we end up having to save the
     current value of A (useless) in order to mov16ind8 into A, and
     then later restore it? *)
  fun put_tmp_in_reg16 (acc : Acc.acc) (tmp : int) (k : acc * reg -> acc) =
    claim_reg16 acc [A, C, D, DH_SI, BH_DI]
    (fn (acc, tmpreg) =>
     let
       val acc =
         mov16ind8 acc (tmpreg <- EBP_TEMPORARY tmp) ??
         forget_reg16 (reg_to_machreg tmpreg)
     in
       k (acc, tmpreg)
     end)

  fun push_tmp16 acc (tmp : int) =
    put_tmp_in_reg16 acc tmp
    (fn (acc : acc, tmpreg : reg) =>
     acc //
     PUSH (reg_to_multireg16 tmpreg))

  fun pop_tmp16 acc (tmp : int) =
    claim_reg16 acc [C, D, DH_SI, BH_DI, A]
    (fn (acc, tmpreg) =>
     let
       val acc = acc //
         POP (reg_to_multireg16 tmpreg) ??
         forget_reg16 (reg_to_machreg tmpreg)
     in
       mov16ind8 acc (EBP_TEMPORARY tmp <~ tmpreg)
     end)

  (* Load the temporary at the given offset with the given constant. *)
  (* PERF: rather than loading into ax, then pushing, then zeroing
     ax to clear the destination, then popping, then xoring,
      - zero ax
      - clear destination
      - imm ax
      - xor destination

     (Or do multistrategy on those two?) *)
  fun imm_tmp16 acc (tmp : int) (v : Word16.word) : acc =
    if v = Word16.fromInt 0
    then
      (* If storing zero, we don't need to both AND and XOR;
         we can just clear with AND. *)
      let val acc = acc ++ AX
      in
        imm_ax16 acc (Word16.fromInt 0) //
        AND (S16, EBP_TEMPORARY tmp <~ A) -- AX
      end
    else
      (* General case *)
      claim_reg16 acc [A, C, D, DH_SI, BH_DI]
      (fn (acc, tmpreg) =>
       let val acc = imm_reg16only acc tmpreg v
       in mov16ind8 acc (EBP_TEMPORARY tmp <~ tmpreg)
       end)

      (*
  (* Binary NOT of register r. *)
  fun complement_reg16 acc r : acc =
  (* Strategy here is to do R <- XOR(R, OxFFFF).
     Generating FFFF is pretty cheap (0 - 1). We have to put it in a temporary
     in order to do it; we'll use [EBX]+0x20. *)
    let in
      assert_claimed acc (reg_to_multireg16 r);
      claim_reg16 acc [C, D, DH_SI, BH_DI, A]
      (fn (acc, tmpr) =>
       let
         val acc = imm_reg16only acc tmpr (Word16.fromInt 0xFFFF)
       in
         mov16ind8 acc (IND_EBX_DISP8 0wx20 <~ tmpr)
       end) //
      XOR (S16, r <- IND_EBX_DISP8 0wx20) ??
      forget_reg16 (reg_to_machreg r)
    end handle e => raise e
*)

  (* TODO: If this is not allowed then it should be explicitly banned!
     Many ops can easily support the case where operands are the same;
     e.g. in xor and sub it just becomes a imm16 of 0. *)
  fun assert_neq msg a b =
    if a <> b then ()
    else raise Tactics ("Tactic " ^ msg ^ " not certified for the case " ^
                        "where input temporaries are the same slot!")

  local
    (* Subtract the word w from the 16-bit register.
       Works for BP; this is used among other things to shift
       the temporary frame. *)
    (* PERF: We could also use SUB_IMM if it's a printable size. *)
    fun raw_sub_reg16 reg acc tfs w =
      if w = Word16.fromInt 0
      then acc
      else
        let
          (* Use the first slot after the temp_frame *)
          val slot = tfs
        in
          assert_claimed acc EBP;
          (* PERF: If we happen to have w in a temporary,
             just get that temporary! *)
          imm_tmp16 acc slot w //
          SUB (S16, CH_BP <- EBP_TEMPORARY slot) ??
          forget_reg16 M.EBP
        end
  in
    fun add_reg16_imm reg acc tfs n =
      let
      (* As with the general-purpose add tactic, we'll actually subtract.
         This is simpler because n is a constant. *)
        val startbytes = Acc.insbytes acc
        val raw = raw_sub_reg16 reg acc tfs (Word16.~ n)
        val rawbytes = Acc.insbytes raw - startbytes
        val intn = Word16.toInt n
      in
        if intn < rawbytes
        then
          repeated intn acc (INC ` reg_to_multireg16 reg) ??
            forget_reg16 (reg_to_machreg reg)
        else raw
      end

    (* PERF: If we are adding or subtracting very large words
       (e.g., negative ints), then doing INC here and DEC above could
       be faster. *)
    fun sub_reg16_imm reg acc tfs n =
      let
        val startbytes = Acc.insbytes acc
        val raw = raw_sub_reg16 reg acc tfs n
        val rawbytes = Acc.insbytes raw - startbytes
        val intn = Word16.toInt n
      in
        if intn < rawbytes
        then
          repeated intn acc (DEC ` reg_to_multireg16 reg) ??
            forget_reg16 (reg_to_machreg reg)
        else
          raw
      end

    (* Add a constant to BP. We have to be a little careful because BP is
       used as the base pointer for the temporaries themselves. *)
    val add_bp = add_reg16_imm CH_BP
    val sub_bp = sub_reg16_imm CH_BP
    val add_bx = add_reg16_imm B
    val sub_bx = sub_reg16_imm B
  end

  fun xor_tmp16 acc dst_tmp src_tmp =
    let in
      (* Fine if both arguments are the same; we read src into
         a register before doing anything else. *)
      put_tmp_in_reg16 acc src_tmp
      (fn (acc, tmpreg) =>
       acc //
       XOR (S16, EBP_TEMPORARY dst_tmp <~ tmpreg))
    end

  fun and_tmp16 acc dst_tmp src_tmp =
    let in
      (* Fine if both arguments are the same; we read src into
         a register before doing anything else. *)
      put_tmp_in_reg16 acc src_tmp
      (fn (acc, tmpreg) =>
       acc //
       AND (S16, EBP_TEMPORARY dst_tmp <~ tmpreg))
    end

  fun sub_tmp16 acc dst_tmp src_tmp =
    let in
      (* Fine if both arguments are the same; we read src into
         a register before doing anything else. *)
      put_tmp_in_reg16 acc src_tmp
      (fn (acc, tmpreg) =>
       acc //
       SUB (S16, EBP_TEMPORARY dst_tmp <~ tmpreg))
    end

  fun sub_tmp16_lit acc tmp w =
    (* PERF choose a register that can efficiently load
       this temporary *)
    save_and_claim acc AX
    (fn acc =>
     imm_ax16 acc w //
     SUB (S16, EBP_TEMPORARY tmp <~ A))

  fun add_tmp16 acc dst_tmp src_tmp : acc =
    (* reg <- 0xFFFF
       reg ^= src_tmp   (reg = ~src)
       inc reg          (reg = 0 - src)
       sub (dst, reg)   (dst -= -src, i.e., dst += src)
       *)
    let
      val acc = acc ++ AX
    in
      assert_neq "add" dst_tmp src_tmp;
      imm_ax16 acc (Word16.fromInt 0xFFFF) //
      XOR (S16, A <- EBP_TEMPORARY src_tmp) ??
      forget_reg16 M.EAX //
      INC AX ??
      forget_reg16 M.EAX //
      SUB (S16, EBP_TEMPORARY dst_tmp <~ A) -- AX
    end

  fun load16 acc dst_tmp addr_tmp : Acc.acc =
    if dst_tmp <> addr_tmp
    then
      let
        (* We can only do a pure [reg] indirect for certain
           registers, like DI. So we just use that one
           unconditionally here. *)
        val acc = acc ++ DI ++ SI
        (* PERF: When we do a sequence of moves like this, we
           can probably optimize it to e.g. zero the two destinations
           first (sharing some zeroing code), then follow with XORs?
           (be careful about the case that the temporaries alias!) *)
        val acc = mov16ind8 acc (BH_DI <- EBP_TEMPORARY addr_tmp)
        (* BH_DI <- IND_DI would work here, except that
           mov16ind8 doesn't work correctly if the registers
           interfere (it takes multiple steps). *)
        val acc = mov16ind8 acc (DH_SI <- IND_DI) ?? forget_reg16 M.ESI
        val acc = mov16ind8 acc (EBP_TEMPORARY dst_tmp <~ DH_SI)
      in
        acc -- SI -- DI
      end
    else
       (* The code above works when the temporaries are the
          same, but we can be a bit smarter in this case. *)
      let
        val one_tmp = dst_tmp
        val acc = acc ++ AX ++ DI

        (* This is frankly totally sweet. *)
        val acc = imm_ax16 acc (Word16.fromInt 0) //
          (* First, zero DI. *)
          PUSH AX // POP DI ?? learn_reg16 M.EDI (Word16.fromInt 0) //
          (* Now, read the address to load.
             tmp contains addr, DI contains addr. *)
          XOR (S16, BH_DI <- EBP_TEMPORARY one_tmp) ?? forget_reg16 M.EDI //
          (* Now, fetch the contents of the address, and XOR it
             with DI. (I wager this is a very rare opcode in the
             world.) *)
          XOR (S16, BH_DI <- IND_DI) ?? forget_reg16 M.EDI //
          (* DI contains (addr ^ contents); a totally weird value.
             We can get the temporary to contain 'contents' as
             desired with one more xor, as it has the 'decryption key'. *)
          XOR (S16, EBP_TEMPORARY one_tmp <~ BH_DI)
      in
        acc -- DI -- AX
      end

  (* Note that both temporaries hold 16-bit values, even though
     the load is of a single byte.

     Other idea: just do a 16-bit load but then mask off the upper bits?
     Like, xor once to get HI LO (where we want 00 LO). Then AND to
     get HI 00. Then XOR again to get 00 LO. Would be fast if we could
     load into AX (since we have AND_A_IMM) but unfortunately we can
     only target stuff like DI. Subtracting the address first to use
     a +disp8 won't work either, since the address could be < 0x20 to
     begin with...
     *)
  fun load8 acc dst_tmp addr_tmp : Acc.acc =
    let
      (* OK for addr and dst to be the same, because once we read
         addr we're done with it. *)
      (* We can only do a pure [reg] indirect for certain
         registers, like DI. So we just use that one
         unconditionally here. *)
      val acc = acc ++ DI ++ DX ++ AX
      val acc = imm_ax16 acc (Word16.fromInt 0) //
        PUSH AX // POP DI ?? learn_reg16 M.EDI (Word16.fromInt 0) //
        PUSH AX // POP DX ?? learn_reg16 M.EDX (Word16.fromInt 0) //
        (* Read 16-bit address. *)
        XOR (S16, BH_DI <- EBP_TEMPORARY addr_tmp) ?? forget_reg16 M.EDI //
        (* Destination register unfortunately needs to be
           AH, CH, DH, or BH with printable 8-bit loads. This isn't
           a big deal because we can do an 8-bit write to the temporary
           from any register, including DH. *)
        XOR (S8, DH_SI <- IND_DI) ?? forget_reg16 M.EDX //
        (* Zero the destination. *)
        AND (S16, EBP_TEMPORARY dst_tmp <~ A) -- AX //
        XOR (S8, EBP_TEMPORARY dst_tmp <~ DH_SI)
    in
      acc -- DX -- DI
    end

  fun store16 acc dst_addr src_tmp : Acc.acc =
    let
      (* OK when dst = src. We read each one to a register
         separately (and don't modify them).
         PERF: Is it possible to use one register in that case?
         Maybe. But such code should be pretty rare (in C, a = &a). *)
      val acc = acc ++ SI ++ DI ++ AX
      val acc = imm_ax16 acc (Word16.fromInt 0) //
        PUSH AX // POP SI ?? learn_reg16 M.ESI (Word16.fromInt 0) //
        PUSH AX // POP DI ?? learn_reg16 M.EDI (Word16.fromInt 0) -- AX //
        (* Load dest addr first. *)
        XOR (S16, DH_SI <- EBP_TEMPORARY dst_addr) ?? forget_reg16 M.ESI
      val () = assert_reg16 acc M.EDI (Word16.fromInt 0)

      (* Note: The glorious XOR hack from above does work here, but
         doesn't save us any bytes or registers. (XOR XOR XOR instead
         of AND XOR XOR.) *)
      val acc = acc //
        (* Zero destination. *)
        AND (S16, IND_SI <~ BH_DI) //
        (* Now read the value to store. *)
        XOR (S16, BH_DI <- EBP_TEMPORARY src_tmp) ?? forget_reg16 M.EDI //
        (* And finally, write it. *)
        XOR (S16, IND_SI <~ BH_DI)
    in
      acc -- DI -- SI
    end

  fun store8 acc dst_addr src_tmp : Acc.acc =
    let
      val acc = acc ++ DI ++ DX ++ AX
      val acc = imm_ax16 acc (Word16.fromInt 0) //
        PUSH AX // POP DI ?? learn_reg16 M.EDI (Word16.fromInt 0) //
        PUSH AX // POP DX ?? learn_reg16 M.EDX (Word16.fromInt 0) -- AX //
        (* Load dest addr first. *)
        XOR (S16, BH_DI <- EBP_TEMPORARY dst_addr) ?? forget_reg16 M.EDI
      val acc = acc //
        (* Zero destination byte. *)
        AND (S8, IND_DI <~ DH_SI) //
        (* Now read the value to store. The temporary is 16 bits in size, but
           we only care about its low 8 bits. If we do an 8-bit load,
           we get the low byte (little-endian), as desired. Put it in
           DH. *)
        XOR (S8, DH_SI <- EBP_TEMPORARY src_tmp) ?? forget_reg16 M.EDX //
        (* Now write it to the dest addr. *)
        XOR (S8, IND_DI <~ DH_SI)
    in
      acc -- DX -- DI
    end

  (* This sets up the initial invariants.

     (Note that some speculation in this comment is resolved in the header
      of asm.sml)

     - EBX = 0x00000100. We don't have any register-to-register operations,
       but we can do some indirect addressing, for example
          MOV ESI <- [EBX]
       or
          MOV ESI <- [EBX]+'a'     (of course note we do not have MOV)

       EBX is a fairly useless register, since it can't be the REG
       argument except when using [REG]+disp32 as R/M, and all
       registers can be used that way. By keeping it a constant value,
       we can use the following slots as temporaries:

          [EBX], i.e. DS:00000100 (available registers are ESP, EBP, ESI, EDI)
          [EBX]+0x20, i.e. DS:00000120 (all registers available)
          ...
          [EBX]+0x7e, i.e. DS:0000017e (all registers available)

       0 would be an obvious choice for the constant value, but DS:0000
       contains 256 bytes of the PSP, which we want to keep:
          - it contains the command line and maybe some other useful stuff
            from the program's environment.
          - it's guaranteed to contain INT 21; RETF (at DS:0050), which we could
            use to do system calls without self-modifying code if we could get
            the right stuff on the stack and somehow get control here (it's in
            range of a JNO 0x7e at the end of the address space, so this seems
            possible.)

       The base value basically doesn't matter. I think everything
       here works fine with different values TEMP_START (up to 0xFFFF
       - 0x7E). We may need to preserve other parts of the data
       segment, though, such as the region right before the address
       space wraps.

       This is lots of temporaries, so that's nice. It's also critical
       because it's the only way we can do math ops like XOR on
       non-immediate values. Example:
          Get DS:0100 to contain V2
          PUSH AX
          POP BP
          XOR BP <- [EBX]
          PUSH BP
          POP AX
       or
          Get DS:0120 to contain V2
          XOR AX <- [EBX]+0x20

       Note that we also have disp32 available, but this is basically useless in
       real mode because all printable values will overflow the segment.
       *)
  val IVT_INT_21 = 0x21 * 4
  val IVT_INT_ILLEGAL = 0x06 * 4

  (* At program start, set up some important state:
      - Illegal instruction handler so that we can eventually exit
      - EBP points to start of temporary frame in SS
      - EBX points to the start of the local frame in DS

     When this returns, EBP and EBX will be claimed. *)
  fun initialize (acc, init_ebp : Word16.word, init_ebx : Word16.word) : acc =
    let
      val acc = acc ++ EAX
      (* Overwrite interrupt vector table so that 'illegal instruction'
         is actually DOS INT 21h. *)

      (* Clear some registers. *)
      val acc = acc //
        AND_A_IMM ` I32 0wx40204020 //
        AND_A_IMM ` I32 0wx20402040 ??
        learn_reg32 M.EAX 0w0 //
        (* Zero EBX *)
        PUSH EAX ++ EBX //
        POP EBX ??
        learn_reg32 M.EBX 0w0 //
        (* Zero ESI *)
        PUSH EAX ++ ESI //
        POP ESI ??
        learn_reg32 M.ESI 0w0 //
        (* Zero EDI *)
        PUSH EAX ++ EDI //
        POP EDI ??
        learn_reg32 M.EDI 0w0

      val acc = imm_reg16 acc B (Word16.fromInt IVT_INT_21)

      (* Read int21 handler into ESI. It contains 0 so we can XOR into it. *)
      val () = assert_reg32 acc M.ESI 0w0
      val acc = acc //
        (* Access segment FS=0 *)
        DB 0wx64 // XOR (S32, DH_SI <- IND_BX) ??
        forget_reg32 M.ESI

      val acc = imm_reg16 acc B (Word16.fromInt IVT_INT_ILLEGAL)
      (* Now overwrite the illegal instruction handler. *)
      (* EDI has zero, so we can first clear the field. *)
      val () = assert_reg32 acc M.EDI 0w0
      val acc = acc //
        (* AND with 0 to clear the field. *)
        (* Access segment FS=0 *)
        DB 0wx64 // AND (S32, IND_BX <~ BH_DI) //
        (* XOR with existing 0 yields the desired value. *)
        (* Access segment FS=0 *)
        DB 0wx64 // XOR (S32, IND_BX <~ DH_SI)

      val acc = acc -- ESI -- EBX

      (* EDI still has zero. *)
      val () = assert_reg32 acc M.EDI 0w0
      (* Now initialize EBX and EBP. *)
      (* Push 32-bit zero; we'll use this for the high word (16-bit) of
         EBX then EBP. *)
      val acc = acc // PUSH EDI -- EDI

      val acc = imm_ax16 acc init_ebx
      (* EBX stays claimed always.
         XXX actually maybe it should have "locked" state, so that we can't even
         save_and_claim. *)
      val acc = acc ++ EBX
      val acc = acc // PUSH AX // POP EBX ??
        learn_reg32 M.EBX (Word32.fromInt ` Word16.toInt init_ebx)

      val acc = imm_ax16 acc init_ebp
      val acc = acc ++ EBP
      val acc = acc // PUSH AX // POP EBP ??
        learn_reg32 M.EBP (Word32.fromInt ` Word16.toInt init_ebp)

      val acc = acc -- EAX
    in
      acc
    end

  (* This is an old one for testing and manual invocations.
     XXX remove it... *)
  fun old_initialize () : acc =
    let
      val TEMP_START = Word16.fromInt 0x0100
    in
      initialize (empty (X86.CTX { default_32 = false }) M.all_unknown,
                  Word16.fromInt 0x0,
                  TEMP_START)
    end

  (* Put an individual character (in 16-bit temporary at offset t)
     to the console. Uses interrupt instruction, so non-ASCII. *)
  fun putc16 acc tmp : acc =
    let
      val acc = acc ++ DX
      val acc = mov16ind8 acc (D <- EBP_TEMPORARY tmp) ??
        forget_reg16 M.EDX ++
        AX
      (* We don't care about the value in AL, so just use something
         printable.. *)
      val acc = imm_ax16 acc (Word16.fromInt 0x0620)
    in
      acc // INT 0wx21 ??
      forget_reg16 M.EDX ??
      forget_reg16 M.EAX -- AX -- DX
    end

  (* This one is a bit obnoxious because have to put the byte
     in memory at DS:SI first. I guess we can just use SI=0,
     which is byte 0 of the PSP? It might be good to just reserve
     some space in DS for this kind of thing. *)
  fun out8_16 acc port byte : acc =
    let
      (* Works fine if port and byte are the same temporary
         (both are just read). *)
      val acc = acc ++ AX ++ DI ++ SI ++ DX
      val acc = imm_ax16 acc (Word16.fromInt 0) //
        PUSH AX // PUSH AX // PUSH AX -- AX //
        POP SI ?? learn_reg16 M.ESI (Word16.fromInt 0) //
        POP DI ?? learn_reg16 M.EDI (Word16.fromInt 0) //
        POP DX ?? learn_reg16 M.EDX (Word16.fromInt 0) //
        (* zero DS:SI. Note we could even use SI as the source
           here, but only because we know it's zero! *)
        AND (S16, IND_SI <~ BH_DI) //
        (* Load DI with the byte we want to write. *)
        XOR (S16, BH_DI <- EBP_TEMPORARY byte) ?? forget_reg16 M.EDI //
        (* And write it to SI. Note that this places the low order
           byte at 0000, which is what we want. *)
        XOR (S16, IND_SI <~ BH_DI) //
        (* We also need the port in DX. *)
        XOR (S16, D <- EBP_TEMPORARY port) ?? forget_reg16 M.EDX //
        (* And finally, do the actual output. *)
        OUTS S8
    in
      acc -- DX -- SI -- DI
    end

  (* Generate code that prints the string. Uses the interrupt instruction, so
     non-ASCII. *)
  fun printstring acc s : acc =
    let
      fun emit acc nil = acc
        | emit acc (c :: rest) =
        let
          (* Load AH=06, AL=char *)
          val value = Word16.fromInt (0x06 * 256 + ord c)
          val acc = acc ++ AX
          val acc = imm_ax16 acc value
          (* PERF we actually only need to set DL *)
          val acc = acc ++ DX
          val acc = acc // PUSH AX // POP DX ??
            learn_reg16 M.EDX value
          val acc =
            acc // INT 0wx21 ??
            (* illegal instruction *)
            (* acc // X86.DB 0wx63 // X86.DB 0wx2a ?? *)
            (* interrupt returns character written in AL.
               Not known whether it preserves DX? *)
            forget_reg16 M.EDX ??
            learn_reg16 M.EAX value -- AX -- DX
        in
          emit acc rest
        end
    in
      emit acc (explode s)
    end

  (* Exits the program. This is ASCII now; it requires the interrupt
     vector to have been modified in initialization. *)
  fun exit acc : acc =
    let val acc = acc ++ AX
    in
      (* This will execute INT 21. AH=0x4C is "Exit".
         Note that we can set AL to a status byte and that gets
         returned to the shell; would be useful to thread through the
         return value from main. *)
      imm_ax16 acc (Word16.fromInt 0x4c00) //
      (* "cya" is an illegal instruction; we never return *)
      X86.DB 0wx63 // X86.DB 0wx79 // X86.DB 0wx61 //
      (* follow with ! to be emphatic *)
      X86.DB 0wx21 -- AX
    end

end
