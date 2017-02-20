structure ToASM :> TOASM =
struct
  infixr 9 `
  fun a ` b = a b

  structure C = CIL
  structure A = ASM

  exception ToASM of string
  datatype sz = datatype ASM.sz

  (* Comes after PSP. Some of the PSP is useful, so we could
     alternately have like char _abc_psp[256] as a global,
     as long as we could ensure that it came first.
  *)
  val FIRST_GLOBAL_POS = 256

  fun printable8 (b : Word8.word) = b >= 0wx20 andalso b <= 0wx7e

  structure Segment :>
  sig
    (* Out-of-bounds writes and writes to locked regions throw
       an exception. *)
    exception Segment of string
    (* Always 64kb. *)
    type segment
    val empty : unit -> segment
    (* set_range seg start len f
       call f 0, f 1, ... f (len - 1) to compute the bytes
       to populate the segment, starting at start. *)
    val set_range : segment -> int -> int -> (int -> Word8.word) -> unit
    (* set_range seg start str
       Write the string to the segment at the start location. *)
    val set_string : segment -> int -> string -> unit
    (* set_vec seg start vec *)
    val set_vec : segment -> int -> Word8Vector.vector -> unit
    (* lock_range seg start len
       Locks the range so that an exception is raised if a write
       (or lock) to that region is attempted. *)
    val lock_range : segment -> int -> int -> unit
    (* Need not all be initialized. *)
    val extract : segment -> Word8Vector.vector
  end =
  struct
    exception Segment of string
    datatype segment =
      S of { bytes : Word8Array.array,
             locked : bool array }
    fun empty () =
      S { bytes = Word8Array.array (65536, Word8.fromInt ` ord #"_"),
          locked = Array.array (65536, false) }

    fun update (S { bytes, locked }) idx byte =
      if Array.sub (locked, idx)
      then raise Segment ("write to locked idx " ^ Int.toString idx)
      else Word8Array.update (bytes, idx, byte)

    fun set_range seg start len f =
      if start < 0 orelse start + len > 65536 orelse len < 0
      then raise Segment "set_range bad start/len"
      else
        Util.for 0 (len - 1)
        (fn i =>
         let val b = f i
         in
           update seg (start + i) b
         end)

    fun set_vec seg start vec =
      Util.for 0 (Word8Vector.length vec - 1)
      (fn i =>
       update seg (start + i) (Word8Vector.sub (vec, i)))

    fun set_string seg start str =
      Util.for 0 (size str - 1)
      (fn i =>
       update seg (start + i) (Word8.fromInt (ord (String.sub (str, i)))))

    fun lock_range (S { locked, ... }) start len =
      Util.for 0 (len - 1)
      (fn i =>
       if Array.sub (locked, start + i)
       then raise Segment ("tried to lock already-locked " ^
                           Int.toString (start + i))
       else Array.update (locked, start + i, true))

    fun extract (S { bytes, ... }) =
      Word8Vector.tabulate (65536, fn i => Word8Array.sub (bytes, i))
  end

  (* Optimizations TODO:
     - Reuse temporaries.
     - Replace locals with temporaries
        (Should this be done in CIL though? Can't do it for
         stuff that spans blocks there.)
        (Is it even faster, anyway? I guess it would cut
         down on moves between locals and "registers")
     - A handful of operators can use immediate values, at
       least printable ones. *)

  (* Notes on translation strategy:

     - The comparison operators are some trouble; we don't
       something like cmov, so we have to do a compare-
       and-jump in order to compute the values. (It would
       maybe be possible to subtract and check the sign
       bit manually, but it's pretty fiddly.) So one of the
       things that the CIL optimizer does is rewrite
       comparison expressions like "a < b" to the condition
       forms, like gotoif (a < b).

     - all temporaries are either 16 or 32 bits.
       many 8-bit operations are available to us, but
       it adds lots of complexity.

     *)


  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  fun szbytes S8 = 1
    | szbytes S16 = 2
    | szbytes S32 = 4

  (* Layout size of a CIL type (e.g. in a locals frame or struct).
     This is not necessarily the size of a temporary that holds
     such a value.

     Signedness doesn't matter any more -- the representation is
     the same and the operations are explicit. *)
  fun ctypsize (C.Word32 _) = S32
    | ctypsize (C.Word16 _) = S16
    | ctypsize (C.Word8 _) = S8
    (* All pointers are 16-bit offsets into DS. *)
    | ctypsize (C.Pointer _) = S16
    (* Labels in code segment, not addresses but no way for
       them to be bigger than 16 bits! *)
    | ctypsize (C.Code _) = S16
    (* Maybe this should already have been compiled away. *)
    | ctypsize (C.Struct _) = raise ToASM "unimplemented: structs"

  (* Accumulate all of the mentions of Locals into the map. *)
  structure GetSizesArg : CILPASSARG =
  struct
    type arg = C.typ SM.map ref
    structure CI = CILIdentity(type arg = arg)
    open CI
    fun case_AddressLiteral arg (selves, ctx) (loc, typ) =
      let val ret = CI.case_AddressLiteral arg (selves, ctx) (loc, typ)
      in
        case loc of
          C.Global _ => ret
        | C.Local s =>
            (case SM.find (!arg, s) of
               NONE =>
                 let in
                   arg := SM.insert (!arg, s, typ);
                   ret
                 end
             | SOME otyp =>
                 if C.eq_typ (typ, otyp)
                 then ret
                 else raise ToASM ("Local " ^ s ^
                                   " used at inconsistent types: " ^
                                   C.typtos typ ^ " and " ^ C.typtos otyp))
      end
  end
  structure GetSizes = CILPass(GetSizesArg)

  infix //
  fun cmd // (cmds, next) = (cmd :: cmds, next)

  local
    val ctr = ref 0
  in
    fun new_named_tmp (func, name, size) =
      let
        val x = !ctr
      in
        ctr := x + 1;
        (* XXX this is pretty bogus since we keep using $.
           Should either parse to remove $, or just have
           a symbol type. *)
        A.N { func = func, name = name ^ "$t" ^ Int.toString x, size = size }
      end
  end

  (* Generate the blocks for a CIL Function, once we know the locals
     layout. *)
  fun genblocks { name, offsets, argbytes, globalpositions,
                  localbytes, body, blocks } =
    let
      (* Blocks we've already translated, just for sanity checking. *)
      val done = ref SM.empty : unit SM.map ref
      (* Blocks left to translate. *)
      val todo = ref SM.empty : C.stmt SM.map ref
      val () = app (fn (lab, stmt) =>
                    todo := SM.insert (!todo, lab, stmt)) blocks

      fun newtmp (n, size) = new_named_tmp (name, n, size)
      fun vartmp (var, sz) =
        A.N { func = name, name = var, size = sz }

      (* Generate code to put the value v in a new temporary,
         then call the continuation with that temporary and its size. *)
      fun gentmp (ctx : C.context) (v : C.value)
        (k : A.named_tmp -> A.named_tmp A.cmd list * 'b) :
        A.named_tmp A.cmd list * 'b =
        case v of
          C.Var var =>
            (case C.Context.lookup (ctx, var) of
               NONE => raise ToASM ("Unbound variable " ^ var)
             | SOME ctyp =>
                 (* A variable is always translated to a same-named
                    temporary at the appropriate size. *)
                 k (vartmp (var, ctypsize ctyp)))
        (* typ is the type of the thing thing pointed to.
           we don't use it here; the address is always a 16-bit DS offset. *)
        | C.AddressLiteral (loc, typ) =>
           (case loc of
              C.Local l =>
                let val tmp = newtmp ("addr_" ^ l, A.S16)
                in
                  case ListUtil.Alist.find op= offsets l of
                    NONE => raise ToASM ("unallocated local " ^ l ^ "?")
                  | SOME pos =>
                      A.FrameOffset (tmp, Word16.fromInt pos) // k tmp
                end
            | C.Global l =>
                let val tmp = newtmp ("addr_" ^ l, A.S16)
                in
                   case ListUtil.Alist.find op= globalpositions l of
                     NONE => raise ToASM ("unallocated global " ^ l ^ "?")
                   | SOME pos =>
                       A.Immediate16 (tmp, Word16.fromInt pos) // k tmp
                end)
        | C.FunctionLiteral (name, ret, args) =>
            raise ToASM "unimplemented function literals"
        | C.Word8Literal w8 =>
            let val tmp = newtmp ("imm", A.S8)
            in A.Immediate8 (tmp, w8) // k tmp
            end
        | C.Word16Literal w16 =>
            let val tmp = newtmp ("imm", A.S16)
            in A.Immediate16 (tmp, w16) // k tmp
            end
        | C.Word32Literal w32 =>
            let val tmp = newtmp ("imm", A.S32)
            in A.Immediate32 (tmp, w32) // k tmp
            end
        | C.StringLiteral _ => raise ToASM "unimplemented string literals"

      (* Generate code for e, and bind vt (if SOME) to its value. *)
      fun genexp (ctx : C.context) (vt : (string * C.typ) option, e : C.exp)
        (k : unit -> A.named_tmp A.cmd list * 'b) :
        A.named_tmp A.cmd list * 'b =
        case (e, vt) of
          (C.Call (fv, argvs), vt) =>
            (* This is the only one that we actually need to emit if
               the result is unused (i.e. vt is NONE). *)
            raise ToASM "unimplemented call"
        | (_, NONE) => k ()
        | (C.Value value, SOME (var, t)) =>
            (* PERF, should probably avoid the mov in most of
               these cases. Maybe should be done as part of temporary
               allocation phase though. *)
            gentmp ctx value
            (fn tmp =>
             (* XXX Check compatibility of t and sz from tmp? *)
             let val A.N { size, ... } = tmp
             in A.Mov (vartmp (var, size), tmp) // k ()
             end)
        (* These should be compiled away into GotoIf (cond, ...) *)
        | (C.Greater _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.GreaterEq _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.Above _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.AboveEq _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.Less _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.LessEq _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.Below _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.BelowEq _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.Eq _, _) => raise ToASM "bug: unexpected comparison op"
        | (C.Neq _, _) => raise ToASM "bug: unexpected comparison op"

        | (C.Load (w, v), SOME (var, t)) =>
            gentmp ctx v
            (fn addr =>
             (case w of
                C.Width8 => A.Load8 (vartmp (var, ctypsize t), addr) // k ()
              | C.Width16 => A.Load16 (vartmp (var, ctypsize t), addr) // k ()
              | C.Width32 => raise ToASM "unimplemented 32-bit loads"))

            (*
        (* For dst < src. Just discards bits. *)
  | Truncate of { src: width, dst: width, v: value }
    (* For dst > src. Sign-extends if signed is true. *)
  | Promote of { signed: bool, src: width, dst: width, v: value }
    (* TODO: LoadImmediate *)
  | Plus of width * value * value
  | Minus of width * value * value
    (* Not clear that we support 8-bit times / div? *)
  | Times of width * value * value
  | SignedDivision of width * value * value
  | UnsignedDivision of width * value * value
    (* Also signed mod? *)
  | UnsignedMod of width * value * value
    (* For LeftShift and RightShift, shift amount should be word8. *)
  | LeftShift of width * value * value
    (* Recall that C does not define the behavior of shifts on negative values,
       so this is unsigned. *)
  | RightShift of width * value * value
    (* "Greater" and "Less" are signed.
       "Above" and "Below" are unsigned. *)
  (* These are all bitwise. && and || are compiled away. *)
  | And of width * value * value
  | Or of width * value * value
  | Xor of width * value * value
  | Not of width * value
  | Complement of width * value
  | Negate of width * value
*)
        | (_, SOME (var, t)) => raise ToASM "unimplemented many exps..."

      fun gencmds ctx stmt =
        case stmt of
          C.Bind (var, t, e, s) =>
            genexp ctx (SOME (var, t), e)
            (fn () =>
             let val ctx = C.Context.insert (ctx, var, t)
             in gencmds ctx s
             end)
        | C.Do (e, s) =>
            genexp ctx (NONE, e) (fn () => gencmds ctx s)
        | C.Store (width, addr, v, s) =>
            let val (cmds, next) = gencmds ctx s
            in
              case width of
                C.Width8 =>
                  gentmp ctx addr
                  (fn addrtmp =>
                   gentmp ctx v
                   (fn vtmp =>
                    (A.Store8 (addrtmp, vtmp) :: cmds, next)))
              | C.Width16 =>
                  gentmp ctx addr
                  (fn addrtmp =>
                   gentmp ctx v
                   (fn vtmp =>
                    (A.Store16 (addrtmp, vtmp) :: cmds, next)))
              | C.Width32 =>
                  (* Maybe just compile it as two 16-bit stores
                     for now? *)
                  raise ToASM "32-bit stores not implemented yet"
            end

        | C.GotoIf (cond, truelab, s) =>
            let val (cmds, next) = gencmds ctx s
            in
              case cond of
                C.CLess (w, a, b) =>
                  gentmp ctx a
                  (fn atmp =>
                   gentmp ctx b
                   (fn btmp =>
                    (A.JumpCond (A.Less (atmp, btmp), truelab) :: cmds,
                     next)))
              | C.CLessEq (w, a, b) =>
                  gentmp ctx a
                  (fn atmp =>
                   gentmp ctx b
                   (fn btmp =>
                    (A.JumpCond (A.LessEq (atmp, btmp), truelab) :: cmds,
                     next)))
              | C.CBelow (w, a, b) =>
                  gentmp ctx a
                  (fn atmp =>
                   gentmp ctx b
                   (fn btmp =>
                    (A.JumpCond (A.Below (atmp, btmp), truelab) :: cmds,
                     next)))
              | C.CBelowEq (w, a, b) =>
                  gentmp ctx a
                  (fn atmp =>
                   gentmp ctx b
                   (fn btmp =>
                    (A.JumpCond (A.BelowEq (atmp, btmp), truelab) :: cmds,
                     next)))
               (* PERF for these two, we should generate EqZero and
                  NeZero if one argument is a literal zero. *)
              | C.CEq (w, a, b) =>
                  gentmp ctx a
                  (fn atmp =>
                   gentmp ctx b
                   (fn btmp =>
                    (A.JumpCond (A.Eq (atmp, btmp), truelab) :: cmds,
                     next)))
              | C.CNeq (w, a, b) =>
                  gentmp ctx a
                  (fn atmp =>
                   gentmp ctx b
                   (fn btmp =>
                    (A.JumpCond (A.NotEq (atmp, btmp), truelab) :: cmds,
                     next)))
            end

        | C.Goto lab =>
            (case SM.find (!done, lab) of
               (* If we haven't yet processed the destination block,
                  just put it next and fall through. *)
               SOME () => (nil, SOME lab)
               (* Otherwise, an explicit jump and we don't have
                  a suggestion for the next block. *)
             | NONE => ([A.JumpCond (A.True, lab)], NONE))

        | C.Return v =>
            let
              (* FIXME where does the return value go? *)
              val ratmp = newtmp ("returnaddr", S16)
            in
              (* PERF can just merge these *)
              (A.ShrinkFrame localbytes ::
               A.ShrinkFrame argbytes ::
               (* XXX this is probably no good -- we've just
                  moved the base pointer so the allocation of
                  this temporary will not be right. Ideas:
                   - "Return" macro that keeps track of the
                     temporary or uses a register;
                   - Alternatively/Equivalently, JumpInd always takes
                     the address from the top of the stack (this might
                     be cheaper than you think because stack ops are
                     mostly 1 byte)
                   - Oh wait it's actually fine because temporaries
                     are not the same as "frame" (which is addressable
                     local variables)? This will become clear later..
                    *)
               A.Pop ratmp ::
               A.JumpInd ratmp ::
               nil,
               NONE)
            end
        | C.End =>
            (* Unreachable, so we can just fall off the end.
               XXX Actually I think this is used inconsistently,
               like for local variable initialization? *)
            (nil, NONE)

      fun donext () =
        case SM.headi (!todo) of
          NONE => nil
        | SOME (lab, _) => genblock lab

      (* Translate the named block first, then any remaining blocks.
         lab must be in the domain of the todo map. *)
      and genblock lab =
        let
          (* First move it from todo to done, in case of recursive
             references. *)
          val (ntodo, stmt) = SM.remove (!todo, lab)
          val () = todo := ntodo
          val () = done := SM.insert (!done, lab, ())
          val (cmds, next) = gencmds C.Context.empty stmt
        in
          (lab, cmds) ::
          (case next of
             NONE => donext ()
           | SOME lnext => genblock lnext)
        end

      val header_label = name ^ "$hdr"
      fun genall () =
        (* Need to set up local variable frame. Caller has already
           set up 'argbytes' bytes for us. *)
        (header_label, [A.ExpandFrame localbytes]) ::
        (* And then continue with the function's entry point. *)
        genblock body
    in
      genall ()
    end


  fun doproc globalpositions
             (name, C.Func
              { args : (string * C.typ) list,
                ret : C.typ,
                body : string,
                blocks : (string * C.stmt) list }) : A.named_tmp A.block list =
    let
      val argsizes : sz SM.map =
        foldl (fn ((s, t), m) => SM.insert (m, s, ctypsize t)) SM.empty args

      val localsizes =
        let
          val localtypes : C.typ SM.map ref = ref SM.empty
          fun oneblock (_, stmt) =
            ignore ` GetSizes.converts localtypes C.Context.empty stmt
        in
          app oneblock blocks;
          (* Exclude args from locals, since they are allocated as
             args. *)
          SM.filteri (fn (k, s) =>
                      not ` Option.isSome ` SM.find (argsizes, k)) `
          SM.map ctypsize ` !localtypes
        end

      (* Arguments and locals are not on the normal stack. They go in the
         data segment and are accessed through some base pointer (EDX?
         ESI?) which probably will be pointing 0x20 bytes before the
         frame. We do this because it's possible to take their
         address, and we represent pointers as 16-bit offsets into the
         data segment. *)
      val nextpos = ref 0
      (* We use a totally packed layout, ignoring the fact that bad
         alignment causes lost cycles. We are much more constrained
         by code/data size (64k) than clock speed (4+ GHz).
         PERF: Rearranging locals and args to get better alignment
         would be pretty easy, and free from code/data size perspective.
         Rearranging args would take a global analysis (note that it
         has to be based on the types, so that it works consistently for
         function pointers, and consider the possibility that someone takes
         the address of main), but would not be so bad. *)
      val offsets : (string * int) list ref = ref nil
      fun alloc (s, sz) =
        let in
          offsets := (s, !nextpos) :: !offsets;
          nextpos := !nextpos + szbytes sz
        end

      (* Args must come first *)
      val () = SM.appi alloc argsizes
      val argbytes = !nextpos
      val () = SM.appi alloc localsizes
      val localbytes = !nextpos - argbytes

      val blocks = genblocks { name = name, offsets = !offsets,
                               globalpositions = globalpositions,
                               argbytes = argbytes, localbytes = localbytes,
                               body = body, blocks = blocks }
    in
      print ("Frame for proc " ^ name ^ ":\n" ^
             String.concat (map (fn (s, off) => "  " ^ Int.toString off ^
                                 ": " ^ s ^ "\n") (rev (!offsets))));
      blocks
(*
      A.Proc { name = name,
               blocks = blocks,
               offsets = !offsets,
               argbytes = argbytes,
               localbytes = localbytes }
*)
    end

  (* Allocate globals at the beginning of DS so that we know their
     addresses. Doesn't perform initialization. *)
  fun allocglobals (datasegment, globals : (string * C.global) list) =
    let
      (* Unlike locals, we expect all of the globals to already be
         collected into the toplevel list. *)
      val nextpos = ref FIRST_GLOBAL_POS
      val globalpositions = ref nil : (string * int) list ref
      fun alloc (s, vec) =
        let val size = Word8Vector.length vec
        in
          Segment.set_vec datasegment (!nextpos) vec;
          Segment.lock_range datasegment (!nextpos) size;
          globalpositions := (s, !nextpos) :: !globalpositions;
          nextpos := !nextpos + size
        end
      fun oneglobal (name, C.Glob { typ, bytes, init }) =
        let
          val size = ctypsize typ
          val bytes =
            (case bytes of
               NONE => Word8Vector.tabulate (szbytes size,
                                             fn _ =>
                                             Word8.fromInt ` ord #"?")
             | SOME b => b)
        in
          if Word8Vector.all printable8 bytes
          then ()
          else raise ToASM ("Non-printable init bytes for global " ^
                            name);
          (case init of
             NONE => ()
           | SOME _ => raise ToASM ("Expected initialization of global " ^
                                    name ^ " to have been moved to pre-" ^
                                    "main init function by the optimizer."));
          alloc (name, bytes)
        end
    in
      app oneglobal globals;
      (!globalpositions, !nextpos)
    end

  fun toasm (C.Program { main, functions, globals }) =
    let
      val datasegment = Segment.empty ()
      val () = Segment.set_range datasegment 0 256 (fn _ =>
                                                    Word8.fromInt ` ord #".")
      val () = Segment.set_string datasegment 0
        ("[This area gets overwritten with the Program Segment Prefix. " ^
         "It's actually technically part of the header, but it would " ^
         "start at DS:0000 if it weren't overwritten. After these 256 " ^
         "bytes is where the program's compiled globals go.")
      val () = Segment.set_string datasegment 255 "]"
      val () = Segment.lock_range datasegment 0 256

      val (globalpositions, frame_stack_start) =
        allocglobals (datasegment, globals)
      (* XXX global initialization code!
         XXX something has to set up EBX to point to the frame stack,
         i.e., frame_stack_start *)
      fun onefunction (name, func) =
        (name, doproc globalpositions (name, func))

      val proc_blocks : (string * A.named_tmp A.block list) list =
        map onefunction functions
      (* Each proc is a list of blocks that have to go in that order,
         but the lists can be permuted however we like. We don't do
         any intraprocedural optimization. Pull out the "main" block
         so that it goes immediately after our initialization block,
         which is first. *)
      val blocks =
        case ListUtil.Alist.extract op= proc_blocks main of
          NONE => raise ToASM ("there is no main function " ^ main ^ "?")
        | SOME (mproc : A.named_tmp A.block list, rest) =>
            let
              (* Flatten the blocks. *)
              val rest = List.concat ` map #2 rest
            in
              (* Put main's code first. *)
              mproc @ rest
            end

      (* XXX generate fresh label from something... *)
      val init_block = ("__abc_init", [A.Init])
    in
      (* Probably should include some headroom here..? *)
      if frame_stack_start > 65536
      then raise ToASM ("Size of globals statically exceeds the 16-bit " ^
                        "data-segment. This program can't be compiled " ^
                        "for this architecture! :(")
      else ();

      A.Program { blocks = init_block :: blocks,
                  datasegment = Segment.extract datasegment }
    end
  handle Segment.Segment s => raise ToASM ("Segment: " ^ s)

end