structure ToASM :> TOASM =
struct
  infixr 9 `
  fun a ` b = a b

  structure C = CIL
  structure A = ASM

  exception ToASM of string
  datatype sz = datatype ASM.sz

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
    fun newtmp (name, size) =
      let
        val x = !ctr
      in
        ctr := x + 1;
        (* XXX this is pretty bogus since we keep using $.
           Should either parse to remove $, or just have
           a symbol type. *)
        (name ^ "$t" ^ Int.toString x, size)
      end
  end

  (* Generate the blocks for a CIL Function, once we know the locals
     layout. *)
  fun genblocks { name, offsets, argbytes, localbytes, body, blocks } =
    let
      (* Blocks we've already translated, just for sanity checking. *)
      val done = ref SM.empty : unit SM.map ref
      (* Blocks left to translate. *)
      val todo = ref SM.empty : C.stmt SM.map ref
      val () = app (fn (lab, stmt) =>
                    todo := SM.insert (!todo, lab, stmt)) blocks

      (* Generate code to put the value v in a new temporary,
         then call the continuation with that temporary and its size. *)
      fun gentmp (ctx : C.context) (v : C.value)
                 (k : A.tmp -> A.cmd list * 'b) : A.cmd list * 'b =
        case v of
          C.Var var =>
            (case C.Context.lookup (ctx, var) of
               NONE => raise ToASM ("Unbound variable " ^ var)
             | SOME ctyp =>
                 (* A variable is always translated to a same-named
                    temporary at the appropriate size. *)
                 k (var, ctypsize ctyp))
        (* typ is the type of the thing thing pointed to. *)
        | C.AddressLiteral (loc, typ) => raise ToASM "unimplemented addr"
        | C.Word8Literal w8 => raise ToASM "unimplemented w8"
        | C.Word16Literal w16 => raise ToASM "unimplemented w16"
        | C.Word32Literal w32 => raise ToASM "unimplemented w32"
        | C.StringLiteral _ => raise ToASM "unimplemented string literals"

      (* Generate code for e, and bind vt (if SOME) to its value. *)
      fun genexp (ctx : C.context) (vt : (string * C.typ) option, e : C.exp)
                 (k : unit -> A.cmd list * 'b) : A.cmd list * 'b =
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
             let val sz = #2 tmp
             in A.Mov ((var, sz), tmp) // k ()
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

        | (_, SOME (var, t)) => raise ToASM "unimplemented many exps..."

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

  (* Is this necessary? I think we just translate the lvalue and
     don't LOAD from it. *)
  (* | AddressOf of string *)
  (* Similarly, this becomes a LOAD *)
  (* | Dereference of value *)
    (* Math and then LOAD *)
  (* | Subscript of value * value
     | Member of value * string *)
  | Call of value * value list
  | Load of width * value
*)

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
            let val ratmp = newtmp ("returnaddr", S16)
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
          A.Block { name = lab, cmds = cmds } ::
          (case next of
             NONE => donext ()
           | SOME lnext => genblock lnext)
        end

      val header_label = name ^ "$hdr"
      fun genall () =
        (* Need to set up local variable frame. Caller has already
           set up 'argbytes' bytes for us. *)
        A.Block { name = header_label, cmds = [A.ExpandFrame localbytes] } ::
        (* And then continue with the function's entry point. *)
        genblock body
    in
      genall ()
    end


  fun doproc (name, C.Func
              { args : (string * C.typ) list,
                ret : C.typ,
                body : string,
                blocks : (string * C.stmt) list }) : A.proc =
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
         Rearranging args would take a global analysis, but would not
         be so bad. *)
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
                               argbytes = argbytes, localbytes = localbytes,
                               body = body, blocks = blocks }
    in
      print ("Frame for proc " ^ name ^ ":\n" ^
             String.concat (map (fn (s, off) => "  " ^ Int.toString off ^
                                 ": " ^ s ^ "\n") (rev (!offsets))));
      A.Proc { name = name,
               blocks = blocks,
               offsets = !offsets,
               argbytes = argbytes,
               localbytes = localbytes }
    end

  (* XXX globals ! *)
  fun toasm (C.Program { functions, globals = _ }) =
     A.Program { main = "main",
                 procs = map doproc functions }
end