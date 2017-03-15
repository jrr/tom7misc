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
     as long as we could ensure that it came first. *)
  (* Here we place the argv array, which is always the four
     bytes 0x81 0x00 (pointer to command line within PSP)
     0x00 0x00 (null pointer, since in C, argv[argc] is supposed
     to be null. *)
  val ARGV_POS = 256
  val FIRST_GLOBAL_POS = 260

  fun printable8 (b : Word8.word) = b >= 0wx20 andalso b <= 0wx7e

  fun widthsize C.Width8 = A.S8
    | widthsize C.Width16 = A.S16
    | widthsize C.Width32 = A.S32

  (* Optimizations TODO:
     - Reuse temporaries (allocatetmps should do this).
     - Replace locals with temporaries
        (Should this be done in CIL though? Can't do it for
         stuff that spans blocks there.)
        (Addressing tmps is usually much faster than
         reading/writing locals, in addition to avoiding
         the moves.)
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
       it adds lots of complexity. (is this true? -twm)

     *)

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  (* Layout size of a CIL type (e.g. in a locals frame or struct).
     This is not necessarily the size of a temporary that holds
     such a value. (Specifically, Word8 is stored in 16-bit tmps.)

     Signedness doesn't matter any more -- the representation is
     the same and the operations are explicit. *)
  fun typsize (C.Word32 _) = S32
    | typsize (C.Word16 _) = S16
    | typsize (C.Word8 _) = S8
    (* All pointers are 16-bit offsets into DS. *)
    | typsize (C.Pointer _) = S16
    (* Labels in code segment, not addresses but no way for
       them to be bigger than 16 bits! *)
    | typsize (C.Code _) = S16
    (* Maybe this should already have been compiled away. *)
    | typsize (C.Struct _) = raise ToASM "unimplemented: structs"
    | typsize (C.Array _) = raise ToASM "unexpected typsize Array"

  (* Number of bytes in the representation of a C variable (global or
     local) of type t. This allows arrays and structs. (Should this
     just be CIL.sizeof?) *)
  fun cvartypbytes (C.Word32 _) = 4
    | cvartypbytes (C.Word16 _) = 2
    | cvartypbytes (C.Word8 _) = 1
    | cvartypbytes (C.Pointer _) = 2
    | cvartypbytes (C.Code _) = 2
    | cvartypbytes (C.Array (t, i)) = i * cvartypbytes t
    | cvartypbytes (t as C.Struct _) = CIL.sizeof t

  (* Same, but the size of temporary used to store a value of that
     size. XXX If we still need typsize (used to calculate frame
     sizes?) then maybe these two functions should return different
     types to avoid confusion. There should be no tmps of S8. *)
  fun typtmpsize (C.Word8 _) = S16
    | typtmpsize t = typsize t

  (* Accumulate all of the mentions of Locals into the map. *)
  structure GetLocalsArg : CILPASSARG =
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
  structure GetLocals = CILPass(GetLocalsArg)

  infixr //
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

  (* Need to be able predict the label name from a function name. This
     is used to take the address of a function (i.e., its entry
     point), so that we can pass it around (or even just make a normal
     call like f(5)).

     If we generate fresh labels here, we need to first make a pass over
     all functions to annotate labels for them, and then supply that
     mapping to genblocks. *)
  fun function_header_label name =
    "__" ^ name ^ "_$entry"

  (* Compute the number of argument bytes from the types of the arguments.
     We need to be able to do this because we can call an abitrary function
     through a function pointer, where all we know is that function's type. *)
  fun bytes_for_args args =
    List.foldl (fn (t, b) => b + A.szbytes ` typsize t) 0 args

  (* Generate the blocks for a CIL Function, once we know the locals
     layout. *)
  fun genblocks { function_name, offsets, globalpositions,
                  retbytes, argbytes, localbytes, body, blocks } =
    let
      val local_frame_size = retbytes + argbytes + localbytes

      (* Blocks we've already translated, just for sanity checking. *)
      val done = ref SM.empty : unit SM.map ref
      (* Blocks left to translate. *)
      val todo = ref SM.empty : C.stmt SM.map ref
      val () = app (fn (lab, stmt) =>
                    todo := SM.insert (!todo, lab, stmt)) blocks

      fun newtmp (n, size) = new_named_tmp (function_name, n, size)
      fun vartmp (var, sz) =
        A.N { func = function_name, name = var, size = sz }
      fun tmpsize (A.N { size, ... }) = size

      (* Generate code to put the value v in a new temporary,
         then call the continuation with that temporary and its size. *)
      fun gentmp (ctx : C.context) (v : C.value)
        (k : A.named_tmp * C.typ -> A.named_cmd list * 'b) :
        A.named_cmd list * 'b =
        case v of
          C.Var var =>
            (case C.Context.lookup (ctx, var) of
               NONE => raise ToASM ("Unbound variable " ^ var)
             | SOME ctyp =>
                 (* A variable is always translated to a same-named
                    temporary at the appropriate size. *)
                 k (vartmp (var, typtmpsize ctyp), ctyp))
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
                      A.FrameOffset (tmp, Word16.fromInt pos) // k (tmp, C.Pointer typ)
                end
            | C.Global l =>
                let val tmp = newtmp ("addr_" ^ l, A.S16)
                in
                  case ListUtil.Alist.find op= globalpositions l of
                    NONE => raise ToASM ("unallocated global " ^ l ^ "?")
                  | SOME pos =>
                      A.Immediate16 (tmp, Word16.fromInt pos) // k (tmp, C.Pointer typ)
                end)
        | C.FunctionLiteral (name, ret, args) =>
           let
             val lab = function_header_label name
             val tmp = newtmp ("faddr_" ^ name, A.S16)
           in A.LoadLabel (tmp, lab) // k (tmp, C.Code (ret, args))
           end
        | C.Word8Literal w8 =>
           let val tmp = newtmp ("imm", A.S16)
           in
             (* Note that we store 8-bit quantities in the low byte of 16-bit
                registers. There should be no need to sign-extend
                here: All signed operations should either have
                been compiled to explicit promotions (which perform sign
                extension) or be operations like "LessEq8" which look only
                at the low byte of the register/temporary and then treat
                it as signed. *)
             A.Immediate16 (tmp, Word16.fromInt (Word8.toInt w8)) //
             k (tmp, C.Word8 C.Unsigned)
           end
        | C.Word16Literal w16 =>
           let val tmp = newtmp ("imm", A.S16)
           in A.Immediate16 (tmp, w16) // k (tmp, C.Word16 C.Unsigned)
           end
        | C.Word32Literal w32 =>
           let val tmp = newtmp ("imm", A.S32)
           in A.Immediate32 (tmp, w32) // k (tmp, C.Word32 C.Unsigned)
           end
        | C.StringLiteral _ =>
           raise ToASM "expected string literals to be compiled away by now"

      (* Generate code for e, and bind vt (if SOME) to its value. *)
      fun genexp (ctx : C.context) (vt : (string * C.typ) option, e : C.exp)
        (k : unit -> A.named_cmd list * 'b) :
        A.named_cmd list * 'b =
        case (e, vt) of
          (* For expressions that may have effects, we need to emit
             them whether or not the result is used (i.e. when vt is NONE). *)
          (C.Builtin (C.B_EXIT, args), _) =>
            (case args of
               nil =>
                 (* Even if vt is SOME, this does not return, so we just
                    ignore it. *)
                 A.Exit // k ()
             | _ => raise ToASM "unexpected args to exit")

        | (C.Builtin (C.B_PUTC, args), vt) =>
            (case args of
               [v] => gentmp ctx v (fn (ftmp, ftyp) => A.Putc ftmp // k ())
             | _ => raise ToASM "bad args to putc")

        | (C.Builtin (C.B_OUT8, args), vt) =>
            (case args of
               [p, b] =>
                 gentmp ctx p
                 (fn (ptmp, ptyp) =>
                  gentmp ctx b
                  (fn (btmp, btyp) =>
                   A.Out8 (ptmp, btmp) // k ()))
             | _ => raise ToASM "bad args to putc")

        | (C.Call (fv, argvs), vt) =>
            let
              (* It's extremely common for this to be a function call
                 like F(args), where F is a function address literal.
                 In that case, we can generate better code by jumping
                 directly to the function (DirectCall). In the general
                 case, we compute the function address as a normal
                 expression (IndirectCall). *)
              datatype calltype =
                IndirectCall of A.named_tmp
              | DirectCall of string
              fun unpack_function cont =
                case fv of
                  C.FunctionLiteral (name, ret, args) =>
                    cont (C.Code (ret, args), name,
                          DirectCall ` function_header_label name)
                | _ =>
                    gentmp ctx fv
                    (fn (ftmp, ftyp) =>
                     cont (ftyp, "indirect", IndirectCall ftmp))
            in
              unpack_function
              (fn (ftyp : CIL.typ, fname : string, calltype : calltype) =>
               case ftyp of
                 C.Code (rett, argts) =>
                   let
                     val returnbytes = A.szbytes ` typsize rett
                     val argbytes = bytes_for_args argts
                     val returnlabel = CILUtil.newlabel ("ret_from_" ^ fname)
                     val retaddrtmp = newtmp ("retaddr", A.S16)
                     val argaddrtmp = newtmp ("arg", A.S16)

                     (* makeargs (nil, argvs, argts, kk)
                        Put each argument in argvs into a temporary,
                        ensuring that it matches the expected type. Call
                        kk with the list of temporary/size pairs in forward
                        order. *)
                     fun makeargs (revtmps,
                                   argval :: argvs,
                                   argt :: argts,
                                   kk) =
                       gentmp ctx argval
                       (fn (atmp, atyp) =>
                        if typsize atyp <> typsize argt
                        then raise ToASM ("incompatible types to function " ^
                                          "call?")
                        else makeargs ((atmp, typsize argt) :: revtmps,
                                       argvs, argts, kk))
                     | makeargs (revtmps, nil, nil, kk) = kk (rev revtmps)
                     | makeargs _ = raise ToASM "wrong number of args in call?"

                     (* put_tmps_in_args pos argtmps kk
                        After the locals frame for the callee is set up,
                        store the arg values (in the temp list) into the
                        expected slots, and then call the continuation kk.

                        Keeps reusing the temporary argaddrtmp. *)
                     fun put_tmps_in_args pos nil kk =
                       if pos = retbytes + argbytes then kk ()
                       else raise ToASM "didn't fill arg slots as expected"
                       | put_tmps_in_args pos ((a, sz) :: rest) kk =
                         A.FrameOffset (argaddrtmp, Word16.fromInt pos) //
                         (case sz of
                            S8 => A.Store8 (argaddrtmp, a)
                          | S16 => A.Store16 (argaddrtmp, a)
                          | _ => raise ToASM
                              "XXX only 8/16 bit loads implemented") //
                         put_tmps_in_args (pos + A.szbytes sz) rest kk
                   in
                     makeargs
                     (nil, argvs, argts,
                      fn argtmps =>
                      (* Expand the frame to save all locals. After this,
                         don't use locals. *)
                      A.ExpandFrame local_frame_size //

                      (* Start placing args after the slot for the return
                         value. *)
                      put_tmps_in_args retbytes argtmps
                      (fn () =>
                       A.LoadLabel (retaddrtmp, returnlabel) //
                       A.Push retaddrtmp //
                       (* If an indirect call, push the destination of the
                          jump before we lose access to tmps. *)
                       (case calltype of
                          IndirectCall ftmp => A.Push ftmp
                        | DirectCall _ => A.Nop) //
                       (* Save our temporaries. After this, don't use temps. *)
                       A.SaveTemps (A.Named function_name) //
                       (* Do the appropriate indirect or direct jump. *)
                       (case calltype of
                          IndirectCall _ => A.PopJumpInd
                        | DirectCall lab => A.JumpCond (A.True, lab))  //
                       (* Here we'd like to just start another block, but the
                          structure of the code doesn't make this particularly
                          easy. So we insert this meta-command that later gets
                          rewritten into a normal labeled block. *)
                       A.Label returnlabel //
                       A.RestoreTemps (A.Named function_name) //
                       (* Callee has undone the expanded frame. *)
                       (* If we are using the return value, move it to the
                          temporary corresponding to the bound variable.
                          Either way, move the frame pointer. *)
                       (case vt of
                          NONE =>
                            A.ShrinkFrame local_frame_size //
                            k ()
                        | SOME (var, t) =>
                            let
                              val rvtmp = newtmp ("retvaloffset", A.S16)
                            in
                              (* Return value is at offset 0 in the called
                                 function's frame (which we haven't yet
                                 deleted.) *)
                              A.FrameOffset (rvtmp, Word16.fromInt 0) //
                              (case typsize rett of
                                 S8 => A.Load8 (vartmp (var, S16), rvtmp)
                               | S16 => A.Load16 (vartmp (var, S16), rvtmp)
                               | _ => raise ToASM "XXX only 8/16 bit loads implemented") //
                              A.ShrinkFrame local_frame_size //
                              k ()
                            end)))
                   end
               | _ => raise ToASM "call to value of non-code type?")
            end
        | (_, NONE) => k ()
        | (exp, SOME (var, t)) =>
            (case exp of
               C.Call _ => raise ToASM "Bug: already handled above."

             (* We currently define there to be exactly 1 argument,
                which is the entire command line from the PSP. *)
             | C.Builtin (C.B_ARGC, nil) =>
                 A.Immediate16 (vartmp (var, A.S16), Word16.fromInt 1) //
                 k ()

             (* We return ARGV_POS after initializing that pointer to a
                two-word argv array (first points to command line in PSP,
                second is null as required by the standard. *)
             | C.Builtin (C.B_ARGV, nil) =>
                let
                  val valtmp = newtmp ("argv_init", A.S16)
                  val addrtmp = newtmp ("argv2", A.S16)
                  val argvtmp = vartmp (var, typsize t)
                in
                  (* Write zero into second slot first *)
                  A.Immediate16 (addrtmp, Word16.fromInt (ARGV_POS + 2)) //
                  A.Immediate16 (valtmp, Word16.fromInt 0) //
                  A.Store16 (addrtmp, valtmp) //
                  (* Now put the first slot into argv, since that's where
                     we ultimately need it anyway *)
                  A.Immediate16 (argvtmp, Word16.fromInt ARGV_POS) //
                  (* And now write the pointer to the command line
                     in there. *)
                  A.Immediate16 (valtmp, Word16.fromInt 0x0081) //
                  A.Store16 (argvtmp, valtmp) //
                  k ()
                end

             | C.Builtin _ =>
                 raise ToASM ("unexpected builtin or bad args: " ^
                              CIL.exptos exp)
             | C.Value value =>
                 (* We could perhaps avoid the MOV in these cases, but
                    liveness/allocation does a pretty good job of
                    eliminating them. *)
                 gentmp ctx value
                 (fn (tmp, _) =>
                  (* XXX Check compatibility of t and sz from tmp? *)
                  let val A.N { size, ... } = tmp
                  in A.Mov (vartmp (var, size), tmp) // k ()
                  end)
             (* These should have been compiled away into GotoIf (cond, ...) *)
             | C.Greater _ => raise ToASM "bug: unexpected comparison op"
             | C.GreaterEq _ => raise ToASM "bug: unexpected comparison op"
             | C.Above _ => raise ToASM "bug: unexpected comparison op"
             | C.AboveEq _ => raise ToASM "bug: unexpected comparison op"
             | C.Less _ => raise ToASM "bug: unexpected comparison op"
             | C.LessEq _ => raise ToASM "bug: unexpected comparison op"
             | C.Below _ => raise ToASM "bug: unexpected comparison op"
             | C.BelowEq _ => raise ToASM "bug: unexpected comparison op"
             | C.Eq _ => raise ToASM "bug: unexpected comparison op"
             | C.Neq _ => raise ToASM "bug: unexpected comparison op"
             | C.Not _ => raise ToASM "bug: unexpected not"
             | C.Yet _ => raise ToASM "bug: unexpected yet"

             | C.Load (w, v) =>
                 gentmp ctx v
                 (fn (addr, _) =>
                  (case w of
                     C.Width8 =>
                       A.Load8 (vartmp (var, A.S16), addr) // k ()
                   | C.Width16 =>
                       A.Load16 (vartmp (var, A.S16), addr) // k ()
                   | C.Width32 => raise ToASM "unimplemented 32-bit loads"))

             | C.Plus (w, a, b) =>
                 gentmp ctx a
                 (fn (atmp, at) =>
                  gentmp ctx b
                  (fn (btmp, bt) =>
                   let
                     val dst = vartmp (var, tmpsize atmp)
                   in
                     if tmpsize atmp = tmpsize btmp andalso
                        tmpsize atmp = widthsize w
                     then ()
                     else raise ToASM ("incompatible args in Plus: " ^
                                       A.named_tmptos atmp ^ " + " ^
                                       A.named_tmptos btmp ^ " width " ^
                                       C.widthtos w);

                     A.Mov (dst, atmp) //
                     A.Add (dst, btmp) //
                     k ()
                   end))

             | C.Minus (w, a, b) =>
                 gentmp ctx a
                 (fn (atmp, at) =>
                  gentmp ctx b
                  (fn (btmp, bt) =>
                   let
                     val dst = vartmp (var, tmpsize atmp)
                   in
                     if tmpsize atmp = tmpsize btmp andalso
                        tmpsize atmp = widthsize w
                     then ()
                     else raise ToASM "incompatible args in Minus";

                     A.Mov (dst, atmp) //
                     A.Sub (dst, btmp) //
                     k ()
                   end))

             | C.Xor (w, a, b) =>
                 gentmp ctx a
                 (fn (atmp, at) =>
                  gentmp ctx b
                  (fn (btmp, bt) =>
                   let
                     val dst = vartmp (var, tmpsize atmp)
                   in
                     if tmpsize atmp = tmpsize btmp andalso
                        tmpsize atmp = widthsize w
                     then ()
                     else raise ToASM "incompatible args in Xor";

                     A.Mov (dst, atmp) //
                     A.Xor (dst, btmp) //
                     k ()
                   end))

             | C.And (w, a, b) =>
                 gentmp ctx a
                 (fn (atmp, at) =>
                  gentmp ctx b
                  (fn (btmp, bt) =>
                   let
                     val dst = vartmp (var, tmpsize atmp)
                   in
                     if tmpsize atmp = tmpsize btmp andalso
                        tmpsize atmp = widthsize w
                     then ()
                     else raise ToASM "incompatible args in And";

                     A.Mov (dst, atmp) //
                     A.And (dst, btmp) //
                     k ()
                   end))


             (* A | B = (A & B) + (A ^ B) *)
             | C.Or (w, a, b) =>
                 gentmp ctx a
                 (fn (atmp, at) =>
                  gentmp ctx b
                  (fn (btmp, bt) =>
                   let
                     val txor = newtmp ("orxor", tmpsize atmp)
                     val dst = vartmp (var, tmpsize atmp)
                   in
                     if tmpsize atmp = tmpsize btmp andalso
                        tmpsize atmp = widthsize w
                     then ()
                     else raise ToASM "incompatible args in Or";

                     (* PERF: This is a good way to do it, but
                        maybe the decomposition should happen in
                        a tactic. *)
                     A.Mov (txor, atmp) //
                     A.Xor (txor, btmp) //
                     (* Now txor contains A ^ B *)
                     A.Mov (dst, atmp) //
                     A.And (dst, btmp) //
                     (* Now dst contains A & B. *)
                     A.Add (dst, txor) //
                     (* Now dst contains A | B. *)
                     k ()
                   end))

             | C.Cast { src, dst, v : C.value } =>
                 gentmp ctx v
                 (fn (vtmp, vt) =>
                  if typsize src <> typsize dst orelse typsize vt <> typsize src
                  then raise ToASM ("alleged compatible cast between " ^
                                    "types of different widths: " ^
                                    "src=" ^ C.typtos src ^ " dst=" ^ C.typtos dst ^
                                    " arg type=" ^ C.typtos vt)
                  else A.Mov (vartmp (var, typsize dst), vtmp) // k ())

             | C.Promote { signed, src, dst, v : C.value } =>
                 gentmp ctx v
                 (fn (vtmp, vt) =>
                  (case (signed, src, dst) of
                     (false, C.Width8, C.Width16) =>
                       (* This is trivial, because we store 8-bit values as
                          16-bit ones with the high bits set to 0.

                          XXX wait do we? Plus on two 8-bit quantities would
                          overflow into the high byte, unless we explicitly
                          mask afterwards. *)
                       A.Mov (vartmp (var, A.S16), vtmp) // k ()
                   | _ =>
                       raise ToASM "unimplemented promote type"))

             | C.Truncate { src : C.width, dst : C.width, v : C.value } =>
                 gentmp ctx v
                 (fn (vtmp, vt) =>
                  (case (src, dst) of
                     (C.Width16, C.Width8) =>
                       (* Also easy, but we need to mask off the bits
                          to put it in the correct representation.
                          PERF: This can probably be done much more
                          efficiently with a primitive. *)
                       let
                         val masktmp = newtmp ("mask16to8", S16)
                         val dsttmp = vartmp (var, A.S16)
                       in
                         A.Immediate16 (masktmp, Word16.fromInt 0xFF) //
                         A.Mov (dsttmp, vtmp) //
                         A.And (dsttmp, masktmp) //
                         k ()
                       end
                   | _ =>
                       (* 32 to 16 truncation probably needs to be
                          a primitive, since we can't change the type
                          of a temporary otherwise. Easy, though.
                          32 to 8 should just be compiled away as
                          the composition for now? *)
                       raise ToASM "unimplemented truncation type"
                         ))

             | C.Times (w, a, b) => raise ToASM "unimplemented times"
             | C.SignedDivision (w, a, b) => raise ToASM "unimplemented signed division"
             | C.UnsignedDivision (w, a, b) => raise ToASM "unimplemented unsigned division"
             | C.UnsignedMod (w, a, b) => raise ToASM "unimplemented unsigned mod"
             | C.LeftShift (w, a, b) => raise ToASM "unimplemented left shift"
             | C.RightShift (w, a, b) => raise ToASM "unimplemented right shift"
             | C.Complement (w, a) => raise ToASM "unimplemented bitwise complement"
             | C.Negate (w, a) => raise ToASM "unimplemented negation")

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
                  (fn (addrtmp, _) =>
                   gentmp ctx v
                   (fn (vtmp, _) =>
                    (A.Store8 (addrtmp, vtmp) :: cmds, next)))
              | C.Width16 =>
                  gentmp ctx addr
                  (fn (addrtmp, _) =>
                   gentmp ctx v
                   (fn (vtmp, _) =>
                    (A.Store16 (addrtmp, vtmp) :: cmds, next)))
              | C.Width32 =>
                  (* Maybe just compile it as two 16-bit stores
                     for now? *)
                  raise ToASM "32-bit stores not implemented yet"
            end

        | C.GotoIf (cond, truelab, s) =>
            let val (cmds, next) = gencmds ctx s

              (* If one of the operands is zero, return SOME of the
                 other one. *)
              fun getzero (w, a, b) =
                case (w, a, b) of
                  (C.Width8, C.Word8Literal 0w0, b) => SOME b
                | (C.Width8, a, C.Word8Literal 0w0) => SOME a
                | (C.Width32, C.Word32Literal 0w0, b) => SOME b
                | (C.Width32, a, C.Word32Literal 0w0) => SOME a
                (* Ugh, so awkward without word literals... *)
                | (C.Width16, a as C.Word16Literal w, b) =>
                    if w = Word16.fromInt 0 then SOME b
                    else (case b of
                            C.Word16Literal bw =>
                              if bw = Word16.fromInt 0 then SOME a
                              else NONE
                          | _ => NONE)
                | (C.Width16, a, C.Word16Literal w) =>
                    if w = Word16.fromInt 0 then SOME a
                    else NONE
                | _ => NONE
            in
              (* FIXME need to generate 8-bit comparisons (at least
                 for the signed ones) if the arguments are 8-bit. *)
              case cond of
                C.CLess (w, a, b) =>
                  gentmp ctx a
                  (fn (atmp, _) =>
                   gentmp ctx b
                   (fn (btmp, _) =>
                    (A.JumpCond (A.Less (atmp, btmp), truelab) :: cmds,
                     next)))
              | C.CLessEq (w, a, b) =>
                  gentmp ctx a
                  (fn (atmp, _) =>
                   gentmp ctx b
                   (fn (btmp, _) =>
                    (A.JumpCond (A.LessEq (atmp, btmp), truelab) :: cmds,
                     next)))
              | C.CBelow (w, a, b) =>
                  gentmp ctx a
                  (fn (atmp, _) =>
                   gentmp ctx b
                   (fn (btmp, _) =>
                    (A.JumpCond (A.Below (atmp, btmp), truelab) :: cmds,
                     next)))
              | C.CBelowEq (w, a, b) =>
                  gentmp ctx a
                  (fn (atmp, _) =>
                   gentmp ctx b
                   (fn (btmp, _) =>
                    (A.JumpCond (A.BelowEq (atmp, btmp), truelab) :: cmds,
                     next)))

              | C.CEq (w, a, b) =>
                  (case getzero (w, a, b) of
                     SOME other =>
                       gentmp ctx other
                       (fn (otmp, _) =>
                        (A.JumpCond (A.EqZero otmp, truelab) :: cmds,
                         next))
                   | NONE =>
                       gentmp ctx a
                       (fn (atmp, _) =>
                        gentmp ctx b
                        (fn (btmp, _) =>
                         (A.JumpCond (A.Eq (atmp, btmp), truelab) :: cmds,
                          next))))
              | C.CNeq (w, a, b) =>
                  (case getzero (w, a, b) of
                     SOME other =>
                       gentmp ctx other
                       (fn (otmp, _) =>
                        (A.JumpCond (A.NeZero otmp, truelab) :: cmds,
                         next))
                   | NONE =>
                       gentmp ctx a
                       (fn (atmp, _) =>
                        gentmp ctx b
                        (fn (btmp, _) =>
                         (A.JumpCond (A.NotEq (atmp, btmp), truelab) :: cmds,
                          next))))
            end

        (* XXX: This fallthrough stuff is not harmful, but it does make
           this code sort of confusing, and it may be unnecessary since
           tox86 will (?) rearrange blocks. *)
        | C.Goto lab =>
            (case SM.find (!done, lab) of
               (* If we haven't yet processed the destination block,
                  just put it next and fall through. *)
               NONE => (nil, SOME lab)
               (* Otherwise, an explicit jump and we don't have
                  a suggestion for the next block. *)
             | SOME () => ([A.JumpCond (A.True, lab)], NONE))

        | C.Return NONE =>
            (* XXX maybe check that return size is 0? *)
            ([A.PopJumpInd],
             NONE)

        | C.Return (SOME v) =>
            gentmp ctx v
            (fn (rtmp, rett) =>
             let
               val retsize = typsize rett
               (* Address of return value. *)
               val ratmp = newtmp ("retoffset", S16)
             in
               A.FrameOffset (ratmp, Word16.fromInt 0) //
               (case retsize of
                  S8 => A.Store8 (ratmp, rtmp)
                | S16 => A.Store16 (ratmp, rtmp)
                | _ => raise ToASM "XXX only 8/16 bit stores implemented") //

              (* The return address is at the top of the stack.
                 PopJumpInd expects its argument there, so that's
                 all we need to do! *)
              ([A.PopJumpInd],
               NONE)
             end)
        | C.End =>
            (* Unreachable, so we can just fall off the end.
               (This is used in global initialization, but that
               will have been rewritten by now.) *)
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
          val (ntodo, stmt) =
            SM.remove (!todo, lab)
            handle _ => raise ToASM ("bug: " ^ lab ^ " should always be in " ^
                                     "the todo list at this point...")
          val () = todo := ntodo
          val () = done := SM.insert (!done, lab, ())
          val (cmds, next) = gencmds C.Context.empty stmt
        in
          A.Block { name = lab,
                    tmp_frame = A.Named function_name,
                    cmds = cmds } ::
          (case next of
             NONE => donext ()
           | SOME lnext => genblock lnext)
        end

      val header_label = function_header_label function_name
      fun genall () =
        (* Any initialization we like here. This used to allocate
           the frame, but now the caller does that.

           We still need this block (even though it's now empty)
           because elsewhere we may need to refer to this label. *)
        A.Block { name = header_label,
                  tmp_frame = A.Named function_name,
                  cmds = nil } ::
        (* And then continue with the function's entry point. *)
        genblock body
    in
      genall ()
    end

  (* Remove uses of the Label meta-command from the list of blocks,
     by creating an actual block in the sequence with that label. *)
  fun unlabel nil = nil
    | unlabel (A.Block { name, tmp_frame, cmds } :: rest) =
    let
      fun ul nil = (nil, nil)
        | ul (cmd :: crest) =
        let
          val (cs, blocks) = ul crest
        in
          case cmd of
            A.Label lab => (nil, A.Block { name = lab,
                                           (* must be part of the same context,
                                              so inherit its tmp_frame *)
                                           tmp_frame = tmp_frame,
                                           cmds = cs } :: blocks)
          | _ => (cmd :: cs, blocks)
        end

      val (hc, blocks) = ul cmds
    in
      (A.Block { name = name,
                 tmp_frame = tmp_frame,
                 cmds = hc } :: blocks) @ unlabel rest
    end

  fun doproc globalpositions
             (name, C.Func
              { args : (string * C.typ) list,
                ret : C.typ,
                body : string,
                blocks : (string * C.stmt) list }) : A.named_block list =
    let
      val argsizes = ListUtil.mapsecond typsize args
      val localsizes : int SM.map =
        let
          val argset : unit SM.map =
            foldl (fn ((s, t), m) => SM.insert (m, s, ())) SM.empty args

          val localtypes : C.typ SM.map ref = ref SM.empty
          fun oneblock (_, stmt) =
            ignore ` GetLocals.converts localtypes C.Context.empty stmt
        in
          app oneblock blocks;
          (* Exclude args from locals, since they are allocated as
             args. *)
          SM.filteri (fn (k, s) =>
                      not ` Option.isSome ` SM.find (argset, k)) `
          SM.map cvartypbytes ` !localtypes
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
      fun alloc (s, bytes) =
        let in
          offsets := (s, !nextpos) :: !offsets;
          nextpos := !nextpos + bytes
        end

      (* Return value is first. *)
      val retbytes = A.szbytes (typsize ret)
      val () = nextpos := !nextpos + retbytes

      (* Then the args. *)
      val () = List.app (fn (s, sz) => alloc (s, A.szbytes sz)) argsizes
      val argbytes = !nextpos - retbytes
      val () =
        let
          (* XXX what about retbytes?
             ... and maybe just better to ensure this by construction? *)
          val computed_argbytes = bytes_for_args (map #2 args)
        in
          (* A function's argument layout has to be determinable
             from only its types, so that a call can be done
             through a function pointer. *)
          if computed_argbytes = argbytes then ()
          else raise ToASM ("Bug: expected argbytes " ^ Int.toString argbytes ^
                            " but got " ^ Int.toString computed_argbytes)
        end

      val () = SM.appi alloc localsizes
      val localbytes = !nextpos - argbytes

      val blocks = genblocks { function_name = name, offsets = !offsets,
                               retbytes = retbytes,
                               globalpositions = globalpositions,
                               argbytes = argbytes, localbytes = localbytes,
                               body = body, blocks = blocks }

      (* Function calls insert Label meta-commands. Make them into proper
         labels. *)
      val blocks = unlabel blocks
    in
      print ("Frame for proc " ^ name ^ ":\n" ^
             String.concat (map (fn (s, off) => "  " ^ Int.toString off ^
                                 ": " ^ s ^ "\n") (rev (!offsets))));
      blocks
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
          val numbytes = cvartypbytes typ
          val bytes =
            (case bytes of
               NONE => Word8Vector.tabulate (numbytes,
                                             fn _ =>
                                             Word8.fromInt ` ord #"?")
             | SOME b => if Word8Vector.length b <> numbytes
                         then raise ToASM ("Typ/bytes disagree in global " ^
                                           name ^ "?")
                         else b)
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
        ("[Now you're looking at the PSP. The address of the opening square " ^
         "bracket is DS:0000, but this gets overwritten by DOS on load.  ")

      (* val () = Segment.set_string datasegment 0x50 "XXX" *)

      val () = Segment.set_string datasegment 0x81
        ("(Right here is where the command line is placed by DOS, up to  127 bytes." ^
         " Before the open paren is its length in a byte.)")

      val () = Segment.set_string datasegment 255 "]"
      val () = Segment.lock_range datasegment 0 256

      val (globalpositions, frame_stack_start) =
        allocglobals (datasegment, globals)

      fun onefunction (name, func) =
        (name, doproc globalpositions (name, func))

      val proc_blocks : (string * A.named_block list) list =
        map onefunction functions
      (* Each proc is a list of blocks that have to go in that order,
         but the lists can be permuted in any way we like. We don't do
         any intraprocedural optimization. However, 'main' (which is
         actually the internal init code) needs to be first, because
         the asm_init block just falls through to it. Find that block. *)
      val blocks =
        case ListUtil.Alist.extract op= proc_blocks main of
          NONE => raise ToASM ("there is no main function " ^ main ^ "?")
        | SOME (mproc : A.named_block list, rest) =>
            let
              (* Flatten the blocks. *)
              val rest = List.concat ` map #2 rest
            in
              (* Put main's code first. *)
              mproc @ rest
            end

      val init_label = CILUtil.newlabel "__abc_asm_init"
      val init_block = A.Block { name = init_label,
                                 tmp_frame = A.Named "__abc_asm_init",
                                 cmds = [A.Init] }
    in
      (* Probably should include some headroom here..? *)
      if frame_stack_start > 65536
      then raise ToASM ("Size of globals statically exceeds the 16-bit " ^
                        "data-segment. This program can't be compiled " ^
                        "for this architecture! :(")
      else ();

      A.Program { blocks = init_block :: blocks,
                  frame_stack_start = frame_stack_start,
                  datasegment = datasegment }
    end
  handle LibBase.NotFound => raise ToASM "NotFound exn in ToASM?"

  (* Optimization TODO: Remove labels that are never referenced!
     It may also be possible to drop dead blocks? Are there any
     situations where we get them? *)

  (* Easy optimization TODO: Remove labels with no code. These
     actually become nontrivial code (rung + explicit fallthrough) in
     tox86. *)

  (* Optimization TODO: Coalesce blocks that are exactly equal. *)

  (* Optimization TODO: We can use EBX+disp8 (or BX+disp8) addressing
     mode most of the time we write to or read from a local, instead
     of loading the address with FrameOffset and doing an indirect. *)

end
