structure ToASM :> TOASM =
struct
  infixr 9 `
  fun a ` b = a b

  structure C = CIL
  structure A = ASM

  exception ToASM of string

  (* Optimizations TODO:
     - Reuse temporaries.
     - Replace locals with temporaries
        (Should this be done in CIL though? Can't do it for
         stuff that spans blocks there.)
        (Is it even faster, anyway? I guess it would cut
         down on moves between locals and "registers")
     - A handful of operators can use immediate values, at
       least printable ones. *)

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  datatype sz = S8 | S16 | S32
  fun szbytes S8 = 1
    | szbytes S16 = 2
    | szbytes S32 = 4

  (* Signedness doesn't matter any more -- the representation is
     the same and the operations are explicit. *)
  fun ctypsize (C.Word32 _) = S32
    | ctypsize (C.Word16 _) = S16
    | ctypsize (C.Word8 _) = S8
    (* All pointers are 16-bit offsets into DS. *)
    | ctypsize (C.Pointer _) = S16
    (* Labels in code segment, not addresses but no way for
       them to be bigger than 16 bits! *)
    | ctypsize (C.Code _) = S16
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
                 (k : string * sz -> (A.cmd list * 'b)) : A.cmd list * 'b =
        case v of
          C.Var var =>
            (case C.Context.lookup (ctx, var) of
               NONE => raise ToASM ("Unbound variable " ^ var)
             | SOME ctyp =>
                 (* A variable is always translated to a same-named
                    temporary. *)
                 k (var, ctypsize ctyp))
        (* typ is the type of the thing thing pointed to. *)
        | C.AddressLiteral (loc, typ) => raise ToASM "unimplemented addr"
        | C.Word8Literal w8 => raise ToASM "unimplemented w8"
        | C.Word16Literal w16 => raise ToASM "unimplemented w16"
        | C.Word32Literal w32 => raise ToASM "unimplemented w32"
        | C.StringLiteral _ => raise ToASM "unimplemented string literals"

      fun genexp (ctx : C.context) (e : C.exp)
                 (k : string * sz -> (A.cmd list * 'b)) : A.cmd list * 'b =
        raise ToASM "unimplemented genexp"
(*
            Value of value
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
  | Greater of width * value * value
  | GreaterEq of width * value * value
  | Above of width * value * value
  | AboveEq of width * value * value
  | Less of width * value * value
  | LessEq of width * value * value
  | Below of width * value * value
  | BelowEq of width * value * value
  | Eq of width * value * value
  | Neq of width * value * value
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
            let
              val ctx = C.Context.insert (ctx, var, t)
              val (cmds, next) = gencmds ctx s
            in (A.Add16 (var, "unimplemented") :: cmds, next)
            end
        | C.Do (e, s) =>
            let val (cmds, next) = gencmds ctx s
            in (A.Xor16 ("unused", "unimplemented") :: cmds, next)
            end
        | C.Store (width, addr, v, s) =>
            let val (cmds, next) = gencmds ctx s
            in
              case width of
                C.Width8 =>
                  gentmp ctx addr
                  (fn (taddr, _) =>
                   gentmp ctx v
                   (fn (tv, _) =>
                    (A.Store8 (taddr, tv) :: cmds, next)))
              | C.Width16 =>
                  gentmp ctx addr
                  (fn (taddr, _) =>
                   gentmp ctx v
                   (fn (tv, _) =>
                    (A.Store16 (taddr, tv) :: cmds, next)))
              | C.Width32 =>
                  (* Maybe just compile it as two 16-bit stores
                     for now? *)
                  raise ToASM "32-bit stores not implemented yet"
            end
        | C.GotoIf (v, truelab, s) =>
            let val (cmds, next) = gencmds ctx s
            in
              gentmp ctx v
              (fn (tv, _) =>
               (A.JumpCond16 (A.NeZero tv, truelab) :: cmds, next))
            end
        | C.Goto lab =>
            (case SM.find (!done, lab) of
               (* If we haven't yet processed the destination block,
                  just put it next and fall through. *)
               SOME () => (nil, SOME lab)
               (* Otherwise, an explicit jump and we don't have
                  a suggestion for the next block. *)
             | NONE => ([A.JumpCond16 (A.True, lab)], NONE))

        | C.Return v => raise ToASM "unimplemented"
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
        (* Need to set up local variable frame. *)
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