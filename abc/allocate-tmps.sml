structure AllocateTmps :> ALLOCATETMPS =
struct
  infixr 9 `
  fun a ` b = a b

  exception AllocateTmps of string

  open ASM

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  structure ISM = ImperativeMapFn(type ord_key = string
                                  val compare = String.compare)
  structure TM = ImperativeMapFn(type ord_key = ASM.named_tmp
                                 val compare = ASM.named_tmpcompare)
  structure TS = Liveness.TS


  fun map_cond f cond =
    case cond of
      Below (a, b) => Below (f a, f b)
    | BelowEq (a, b) => BelowEq (f a, f b)
    | Less (a, b) => Less (f a, f b)
    | LessEq (a, b) => LessEq (f a, f b)
    | Eq (a, b) => Eq (f a, f b)
    | NotEq (a, b) => NotEq (f a, f b)
    | EqZero tmp => EqZero (f tmp)
    | NeZero tmp => NeZero (f tmp)
    | True => True

  fun map_cmd (ft : 'a -> 'b) (fo : 'c -> 'd) (cmd : ('a, 'c) cmd)
    : ('b, 'd) cmd =
    case cmd of
      SaveTemps a => SaveTemps (fo a)
    | RestoreTemps a => RestoreTemps (fo a)
    | ExpandFrame a => ExpandFrame a
    | ShrinkFrame a => ShrinkFrame a
    | FrameOffset (tmp, w16) => FrameOffset (ft tmp, w16)
    | Label lab => Label lab
    | Load8 (a, b) => Load8 (ft a, ft b)
    | Load16 (a, b) => Load16 (ft a, ft b)
    | Store8 (a, b) => Store8 (ft a, ft b)
    | Store16 (a, b) => Store16 (ft a, ft b)
    | Immediate8 (tmp, w) => Immediate8 (ft tmp, w)
    | Immediate16 (tmp, w) => Immediate16 (ft tmp, w)
    | Immediate32 (tmp, w) => Immediate32 (ft tmp, w)
    | Add (a, b) => Add (ft a, ft b)
    | Sub (a, b) => Sub (ft a, ft b)
    | Complement tmp => Complement (ft tmp)
    | Mov (a, b) => Mov (ft a, ft b)
    | Xor (a, b) => Xor (ft a, ft b)
    | Push tmp => Push (ft tmp)
    | Pop tmp => Pop (ft tmp)
    | LoadLabel (tmp, s) => LoadLabel (ft tmp, s)
    | PopJumpInd => PopJumpInd
    | JumpCond (cond, lab) => JumpCond (map_cond ft cond, lab)
    | Nop => Nop
    | Init => Init
    | Putc t => Putc (ft t)
    | Out8 (a, b) => Out8 (ft a, ft b)
    | Exit => Exit


  (* Optimize the temporaries by coalescing two temporaries
     that don't interfere. There are two important goals:
       - Remove MOV instructions where possible:
           mov dst, src
         can be made into a no-op (i.e., dropped) if dst and
         src are actually the same temporary.
       - Reduce the total number of temporaries in a frame
         so that it fits within printable offsets of EBP.

     These are both achieved by identifying two temporaries
     that can be safely coalesced, and making them the same.
     To make two temporaries the same, we just pick one and
     substitute the other for it everywhere. Because the
     liveness analysis is not easily transformed back into
     blocks, we substitute through cmds, the in/out sets,
     and the blocks. *)
  fun coalesce { cmds, inlive, outlive } blocks =
    let
      (* This routine is imperative, so we keep rewriting this... *)
      (* PERF: We don't read from blocks, so we could instead keep a
         substitution and apply it at the end. Just need to be careful
         because it's not simultaneous substitution. *)
      val blocks = ref blocks

      (* All of the temporaries we see, keyed by context (func). *)
      val all_tmps = ISM.empty () : TS.set ISM.map
      fun gather_tmps cmd =
        ignore (map_cmd
                (fn tmp as ASM.N { func, name, size } =>
                 ISM.update (all_tmps,
                             func,
                             fn NONE => TS.singleton tmp
                              | SOME s => TS.add (s, tmp)))
                Util.I
                cmd)
      val () = Array.app gather_tmps cmds

      (* Two temporaries interfere if they are live at
         the same time; i.e., appear together in an
         entry in the inlive or outlive sets.

         This data structure maps temporaries to the
         set of temporaries that interfere with it.
         (Self-interference is assumed.)

         Interference is not transitive. *)
      val interference = TM.empty () : TS.set TM.map
      fun compute_interference () =
        let
          fun make_interference idx =
            let
              fun interfere s =
                TS.app (fn tmp =>
                        let val others = TS.delete (s, tmp)
                        in
                          (* Try to avoid creating empty interference
                             sets. *)
                          if TS.isEmpty others
                          then ()
                          else
                            TM.update (interference, tmp,
                                       fn NONE => others
                                        | SOME ss => TS.union (ss, others))
                        end) s
            in
              interfere (Array.sub (inlive, idx));
              interfere (Array.sub (outlive, idx))
            end
        in
          TM.clear interference;
          Util.for 0 (Array.length cmds - 1) make_interference
        end
      fun does_interfere a b =
        case TM.find (interference, a) of
          NONE => false
        | SOME s => TS.member (s, b)

      fun print_interference () =
        (* printerference *)
        let in
          print "Interfering tmps:\n";
          TM.appi (fn (tmp, s) =>
                   print (ASM.named_tmptos tmp ^ ": " ^
                          StringUtil.delimit " "
                          (map ASM.named_tmptos (TS.listItems s)) ^ "\n"))
          interference
        end
      val () = print_interference ()

      fun tmpsize (ASM.N { size, ... }) = size

      (* Globally rename src to dst; caller must ensure that this is
         legal and doesn't break the program! *)
      fun coalesce dst src =
        let
          val ASM.N { func = func1, size = sz1, ... } = dst
          val ASM.N { func = func2, size = sz2, ... } = src
          val () = if func1 = func2 then ()
                   else raise AllocateTmps ("Attempt to coalesce tmps from " ^
                                            "different functions.")
          val () = if sz1 = sz2 then ()
                   else raise AllocateTmps ("Attempt to coalesce tmps of " ^
                                            "different sizes! " ^
                                            ASM.named_tmptos dst ^ " and " ^
                                            ASM.named_tmptos src)

          fun rewrite_tmp t =
            if EQUAL = ASM.named_tmpcompare (t, src)
            then dst
            else t
          fun rewrite_cmd cmd = map_cmd rewrite_tmp Util.I cmd
          fun rewrite_set s = TS.map rewrite_tmp s
          fun rewrite_block (Block { name, tmp_frame, cmds }) =
            Block { name = name,
                    tmp_frame = tmp_frame,
                    cmds = map rewrite_cmd cmds }
        in
          print ("Coalesce " ^ ASM.named_tmptos src ^ " to become " ^
                 ASM.named_tmptos dst ^ "\n");
          (* Within commands, we just change the names; easy. *)
          Array.modify rewrite_cmd cmds;
          blocks := map rewrite_block (!blocks);
          (* Within the liveness sets, same thing, except they're sets
             so two elements may become one. *)
          Array.modify rewrite_set inlive;
          Array.modify rewrite_set outlive;
          (* Just remove "src" from the set of all tmps. We only
             need to look in its function key. *)
          ISM.update (all_tmps, func1,
                      (fn NONE => raise AllocateTmps "bug: bad all_tmps"
                        | SOME s =>
                       (TS.delete (s, src)
                        handle LibBase.NotFound =>
                          raise AllocateTmps ("bug: missing " ^
                                              ASM.named_tmptos src ^
                                              " in all_tmps"))));
          (* Finally, recompute interference.
             PERF: This can probably be done in place pretty
             straightforwardly, but this is safer. *)
          compute_interference ()
        end

      (* Prioritize merging tmps that appear in a MOV instruction
         together. *)
      val () =
        Util.for 0 (Array.length cmds - 1)
        (fn idx =>
         case Array.sub (cmds, idx) of
           Mov (dst, src) =>
             (* these should be from the same function and be
                the same size... *)
             if does_interfere src dst
             then ()
             else coalesce src dst
         | _ => ())

      (* Very common to do tmp1 = frameoffset+x, tmp2 = [tmp1].
         There's a good trick for doing tmp = [tmp], so make a
         high priority of coalescing these too. *)
      val () =
        Util.for 0 (Array.length cmds - 1)
        (fn idx =>
         case Array.sub (cmds, idx) of
           Load16 (dst, src) =>
             (case (tmpsize dst, tmpsize src) of
                (ASM.S16, ASM.S16) =>
                  if does_interfere src dst
                  then ()
                  else coalesce src dst
              | _ => raise AllocateTmps ("tmps should both be 16-bit in " ^
                                         "Load16: " ^
                                         ASM.named_tmptos dst ^ " and " ^
                                         ASM.named_tmptos src))
         | _ => ())

      val () = print_interference ()

      (* Now just merge all pairs.
         Note that this is where some smarter "graph coloring"
         algorithms could maybe do a better job. We just
         do it eagerly until we can't. *)
      fun onefunc func =
        let
          val () = print ("Try coalescing in " ^ func ^ "...\n")
          (* Each time we coalesce, we might lose one of the
             temporaries that we're holding onto here. But
             we also don't want to keep starting over from
             the beginning every time we succeed. So, we try
             coalescing all pairs here, but only if they
             both still exist when we consider them. *)
          fun still_exists t =
            case ISM.find (all_tmps, func) of
              NONE => false (* Shouldn't happen, but... *)
            | SOME s => TS.member (s, t)

          (* Keep this vector of tmps for the whole pass. *)
          val tmps =
            case ISM.find (all_tmps, func) of
              NONE => raise AllocateTmps ("Bug: this is called on domain " ^
                                          "of all_tmps?")
            | SOME s => Vector.fromList `TS.listItems s
          val num_tmps = Vector.length tmps
        in
          (* PERF: We don't need to be super smart here, but
             at least check that this approach (try to unify
             src with everything) is at least not pessimal
             for some reason. *)
          Util.for 0 (num_tmps - 1)
          (fn src_idx =>
           let val src = Vector.sub (tmps, src_idx)
           in
             if still_exists src
             then Util.for (src_idx + 1) (num_tmps - 1)
               (fn dst_idx =>
                let
                  val dst = Vector.sub (tmps, dst_idx)
                in
                  if still_exists src andalso
                     still_exists dst andalso
                     tmpsize src = tmpsize dst andalso
                     not (does_interfere src dst)
                  then coalesce src dst
                  else ()
                end)
             else ()
           end)
        end

      val () = app onefunc ` map #1 ` ISM.listItemsi all_tmps
    in
      !blocks
    end

  (* Currently we just do this in a dumb way, allocating each
     distinct temporary to a distinct slot. *)
  fun explicate blocks =
    let
      (* For each context, the set of temporaries mapped to
         their sizes. *)
      val contexts : sz SM.map ref SM.map ref = ref SM.empty

      fun getcontext f =
        case SM.find (!contexts, f) of
          SOME r => r
        | NONE =>
            let val r = ref SM.empty
            in
              contexts := SM.insert (!contexts, f, r);
              r
            end

      fun observetmp (func, name, size) =
        let
          val ctx = getcontext func
        in
          case SM.find (!ctx, name) of
            SOME oldsize =>
              if size = oldsize
              then ()
              else raise AllocateTmps ("Same tmp name used at inconsistent " ^
                                       "sizes: " ^ func ^ "." ^ name)
          | NONE => ctx := SM.insert (!ctx, name, size)
        end

      fun gather (tmp as N { func, name, size }) =
                  (observetmp (func, name, size); tmp)

      fun gathertmps (Block { name = lab, tmp_frame, cmds : named_cmd list }) =
        Block { name = lab,
                tmp_frame = tmp_frame,
                cmds = map (map_cmd gather Util.I) cmds }

      (* First pass simply gathers the used temporaries so that we can
         allocate them. *)
      val blocks : named_block list = map gathertmps blocks

      (* For one function's data... *)
      fun allocatefn (ctx : sz SM.map ref) =
        let
          val next = ref 0
          (* Allocate one tmp to the next spot; updating pos map *)
          fun folder (name, sz, pos) =
            let
              val bytes = szbytes sz
              val pos = SM.insert (pos, name, (!next, sz))
            in
              next := !next + bytes;
              pos
            end
          val pos = SM.foldli folder SM.empty (!ctx)
        in
          { size = !next, pos = pos }
        end

      val layout = SM.map allocatefn (!contexts)

      (* Size of the temporary frame for the named function. It is possible
         for this to be 0, if we never observed any temporaries for that
         function. *)
      fun context_size f =
        (case SM.find (layout, f) of
           NONE => 0
         | SOME { size, pos = _ } => size)

      fun rewrite_off (Named func) = Explicit (context_size func)

      fun rewrite_tmp (N { func, name, size }) =
        (case SM.find (layout, func) of
           NONE => raise AllocateTmps ("bug: rewrite didn't find tmp context " ^
                                       func ^ "?")
         | SOME { size = _, pos } =>
             (case SM.find (pos, name) of
                NONE => raise AllocateTmps ("bug: rewrite didn't find tmp " ^
                                            name ^ " in context " ^ func)
              | SOME (offset, sz) => E { offset = offset, size = sz,
                                         comment = func ^ "." ^ name }))

      (* Next, just allocate in map order, and compute the size for
         each context. Rewrite matching SaveTempsNamed and RestoreTempsNamed,
         plus each tmp occurrence. *)
      fun rewrite_cmd (cmd : named_cmd) : explicit_cmd =
        map_cmd rewrite_tmp rewrite_off cmd

      fun rewrite_block (Block { name, tmp_frame, cmds }) =
        Block { name = name,
                tmp_frame = rewrite_off tmp_frame,
                cmds = map rewrite_cmd cmds }

      val blocks : explicit_block list = map rewrite_block blocks

      val layout_table =
        ["func", "offset", "tmp"] ::
        List.concat
        (map (fn (func, { size, pos }) =>
              [func, "", "(total " ^ Int.toString size ^ ")"] ::
              (* Convert integral offset to text for table *)
              map (fn (offset, txt) => ["", Int.toString offset, txt])
              (* Sort by integral offset. *)
              (ListUtil.sort (ListUtil.byfirst Int.compare)
               (* Flatten tmp positions. *)
               (map (fn (name, (offset, sz)) =>
                     (offset, name ^ " [" ^ Int.toString (szbytes sz) ^ "]"))
                (SM.listItemsi pos))))
         (SM.listItemsi layout))

    in
      print (StringUtil.table 80 layout_table);
      blocks
    end

  fun allocate (Program { blocks, frame_stack_start, datasegment }) =
    let
      val liveness = Liveness.liveness blocks

      val blocks = coalesce liveness blocks
      val blocks = explicate blocks
    in
      (* Data segment doesn't change. *)
      Program { blocks = blocks,
                frame_stack_start = frame_stack_start,
                datasegment = datasegment }
    end
end
