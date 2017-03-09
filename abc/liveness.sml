structure Liveness :> LIVENESS =
struct
  infixr 9 `
  fun a ` b = a b

  exception Liveness of string

  open ASM

  structure SM = ImperativeMapFn(type ord_key = string
                                 val compare = String.compare)
  structure IM = ImperativeMapFn(type ord_key = int
                                 val compare = Int.compare)
  structure SS = SplaySetFn(type ord_key = string
                            val compare = String.compare)
  structure TS = SplaySetFn(type ord_key = ASM.named_tmp
                            val compare = ASM.named_tmpcompare)
  structure IS = SplaySetFn(type ord_key = int
                            val compare = Int.compare)

  local
    fun tset l = foldl TS.add' TS.empty l
    val empty = TS.empty
  in
    fun condreads cond =
      case cond of
        Below (a, b) => tset [a, b]
      | BelowEq (a, b) => tset [a, b]
      | Less (a, b) => tset [a, b]
      | LessEq (a, b) => tset [a, b]
      | Eq (a, b) => tset [a, b]
      | NotEq (a, b) => tset [a, b]
      | EqZero t => tset [t]
      | NeZero t => tset [t]
      | True => empty

    fun writesreads cmd =
      case cmd of
        SaveTemps _ => (empty, empty)
      | RestoreTemps _ => (empty, empty)
      | ExpandFrame _ => (empty, empty)
      | ShrinkFrame _ => (empty, empty)
      | FrameOffset (t, _) => (tset [t], empty)
      | Label _ => (empty, empty)
      | Nop => (empty, empty)
      | Load8 (dst, addr) => (tset [dst], tset [addr])
      | Load16 (dst, addr) => (tset [dst], tset [addr])
        (* Note these read both *temporaries*, writing to *memory*. *)
      | Store8 (addr, src) => (empty, tset [addr, src])
      | Store16 (addr, src) => (empty, tset [addr, src])
      | Immediate16 (t, _) => (tset [t], empty)
      | Immediate32 (t, _) => (tset [t], empty)
      | Add (a, b) => (tset [a], tset [a, b])
      | Sub (a, b) => (tset [a], tset [a, b])
      | Complement t => (tset [t], tset [t])
      | Mov (a, b) => (tset [a], tset [b])
      | Xor (a, b) => (tset [a], tset [a, b])
      | And (a, b) => (tset [a], tset [a, b])
      | Push t => (empty, tset [t])
      | Pop t => (tset [t], empty)
      | LoadLabel (t, _) => (tset [t], empty)
      | PopJumpInd => (empty, empty)
      | JumpCond (cond, _) => (empty, condreads cond)
      | Putc t => (empty, tset [t])
      | Out8 (a, b) => (empty, tset [a, b])
      | Init => (empty, empty)
      | Exit => (empty, empty)
  end

  (* Note: We use a "frame" discipline in ASM programs so that two
     temporaries from different frames cannot interfere. As a result,
     we could probably prevent the propagation of some temporaries
     through PopJumpInd (i.e., from one frame to another).
     AllocateTmps already handles this non-interference, so the
     only purpose might be to make this converge faster. *)

  (* Perform liveness analysis for the program. Outputs for each block a
     vector of the same length as the number of commands. Each element
     of the vector contains the set of live variables (conservative
     over-approximation) on the in-edge of the instruction and on the
     out-edge. *)
  fun liveness (blocks : ASM.named_block list) =
    let
      (* Start index for each block. *)
      val start_idx = SM.empty () : int SM.map
      fun get_start_idx lab =
        case SM.find (start_idx, lab) of
          NONE => raise Liveness ("couldn't find label " ^ lab ^ "?")
        | SOME idx => idx
      (* Reverse map, used for debugging. *)
      val idx_labels = IM.empty () : string list IM.map

      (* A block can only be the target of an indirect jump
         if we take its address. This is the set of all such
         blocks; we consider that anything in here is targeted
         by any PopJumpInd. *)
      val escaping_blocks = ref SS.empty

      (* Indices that contain outgoing indirect jumps. *)
      val indjump_idx = ref IS.empty

      (* The liveness algorithm is a backwards CFG dataflow
         analysis. Since this iterates backwards it is much
         more straightforward on vectors than lists. Linearize
         the entire program.

         offset is the current instruction number.
         *)
      fun linearize_block (offset, nil) = nil
        | linearize_block (offset,
                           Block { name, tmp_frame, cmds } :: rest) =
        let in
          SM.insert (start_idx, name, offset);
          IM.update (idx_labels, offset,
                     (fn NONE => [name] | SOME l => name :: l));
          linearize_cmds (offset, name, rest, cmds)
        end
      and linearize_cmds (offset, name, rest, nil) =
        linearize_block (offset, rest)
        | linearize_cmds (offset, name, rest, cmd :: cmds) =
        let in
          (case cmd of
             LoadLabel (_, lab) =>
               escaping_blocks := SS.add (!escaping_blocks, lab)
           | Label _ =>
               raise Liveness "liveness can't deal with meta command Label"
           | PopJumpInd => indjump_idx := IS.add (!indjump_idx, offset)
           | _ => ());
          cmd :: linearize_cmds (offset + 1, name, rest, cmds)
        end

      val cmds = Array.fromList `linearize_block (0, blocks)
      val num_cmds = Array.length cmds

      (* Now compute the input and output flow edges for each
         command index. *)
      val inedges = Array.array (num_cmds, IS.empty)
      val outedges = Array.array (num_cmds, IS.empty)

      (* Make an out edge from src to dst
         and an in edge into dst from src *)
      fun make_edge (src, dst) =
        let in
          Array.update (outedges, src,
                        IS.add (Array.sub (outedges, src), dst));
          Array.update (inedges, dst,
                        IS.add (Array.sub (inedges, dst), src))
        end

      (* Compute the inedges and outedges sets for the command cmd
         which is at index idx. *)
      fun compute_edges (idx, cmd) =
        let in
          (if no_fallthrough cmd
           then ()
           else if idx + 1 < num_cmds
                then make_edge (idx, idx + 1)
                (* Should this be an error?
                   I guess some optimization could determine that
                   a JumpCond would always happen (but then why not
                   rewrite it?) *)
                else raise Liveness "program falls off the end?");

          (* Jumps can target non-adjacent labels. *)
          (case cmd of
             JumpCond (_, dst) =>
               let val dstidx = get_start_idx dst
               in make_edge (idx, dstidx)
               end
           | PopJumpInd =>
               (* Edge to any escaping block. *)
               (SS.app (fn dst =>
                        let val dstidx = get_start_idx dst
                        in make_edge (idx, dstidx)
                        end) (!escaping_blocks))
           | _ => ())
        end

      (* PERF: We could also trace from the first instruction and just keep
         a set of to-do edges. This would allow us to ignore dead loops. *)
      val () = Array.appi compute_edges cmds

      val inlive = Array.array (num_cmds, TS.empty)
      val outlive = Array.array (num_cmds, TS.empty)

      fun array_eq eq (a1, a2) =
        if Array.length a1 <> Array.length a2
        then raise Liveness "bug: arrays wrong length"
        else
          let
            fun aeq idx =
              idx >= Array.length a1 orelse
              (eq (Array.sub (a1, idx),
                   Array.sub (a2, idx)) andalso aeq (idx + 1))
          in
            aeq 0
          end

      (* Now, propagate liveness! *)
      (* This schedule could be smarter (topological sort), but as long
         as we are going from the end to the beginning it will converge
         reasonably quickly. *)
      fun dataflow_round rounds =
        let
          (* PERF! Can instead check whether we make any updates? *)
          val oldin = Array.tabulate (num_cmds,
                                      fn i => Array.sub (inlive, i))
          val oldout = Array.tabulate (num_cmds,
                                       fn i => Array.sub (outlive, i))

          (* Do the dataflow computation for one index. *)
          fun flow idx =
            let
              val ins = Array.sub (inlive, idx)
              val outs = Array.sub (outlive, idx)

              val (writes, reads) = writesreads (Array.sub (cmds, idx))

              (* New live-in is anything that I use,
                 plus anything that's live-out and I don't
                 clobber. *)
              val ins = TS.union (reads,
                                  TS.difference (outs, writes))

              fun get_out nil = outs
                | get_out (succ :: rest) =
                TS.union (Array.sub (inlive, succ), get_out rest)

              val successors = IS.listItems ` Array.sub (outedges, idx)
              val outs = get_out successors
            in
              Array.update (inlive, idx, ins);
              Array.update (outlive, idx, outs)
            end


          fun inreverse idx =
            if idx < 0 then ()
            else (flow idx; inreverse (idx - 1))
        in
          inreverse (num_cmds - 1);

          if array_eq TS.equal (oldin, inlive) andalso
             array_eq TS.equal (oldout, outlive)
          then print ("Liveness converged after " ^ Int.toString rounds ^
                      " round(s).\n")
          else dataflow_round (rounds + 1)
        end

      val () = dataflow_round 1

      fun tohtml () =
        let
          val rows = ref nil
          fun tsetstr s =
            let val l = TS.listItems s
            in StringUtil.delimit ", " (map ASM.named_tmptos l)
            end
        in
          Util.for 0 (num_cmds - 1)
          (fn idx =>
           rows :=
           ((case IM.find (idx_labels, idx) of
              NONE => ""
            | SOME labs =>
                String.concat
                (map (fn lab => "<tr><td></td><td><b>" ^ lab ^
                      ":</b></td></tr>\n")
                 labs)) ^
            "<tr" ^ (if idx mod 2 = 0
                     then " style=\"background:#F7FAF7\""
                     else "") ^ "><td>" ^
            Int.toString idx ^ "</td><td>" ^
            ASM.named_cmdtos (Array.sub (cmds, idx)) ^ "</td><td>" ^
            StringUtil.delimit ", " (map Int.toString `
                                     IS.listItems `
                                     Array.sub (inedges, idx)) ^ "</td><td>" ^
            StringUtil.delimit ", " (map Int.toString `
                                     IS.listItems `
                                     Array.sub (outedges, idx)) ^ "</td><td>" ^
            tsetstr (Array.sub (inlive, idx)) ^ "</td><td>" ^
            tsetstr (Array.sub (outlive, idx)) ^ "</td></tr>\n") :: !rows);

          "<!doctype html>\n" ^
          "<style>body { font: 12px monospace }\n" ^
          "table { border-collapse: collapse }\n" ^
          "td { padding:2px }</style>\n" ^
          "<table>\n" ^
          "<tr><td>idx</td><td>cmd</td><td>in</td><td>out</td>" ^
          "<td>live in</td><td>live out</td><tr>\n" ^
           String.concat (rev (!rows)) ^
          "</table>\n"
        end

      val () = StringUtil.writefile "liveness.html" (tohtml ())
    in
      { cmds = cmds,
        inlive = inlive,
        outlive = outlive }
    end

end