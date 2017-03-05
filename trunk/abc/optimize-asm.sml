structure OptimizeAsm :> OPTIMIZEASM =
struct
  infixr 9 `
  fun a ` b = a b

  exception OptimizeAsm of string

  open ASM

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  (* PERF: Drop 'nop' *)

  (* PERF: Drop commands after one that unconditionally jumps. *)

  (* PERF: Do flow analysis to determine live blocks; recursive dead
     functions (or probably even dead loops) cannot be dropped with
     the current approach. *)

  (* Simplify labels by coalescing adjacent labels into one. *)
  fun simplify_labels blocks =
    let
      val canonized = ref (SM.empty : string SM.map)
      fun makecanon (empties,
                     (block as Block { name, cmds = nil, tmp_frame }) :: t) =
        makecanon (block :: empties, t)
        | makecanon (empties,
                     (block as Block { name, cmds, tmp_frame }) :: rest) =
        let in
          (* Map every label to name (both empties and itself). *)
          app (fn (Block { name = l, ... }) =>
               canonized := SM.insert (!canonized, l, name)) (block :: empties);
          app (fn (Block { name = l, ... }) =>
               print ("Drop empty block " ^ l ^ " -> " ^ name ^ "\n")) empties;
          block :: makecanon (nil, rest)
        end
        | makecanon (nil, nil) = nil
        | makecanon (empties, nil) =
        let in
          print ("Weird: Program ends with empty labels...\n");
          (* So that all labels are in the domain... *)
          app (fn (Block { name = l, ... }) =>
               canonized := SM.insert (!canonized, l, l)) empties;
          rev empties
        end
      val blocks = makecanon (nil, blocks)

      (* Mark used labels so that we can potentially drop unused
         blocks. This saves code size and can result in more efficient
         splits and cheaper jumps. *)
      val actually_used = ref (SM.empty : unit SM.map)
      (* The first block is externally targeted. *)
      val () = case blocks of
        nil => raise OptimizeAsm "NO blocks?!"
      | Block { name, ... } :: _ =>
          actually_used := SM.insert (!actually_used, name, ())

      fun rewrite l =
        let
          val canon =
            case SM.find (!canonized, l) of
              SOME ll => ll
            | NONE => raise OptimizeAsm ("Use of undefined label? " ^ l)
        in
          actually_used := SM.insert (!actually_used, canon, ());
          canon
        end
      fun rewrite_cmd cmd =
        case cmd of
          Label _ => raise OptimizeAsm ("OptimizeAsm should not get code " ^
                                        "with Label meta-command.")
        | LoadLabel (t, lab) => LoadLabel (t, rewrite lab)
        | JumpCond (c, lab) => JumpCond (c, rewrite lab)
        | cmd => cmd

      fun rewrite_labels (Block { name, cmds, tmp_frame }) =
        Block { name = name,
                cmds = map rewrite_cmd cmds,
                tmp_frame = tmp_frame }

      val blocks = map rewrite_labels blocks

      val () = SM.appi (fn (lab, ()) => print ("Used: " ^ lab ^ "\n"))
        (!actually_used)

      fun unused lab =
        case SM.find (!actually_used, lab) of
          SOME () => false
        | NONE => true

      (* Returns true if the final command in the list is one that
         never falls through. *)
      fun no_fallthroughs nil = false
        | no_fallthroughs [last] = ASM.no_fallthrough last
        | no_fallthroughs (_ :: t) = no_fallthroughs t

      (* Now drop labels that are never targeted. We can coalesce a block
         into the previous one (block2 onto block) when the temp
         frames are the same. (PERF: If we had a way of setting the
         temp frame with a command, we could use that here.) We can
         completely drop an unused block2 if block can't fall through
         to it. *)
      fun drop_blocks ((block as Block { name, cmds, tmp_frame }) ::
                       (block2 as Block { name = name2, cmds = cmds2,
                                          tmp_frame = tmp_frame2 }) ::
                       rest) =
        if unused name2
        then
          if tmp_frame = tmp_frame2
          then
          let in
            print ("Coalescing block with unused label " ^ name2 ^ " into " ^
                   name ^ ".\n");
            drop_blocks (Block { name = name,
                                 cmds = cmds @ cmds2,
                                 tmp_frame = tmp_frame } :: rest)
          end
          else if no_fallthroughs cmds
               then
                 let in
                   print ("Completely unused block " ^ name2 ^ " since " ^
                          name ^ " cannot fallthrough into it.\n");
                   drop_blocks (block :: rest)
                 end
               else block :: drop_blocks (block2 :: rest)
        else block :: drop_blocks (block2 :: rest)
        | drop_blocks blocks = blocks
    in
      drop_blocks blocks
    end

  fun optimize (Program { blocks, frame_stack_start, datasegment }) =
    let val blocks = simplify_labels blocks
    in
      Program { blocks = blocks, frame_stack_start = frame_stack_start,
                datasegment = datasegment }
    end
end
