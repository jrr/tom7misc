structure OptimizeAsm :> OPTIMIZEASM =
struct
  infixr 9 `
  fun a ` b = a b

  exception OptimizeAsm of string

  open ASM

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

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

      (* TODO: Find unused labels, and just drop them. This can allow
         us to get more efficient splits later, or cheaper if short
         blocks are coalesced. *)
      val actually_used = ref (SM.empty : unit SM.map)

      fun rewrite l =
        let in
          actually_used := SM.insert (!actually_used, l, ());
          case SM.find (!canonized, l) of
            SOME ll => ll
          | NONE => raise OptimizeAsm ("Use of undefined label? " ^ l)
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
    in
      map rewrite_labels blocks
    end

  fun optimize (Program { blocks, frame_stack_start, datasegment }) =
    let val blocks = simplify_labels blocks
    in
      Program { blocks = blocks, frame_stack_start = frame_stack_start,
                datasegment = datasegment }
    end
end
