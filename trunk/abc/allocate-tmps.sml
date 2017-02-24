structure AllocateTmps :> ALLOCATETMPS =
struct

  exception AllocateTmps of string

  open ASM

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  (* Currently we just do this in a dumb way, allocating each
     distinct temporary to a distinct slot. *)
  fun allocate (Program { blocks, datasegment }) =
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

      fun map_tmp (f : 'a -> 'b) (cmd : 'a cmd) : 'b cmd =
        case cmd of
          (* These will get updated later *)
          SaveTempsNamed a => SaveTempsNamed a
        | SaveTempsExplicit _ =>
          raise AllocateTmps "not explicit SaveTempsExplicit yet?"
        | RestoreTempsNamed a => RestoreTempsNamed a
        | RestoreTempsExplicit _ =>
          raise AllocateTmps "not explicit RestoreTempsExplicit yet?"
        | ExpandFrame a => ExpandFrame a
        | ShrinkFrame a => ShrinkFrame a
        | FrameOffset (tmp, w16) => FrameOffset (f tmp, w16)
        | Label lab => Label lab
        | Load8 (a, b) => Load8 (f a, f b)
        | Load16 (a, b) => Load16 (f a, f b)
        | Store8 (a, b) => Store8 (f a, f b)
        | Store16 (a, b) => Store16 (f a, f b)
        | Immediate8 (tmp, w) => Immediate8 (f tmp, w)
        | Immediate16 (tmp, w) => Immediate16 (f tmp, w)
        | Immediate32 (tmp, w) => Immediate32 (f tmp, w)
        | Add (a, b) => Add (f a, f b)
        | Sub (a, b) => Sub (f a, f b)
        | Complement tmp => Complement (f tmp)
        | Mov (a, b) => Mov (f a, f b)
        | Xor (a, b) => Xor (f a, f b)
        | Push tmp => Push (f tmp)
        | Pop tmp => Pop (f tmp)
        | LoadLabel (tmp, s) => LoadLabel (f tmp, s)
        | PopJumpInd => PopJumpInd
        | JumpCond (cond, lab) => JumpCond (map_cond f cond, lab)
        | Init => Init

      fun gather (tmp as N { func, name, size }) =
                  (observetmp (func, name, size); tmp)

      fun gathertmps (lab, cmds : named_tmp cmd list) =
        (lab, map (map_tmp gather) cmds)

      val blocks : named_tmp block list = map gathertmps blocks

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
      fun rewritecmd cmd =
        case cmd of
          SaveTempsNamed a => SaveTempsExplicit (context_size a)
        | SaveTempsExplicit _ =>
          raise AllocateTmps "[rewrite] not explicit SaveTempsExplicit yet?"
        | RestoreTempsNamed a => RestoreTempsExplicit (context_size a)
        | RestoreTempsExplicit _ =>
          raise AllocateTmps "[rewrite] not explicit RestoreTempsExplicit yet?"
        | _ => map_tmp rewrite_tmp cmd

      fun rewrite (lab, cmds) = (lab, map rewritecmd cmds)

      val blocks : explicit_tmp block list = map rewrite blocks

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
               (* Data segment doesn't change. *)
      Program { blocks = blocks, datasegment = datasegment }
    end

end
