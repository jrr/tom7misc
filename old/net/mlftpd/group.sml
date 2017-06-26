(* should share a functor with Passwd, probably *)
(* inefficient *)

structure Group :> GROUP =
struct

   type entry = {name : string,
                 gid : int,
                 members : string list}

   exception Group of string

   type ('a, 'b) dict = ('a * 'b) list
   val empty = nil
   val insert = op ::

   val db = (ref nil) : (string, entry) dict ref

   fun parseline l =
        case String.fields (fn c => c = #":") l of
             [name, _, gid, members] =>
                 ({name = name, gid = Option.valOf (Int.fromString gid), 
                   members = String.fields (fn c => c = #",") members}
                    handle _ => raise Group "bad line in group file")
           | _ => raise Group "bad line in group file"

   fun readdb s =
        let
             val f = TextIO.openIn s

             fun go d =
                case TextIO.inputLine f of
                    NONE => d (* EOF *)
                  | SOME l => 
                        let val p = parseline 
                            (StringUtil.losespecr 
                             StringUtil.whitespec l)
                        in go (insert ((#name p, p),d))
                        end
             val dict = go empty
        in
             TextIO.closeIn f;
             db := dict
        end

    fun lookup s = ListUtil.Alist.find op= (!db) s

    fun lookupgid u =
      let
        fun f ((_, entry : entry as {gid,...})::t) = 
              if u = gid then SOME entry
              else f t
          | f nil = NONE
      in
        f (!db)
      end

    fun memberships s =
        List.mapPartial
        (fn (_, e as {members, ...} : entry) =>
         if List.exists (fn x => x = s) members
         then SOME e
         else NONE) (!db)

end
