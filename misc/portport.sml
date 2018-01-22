structure WordsMade =
struct

  structure SM = SplayMapFn(type ord_key = string
                            val compare = String.compare)

  val words = Script.linesfromfile "../../tom7misc/manarags/wordlist.asc"
  val words = map StringUtil.lcase words

  val dict = ref SM.empty : unit SM.map ref
  val () = app (fn s =>
                dict := SM.insert (!dict, s, ())) words
  fun isword w = Option.isSome (SM.find (!dict, w))

  fun oneword s =
    let
      val best : (string * int) option ref = ref NONE
      fun observe (b as (bpfx, boverlap)) =
        case !best of
          NONE => best := SOME b
        | SOME (pfx, overlap) =>
            (case Int.compare (boverlap, overlap) of
               GREATER => best := SOME b
             | LESS => ()
             | EQUAL => if size bpfx > size pfx
                        then best := SOME b
                        else ())

      fun oneprefix n =
        let
          val pfx = String.substring(s, 0, n)
          fun onesuffix m =
            let
              val sfx = String.substring(s, size pfx - m, m)
            in
              if StringUtil.matchhead sfx s
              then observe (pfx, m)
              else ()
            end
        in
          if isword pfx
             andalso s <> (pfx ^ "s")
             andalso s <> (pfx ^ "ed")
          then Util.for 1 (size pfx - 1) onesuffix
          else ()
        end

    in
      Util.for 0 (size s - 1) oneprefix;
      case !best of
        SOME (pfx, n) => SOME (s, pfx, n)
      | NONE => NONE
    end

  val possible = List.mapPartial oneword words

  fun bylength ((_, _, olap), (_, _, olapp)) = Int.compare (olapp, olap)
  val possible = ListUtil.sort bylength possible
  val () = app (fn (s, pfx, m) =>
                print (s ^ " = " ^ pfx ^ " + " ^ s ^ " (" ^ Int.toString m ^
                       "\n")) possible
end
