
functor SetUtil(structure S : ORD_SET) : SETUTIL where type set = S.set
                                                   and type item = S.item =
struct

  type set = S.set
  type item = S.item

  fun fromlist l = List.foldr S.add' S.empty l
  fun tolist s = S.foldr op:: nil s
  fun mappartial f s =
    let
      fun folder (item, s) =
        case f item of
          NONE => s
        | SOME item' => S.add(s, item')
    in
      S.foldr folder S.empty s
    end

  fun fromvector l = Vector.foldr S.add' S.empty l
  fun tovector s =
    (* Need something to initialize the array with... *)
    case S.pop_head s of
      NONE => Vector.fromList nil
    | SOME (item, s) =>
        let
          val len = S.numItems s + 1
          val a = Array.array (len, item)
          (* Index 0 already has the right item. s is the tail *)
          val idx = ref 1
        in
          S.app (fn x =>
                 let in
                   Array.update (a, !idx, x);
                   idx := !idx + 1
                 end) s;
          Array.vector a
        end

end
