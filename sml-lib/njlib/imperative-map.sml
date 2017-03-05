(* PERF: This is just a ref cell containing a splay map,
   but I suspect that if we used SplayTree directly
   (or just implemented some new imperative map structure)
   it could be significantly more efficient. *)

functor ImperativeMapFn(K: ORD_KEY) :> IMPERATIVE_MAP
where type Key.ord_key = K.ord_key =
struct
  structure Key = K

  structure M = SplayMapFn(K)
  type 'a map = 'a M.map ref

  fun empty () = ref M.empty
  fun isempty m = M.isempty (!m)
  fun clear m = m := M.empty

  fun head m = M.head (!m)
  fun headi m = M.headi (!m)

  fun insert (m, k, v) = m := M.insert (!m, k, v)
  fun update (m, k, f) =
    (* PERF can be done with one lookup. *)
    insert (m, k, f (M.find (!m, k)))

  fun modify (m, f) = m := M.map f (!m)
  fun modifyi (m, f) = m := M.mapi f (!m)

  fun find (m, k) = M.find (!m, k)
  fun lookup (m, k) = M.lookup (!m, k)

  fun remove (m, k) =
    let
      val (mm, v) = M.remove (!m, k)
    in
      m := mm;
      v
    end

  fun erase (m, k) = m := M.erase (!m, k)

  fun numItems m = M.numItems (!m)

  fun listItems m = M.listItems (!m)
  fun listItemsi m = M.listItemsi (!m)

  fun collate ord (m1, m2) = M.collate ord (!m1, !m2)

  fun app f m = M.app f (!m)
  fun appi f m = M.appi f (!m)

  fun foldl f b m = M.foldl f b (!m)
  fun foldli f b m = M.foldli f b (!m)

  fun foldr f b m = M.foldr f b (!m)
  fun foldri f b m = M.foldri f b (!m)

  fun filter f m = m := M.filter f (!m)
  fun filteri f m = m := M.filteri f (!m)

end