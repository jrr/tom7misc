
(* Global Pairwise Sequence Alignment *)
structure GPSA =
struct

  (* perhaps functorize over these *)
  val gapscore = ~4
  fun score (c, d) =
    if c = d then 4
    else ~2
  (* XXX be smarter about meta-nucleotides *)


  (* tuned this for speed; not sure it is correct
     any more (XX check against below) *)
  fun bestalignment (s1, s2) =
    let

      val n1 = size s1
      val n2 = size s2

      val a = Unsafe.Array.create ((n1 + 1) * (n2 + 1), 0)

      (* fill in first row, column *)
      val _ =
        Util.for 0 n1
        (fn x =>
         Array.update(a, x, gapscore * x))
      val _ =
        Util.for 0 n2
        (fn y =>
         Array.update(a, y * n1, gapscore * y))

      (* best alignment of 
         the x-length prefix of s1 with
         the y-length prefix of s2. *)
      fun ba x y =
        let in
          Array.sub(a, y * n1 + x)
        end

      (* fill in the cell x,y *)
      fun cba x y =
          (* compute *)
          let 
            val yi = y - 1
            val xi = x - 1

            val diag = ba (x - 1) (y - 1) + 
              score(String.sub(s1, xi),
                    String.sub(s2, yi))

            val up = ba x (y - 1) + gapscore
            val left = ba (x - 1) y + gapscore

            val res = Int.max(Int.max(up, left), diag)
          in
            Array.update(a, y * n1 + x, res)
          end

      (* initializes matrix to avoid
         deep recursion *)
      val _ =
        Util.for 1 n1
        (fn x =>
         Util.for 1 n2
         (fn y =>
          cba x y))

      val ans = ba n1 n2

    in
      ans
    end

  fun bestalignment_recursive (s1, s2) =
    let
      val dummy = 
        case Int.maxInt of
          (* assume nothing scores better than 10 *)
          NONE => size s1 * size s2 * 10 + 1
        | SOME mi => mi

      val n1 = size s1
      val n2 = size s2

      (* indices into this array are offset by
         one: the row and column for zero-length
         prefixes is not represented, and the
         entry for the full match is at 
         (n1-1, n2-1). *)
      val a = Array.array (n1 * n2, dummy)

      (* best alignment of 
         the x-length prefix of s1 with
         the y-length prefix of s2. *)
      fun ba 0 y = gapscore * y
        | ba x 0 = gapscore * x
        | ba x y =
        let 
          val yi = y - 1
          val xi = x - 1
          val cur = Array.sub(a, yi * n1 + xi)
        in
          if cur = dummy
          then 
            (* compute *)
            let 
              val diag = ba (x - 1) (y - 1) + 
                score(String.sub(s1, xi),
                      String.sub(s2, yi))

              (* PERF: could do a sort of alpha-beta pruning
                 here; if we pass along a "beat this" param,
                 then we can sometimes know that we're screwed
                 when we have to make up a deficit that's more
                 than distance-from-origin * max-similarity
                 (actually distance-from-origin is not accurate.
                 if we go along a diagonal, then we can make up
                 max-similarity * diag-length. But if we have to
                 go vertically or horizontally, then we will incur
                 a gap penalty! So it's something like 
                 best-possible = 
                   abs(xpos - ypos) * gapscore + 
                   min(xpos, ypos) * maxscore          *)

              val up = ba x (y - 1) + gapscore
              val left = ba (x - 1) y + gapscore


              val res = Int.max(Int.max(up, left), diag)
            in
              Array.update(a, yi * n1 + xi, res);
              res
            end
          else cur
        end

      (* initializes matrix to avoid
         deep recursion *)
      val _ =
        Util.for 1 n1
        (fn x =>
         Util.for 1 n2
         (fn y =>
          ba x y))

      val ans = ba n1 n2

(* 
   (* debugging *)

      fun matrix y =
        if y >= n2
        then nil
        else List.tabulate(n1, 
                           fn x => Int.toString (Array.sub(a, y * n1 + x)))
             :: matrix (y + 1)

      val tab = map str (explode s2) :: ListUtil.transpose (matrix 0)
      val tab = ("" :: map str (explode s1)) :: ListUtil.transpose tab
      val _ = print (StringUtil.table 75 tab)
*)
    in
      ans
    end


end
