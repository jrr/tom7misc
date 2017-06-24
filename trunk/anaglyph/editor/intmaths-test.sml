structure IntMathsTest =
struct

  exception Failed of string
  structure IM = IntMaths

  fun ctos (x, y) = Int.toString x ^ "," ^ Int.toString y
  fun vtos (p1, p2) = ctos p1 ^ "->" ^ ctos p2
  fun ttos (a, b, c) = "(" ^ ctos a ^ ";" ^ ctos b ^ ";" ^ ctos c ^ ")"

  fun perms (x, y, z) =
    [(x, y, z),
     (x, z, y),
     (y, z, x),
     (y, x, z),
     (z, x, y),
     (z, y, x)]

  fun assert_overlapping (a, b, c) (d, e, f) =
    let
      val aperm = perms (a, b, c)
      val bperm = perms (d, e, f)
    in
      app (fn ta =>
           app (fn tb =>
                if IM.triangleoverlap ta tb
                then ()
                else raise Failed ("assert_overlap " ^ ttos ta ^
                                   " " ^ ttos tb)) bperm) aperm
    end

  fun assert_self_eq t =
    let
      val p = perms t
    in
      app (fn ta =>
           app (fn tb =>
                if IM.triangleseq ta tb
                then ()
                else raise Failed ("triangleseq " ^ ttos ta ^
                                   " " ^ ttos tb ^ ". Canonized: " ^
                                   ttos (IM.canonize ta) ^ " / " ^
                                   ttos (IM.canonize tb))) p) p
    end

  fun test_triangleseq () =
    let in
      assert_self_eq ((0, 0), (1, 1), (1, 0));
      assert_self_eq ((4, 8), (8, 0), (0, 0));
      assert_self_eq ((4, 6), (6, 2), (2, 1))
    end

  fun test_triangleoverlap () =
    let
      (*
                  a
                 / \
         c------/---\------c
          \    /  b  \    /
           \  /  / \  \  /
            \/  /   \  \/
            /\ b-----b /\
           /  \       /  \
          a---------------a
                \   /
                 \ /
                  c
          *)

      val a1 = (4, 8)
      val a2 = (8, 0)
      val a3 = (0, 0)

      val b1 = (4, 6)
      val b2 = (6, 2)
      val b3 = (2, 2)

      val c1 = (~2, 7)
      val c2 = (10, 7)
      val c3 = (4, ~2)
    in
      assert_overlapping (a1, a2, a3) (b1, b2, b3);
      assert_overlapping (a1, a2, a3) (a1, a2, a3);
      assert_overlapping (b1, b2, b3) (b1, b2, b3);

      assert_overlapping (a1, a2, a3) (c1, c2, c3);
      assert_overlapping (c1, c2, c3) (a1, a2, a3);

      assert_overlapping (b1, b2, b3) (c1, c2, c3);
      assert_overlapping (c1, c2, c3) (b1, b2, b3);

      ()
    end

  fun test () =
    let in
      test_triangleseq ();
      test_triangleoverlap ()
    end
  handle Failed s =>
    let in
      print ("Test failed: " ^ s ^ "\n");
      OS.Process.exit OS.Process.failure
    end

end

val () = IntMathsTest.test ()

