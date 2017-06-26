
(* Global Pairwise Sequence Alignment *)
structure GPSA_C =
struct

  fun bestalignment (s1, s2) =
    let
      val n1 = size s1
      val n2 = size s2

      val bestalignment_c =
        _import "ml_bestalignment" : int * CharVector.vector *
                                     int * CharVector.vector -> int ;
    in
      bestalignment_c (n1, s1, n2, s2)
    end

  (* XXX check that strings are all within radix,
     otherwise C code is unsafe *)
  fun bestalignment_mtx (radix, matrix, gapscore) (s1, s2) =
    let
      val n1 = size s1
      val n2 = size s2

      val bestalignment_c =
        _import "ml_bestalignment_mtx" : int * int Array.array * int *
                                         int * CharVector.vector *
                                         int * CharVector.vector -> int ;
    in
      bestalignment_c (radix, matrix, gapscore, n1, s1, n2, s2)
    end


end
