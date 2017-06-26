structure SuDoku =
struct
    (* there are dim*dim digits. *)
    val dim = 3
    (* must be at least 2<<(dim+1) + 1 bits *)
    (* PERF word might be cheaper; no overflow, but
       we never do arithmetic anyway *)
    (* structure I = Int8 *)
      structure I = Int

    (* print a digit as a constant-width string *)
    val pdigit = I.toString
    val pempty = "-"

    exception SuDoku of string

    (* of size dim * dim * dim.
       if a cell contains 0, then it is "empty",
       otherwise it contains a "digit" 1-dim inclusive.
       *)
    type doku = I.int array

    val width = dim * dim

    (* give the coordinate of the top left of a box *)
    fun boxi b =
        let
            val r = b div dim
            val c = b mod dim
        in
            dim * (r * width + c)
        end

    fun idx box row col = 
        let val i =
            row * width + col + boxi box
        in
            (*
            print ("idx for " ^ Int.toString box ^ "/" ^
                   Int.toString row ^ "/" ^ Int.toString col ^
                   ": " ^ Int.toString i ^ "\n"); *)
            i
        end

    fun set a box row col n =
        Array.update(a, idx box row col, n)
        
    fun get a box row col = 
        Array.sub(a, idx box row col)

    val seed = ref (Word32.fromInt (LargeInt.toInt 
                                    (LargeInt.mod 
                                     (Time.toSeconds (Time.now()), 
                                      0xFFFFFF))))

    (* val seed = ref (0wx993 : Word32.word) *)
        
    (* give a "random" number 0..width-1 *)
    fun randi () =
        let in
            seed := Word32.* (!seed, 0wxDEADBEEF);
            seed := Word32.xorb (!seed, 0wx17138811);
            seed := Word32.orb (Word32.<<(!seed, 0w5),
                                Word32.>>(!seed, 0w31 - 0w5));
            seed := Word32.+ (!seed, 0w7);
            (width + (Word32.toIntX(Word32.andb(!seed, 0wxFFFFFF)) mod width))
            mod width
        end
        
    fun prdoku a =
        let
        in
            Util.for 0 (width - 1)
            (fn r =>
             let in
                 Util.for 0 (width - 1)
                 (fn c =>
                  let in
                      (case Array.sub(a, r * width + c) of
                           0 => print pempty
                         | d => print (pdigit d));
                           print " ";
                           (* maybe box separator *)
                           (if (c mod dim) = (dim - 1)
                                andalso c <> width - 1
                            then print " "
                            else ())
                  end);
                 print "\n";
                 (if (r mod dim) = (dim - 1)
                      andalso r <> width - 1
                  then print "\n"
                  else ())
             end)
        end

    local exception No
    in
        fun islegal verbose a =
            let 
                fun column c =
                    Util.for 0 (width - 2)
                    (fn r =>
                     let val x = Array.sub(a, r * width + c)
                     in
                         (* allow zeroes *)
                         if x = 0 then ()
                         else
                         Util.for (r + 1) (width - 1)
                         (fn r' =>
                          let val x' = Array.sub(a, r' * width + c)
                          in
                              if x = x' then 
                                 let in
                                     if verbose then
                                        print ("col " ^ Int.toString c ^ 
                                               " is illegal because indices " ^
                                               Int.toString r ^ " & " ^
                                               Int.toString r' ^ " have " ^
                                               I.toString x ^ "\n")
                                     else ();
                                     raise No
                                 end
                              else ()
                          end)
                     end)

                fun row r =
                    Util.for 0 (width - 2)
                    (fn c =>
                     let val x = Array.sub(a, r * width + c)
                     in
                         if x = 0 then ()
                         else
                         Util.for (c + 1) (width - 1)
                         (fn c' =>
                          let val x' = Array.sub(a, r * width + c')
                          in
                              if x = x' then 
                                 let in
                                     if verbose then
                                        print ("row " ^ Int.toString r ^ 
                                               " is illegal because indices " ^
                                               Int.toString c ^ " & " ^
                                               Int.toString c' ^ " have " ^
                                               I.toString x ^ "\n")
                                     else ();
                                     raise No
                                 end
                              else ()
                          end)
                     end)

                fun box b =
                    Util.for 0 (width - 2)
                    (fn i =>
                     let val x = get a b (i div dim) (i mod dim)
                     in
                         if x = 0 then ()
                         else
                         Util.for (i + 1) (width - 1)
                         (fn i' =>
                          let val x' = get a b (i' div dim) (i' mod dim)
                          in
                              if x = x' then 
                                 let in
                                     if verbose then
                                     print ("box " ^ Int.toString b ^ 
                                            " is illegal because indices " ^
                                            Int.toString i ^ " & " ^
                                            Int.toString i' ^ " have " ^
                                            I.toString x ^ "\n")
                                     else ();
                                     raise No
                                 end
                              else ()
                          end)
                     end)

            in
                Util.for 0 (width - 1) column;
                Util.for 0 (width - 1) row;
                Util.for 0 (width - 1) box;
                true
            end handle No => false
    end

    (* must be a full board; check if it's a legal solution *)
    fun checklegal a = 
        let in
            if islegal true a
            then ()
            else
            let in
                print "Bug: ILLEGAL:\n";
                prdoku a;
                raise SuDoku "board is illegal"
            end
        end

    val ` = 0


end

structure SD = SuDoku