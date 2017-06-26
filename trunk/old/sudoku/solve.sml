
structure Solve =
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
        
    val colors =
      Vector.fromList
      ["#000000",
       "#FF0000",
       "#FF8040",
       "#FFFF00",
       "#00FF00",
       "#0000FF",
       "#8D38C9",
       "#348781",
       "#804000",
       "#AAAAAA"]

    fun prdoku_color a =
      let in
      print "<table>\n";
       Util.for 0 (width - 1)
       (fn r =>
        let in
          print "<tr>";
            Util.for 0 (width - 1)
            (fn c =>
             let 
               val i = Array.sub(a, r * width + c)
             in
               print ("<td width=64 height=64 bgcolor=\"" ^ Vector.sub(colors, i) ^ "\">&nbsp;</td>");
                 (* maybe box separator *)
                 (if (c mod dim) = (dim - 1)
                      andalso c <> width - 1
                  then print "<td width=4 bgcolor=\"#000000\">&nbsp;</td>"
                  else ())
             end);
            print "</tr>\n";
            (if (r mod dim) = (dim - 1)
                 andalso r <> width - 1
             then print "<tr><td bgcolor=\"#000000\" height=4 colspan=12>&nbsp;</td></tr>\n"
             else ())
        end);
       print "</table>\n"
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

    (* XXX do analytically;
       this only works when dim = 3! *)
    fun easy () =
        Array.fromList 
           [1, 2, 3,   4, 5, 6,   7, 8, 9,
            4, 5, 6,   7, 8, 9,   1, 2, 3,
            7, 8, 9,   1, 2, 3,   4, 5, 6,

            2, 3, 1,   5, 6, 4,   8, 9, 7,
            5, 6, 4,   8, 9, 7,   2, 3, 1,
            8, 9, 7,   2, 3, 1,   5, 6, 4,

            3, 1, 2,   6, 4, 5,   9, 7, 8,
            6, 4, 5,   9, 7, 8,   3, 1, 2,
            9, 7, 8,   3, 1, 2,   6, 4, 5
            : I.int]

    fun boxnums () =
        Array.fromList 
           [1, 1, 1,   2, 2, 2,   3, 3, 3,
            1, 1, 1,   2, 2, 2,   3, 3, 3,
            1, 1, 1,   2, 2, 2,   3, 3, 3,

            4, 4, 4,   5, 5, 5,   6, 6, 6,
            4, 4, 4,   5, 5, 5,   6, 6, 6,
            4, 4, 4,   5, 5, 5,   6, 6, 6,

            7, 7, 7,   8, 8, 8,   9, 9, 9,
            7, 7, 7,   8, 8, 8,   9, 9, 9,
            7, 7, 7,   8, 8, 8,   9, 9, 9
            : I.int]

    val  ` = 0
    fun easy () =
        Array.fromList 
           [`, `, `,   4, `, `,   5, 6, 8,
            `, `, 2,   `, 9, 3,   `, `, `,
            `, 5, `,   `, `, 7,   `, `, `,

            `, 3, 7,   6, `, `,   `, `, 1,
            `, `, `,   `, 2, `,   `, `, `,
            8, `, `,   `, `, 1,   9, 7, `,

            `, `, `,   2, `, `,   `, 4, `,
            `, `, `,   1, 5, `,   7, `, `,
            5, 1, 8,   `, `, `,   `, `, `
            : I.int]


    fun easy () =
        Array.fromList 
           [`, `, `,   4, `, `,   5, 6, 8,
            `, `, 2,   `, 9, 3,   `, `, `,
            `, 5, `,   `, `, 7,   `, `, `,

            `, 3, 7,   6, `, `,   `, `, 1,
            `, `, `,   `, 2, `,   `, `, `,
            8, `, `,   `, `, 1,   9, 7, `,

            `, `, `,   2, `, `,   `, 4, `,
            `, `, `,   1, 5, `,   7, `, `,
            5, 1, 8,   `, `, `,   `, `, `
            : I.int]

    (* val verbose = true *)

    exception Done
      
    fun make () =
        let
            val a = easy ()
            val set = set a
            val get = get a

            fun sub i = Array.sub(a, i)
            fun subrc r c = sub (r * width + c)
            fun up i x = Array.update(a, i, x)
            fun uprc r c x = up (r * width + c) x

            (* is it legal to set position i to digit d? *)
            fun legalmove verbose i d =
                let 
                    val old = sub i
                    val _ = up i d

                    (* PERF omg don't look at the whole damn board!! *)
                    val ill = islegal verbose a
                in
                    up i old;
                    if verbose then
                       (if ill then print (" ! yes, " ^ I.toString d ^ "@" ^ 
                                           Int.toString i ^ " is legal\n")
                        else print (" .. " ^ I.toString d ^ "@" ^
                                    Int.toString i ^ " illegal\n"))
                    else ();
                    ill
                end

            (* is the board, as is, unsolvable? 
               the argument, if SOME i, indicates an index
               that may be an illegal placement--but the rest of
               the board must be legal. *)
            fun unsolvable () =
                (* for each 0 that appears in the matrix,
                   generate its list of legal moves.

                   if there are no zeroes, then return false
                   immediately.

                   if there are no legal moves for a zero, 
                   return true immediately.

                   otherwise, do search trying all possibilities, 
                   starting with the most constrained square. *)
                let
                    val strainedn = ref (dim * dim + 1)
                    val strainedl = ref nil
                    val strainedi = ref ~1

                    fun checkalts i =
                        if sub i = 0
                        then 
                            let val (alts, nalts) = 
                                let 
                                    val l = ref nil
                                    val n = ref 0
                                in
                                    Util.for 1 width
                                    (fn x =>
                                     if legalmove false i (I.fromInt x)
                                     then 
                                         let in
                                             n := !n + 1;
                                             l := (I.fromInt x) :: !l
                                         end
                                     else ());
                                    (* print ("there are " ^ Int.toString (!n) ^ " legals\n"); *)
                                    (!l, !n)
                                end
                            in
                                (* now, is this fewer than before? *)

                                (* PERF can shortcut if this is 0 *)
                                if nalts < !strainedn
                                then
                                    let in
                                        strainedn := nalts;
                                        strainedl := alts;
                                        strainedi := i
                                    end
                                else ()
                            end
                        else ()
                in
                    Util.for 0 (width * width - 1) checkalts;

                    (* print ("strainedi: " ^ Int.toString (!strainedi) ^ "\n"); *)
                    (* if there are no free squares, it is solvable
                       (solved) *)
                    if !strainedi = ~1
                    then 
                        let in
                            print "solution:\n";
                            prdoku_color a;
                            (* raise Done *)
                            ()
                        end
                    else
                        let in
                          (*
                            print ("Most constrained move: " ^
                                   Int.toString (!strainedn) ^ " at " ^
                                   Int.toString (!strainedi) ^ "\n");
                            *)

                            (* for the chosen free square, it must be
                               unsolvable no matter what legal play
                               we make. *)
                            (foldl (fn (d, b) =>
                                    let in
                                        up (!strainedi) d;
                                        unsolvable ()
                                    end) () (!strainedl))
                            before up (!strainedi) 0
                        end
                end 
        in
          unsolvable ()
        end

end
