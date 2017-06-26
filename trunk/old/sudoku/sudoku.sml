
(* Generate Su Doku puzzles.

   First, we need to generate a "solved puzzle," that is, a legal and
   completely filled board. Each legal position has different
   characteristics, and these positions are difficult to
   generate--essentially similar to an n-queens problem, although the
   solutions are more sparse.

   Since we're trying to generate interesting problems, we'll construct
   these through a randomized hill-climbing algorithm. (The only analytical
   solutions I can come up with generate very stylized matrices.)


   Save the solved puzzle. The next step is to "unsolve" this
   puzzle. This is done by removing entries from the matrix,
   and ensuring that the matrix still has only one solution.

   Being smart about the way I remove entries could improve the speed
   of this step; for instance by not requiring a check after each
   removal!

   The program continues doing this until it is interrupted; however,
   it does not backtrack. Therefore, if no digit can be removed without
   making the puzzle ambiguous, we succeed.

   *)


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

    (* for 2x2x2 
       fun easy () =
       Array.fromList
        [ 1, 2,  3, 4, 
          3, 4,  1, 2,

          2, 1,  4, 3,
          4, 3,  2, 1 ]
        *)

    (* there are a few matrix operations that preserve the
       invariants. 
       
       * swapping rows and columns within metarows/metacolumns
       * matrix transpose
       * (* NO: convolute: boxes become rows *)

       *)
    fun convolute a =
        Array.tabulate
        (width * width,
         fn i =>
         let
           (* row gives box number *)
           val r = i div width
           (* column gives index within box *)
           val c = i mod width
         in
           (*
           print ("i:" ^ Int.toString i ^ " = " ^
                  "r:" ^ Int.toString r ^ " c:" ^
                  Int.toString c ^ "\n");
           *)
           get a r (c mod dim) (c div dim)
         end)

      
    (* swap a column with the column 'dist' away. 
       if swapping between boxes, will generally break
       the box invariant. *)
    fun swapcol a c dist =
        Util.for 0 (width - 1)
        (fn r =>
         let
             val oc = (c + dist) mod width
             val t = Array.sub(a, r * width + c)
         in
             Array.update(a, r * width + c, Array.sub(a, r * width + oc));
             Array.update(a, r * width + oc, t);
             (*
             print ("swapped cols " ^ Int.toString c ^
             " and " ^ Int.toString oc ^ "\n");
             prdoku a
             *)
             ()
         end)

    fun swaprow a r dist =
        Util.for 0 (width - 1)
        (fn c =>
         let
             val or = (r + dist) mod width
             val t = Array.sub(a, r * width + c)
         in
             (* print ("swaprow " ^ Int.toString c ^ "/" ^ Int.toString dist ^ "\n"); *)
             Array.update(a, r * width + c, Array.sub(a, or * width + c));
             Array.update(a, or * width + c, t);
             (* print ("    ok.\n"); *)
             ()
         end)

    (* mixes up a matrix, preserving invariants *)
    fun safe_mixup 0 a = a
      | safe_mixup n a =
      let
        val a = convolute a

        (* a pair of columns or rows in the same
           metacolumn *)
        fun ss () =
          let
            (* some metacolumn *)
            val c = randi() mod dim

            (* two columns within it *)
            val fst = c*dim + randi() mod dim
            val snd = c*dim + randi() mod dim
          in
            if fst < snd 
            then (fst, snd - fst)
            else (snd, fst - snd)
          end

        val (r, rd) = ss ()
        val (c, cd) = ss ()
         
      in
        print ("  r/rd: " ^ Int.toString r ^ "/" ^ Int.toString rd ^
               "  c/cd: " ^ Int.toString c ^ "/" ^ Int.toString cd ^ "\n");
        (* maybe swap rows and columns safely *)
        print "after convolute:\n";
        prdoku a;
        checklegal a;
        swapcol a c cd;
        print "after swapcol:\n";
        prdoku a;
        checklegal a;
        swaprow a r rd;
        print "after swaprow:\n";
        prdoku a;
        checklegal a;
        print ("safe mixup: " ^ Int.toString n ^ "\n");
        safe_mixup (n - 1) a
      end


    fun predoku () =
        let 
            val a = easy ()

            (* val a = safe_mixup 1000 a *)
                
            fun mixup 0 a = a
              | mixup n a =
                let in
                  swapcol a (randi()) (randi());
                  swaprow a (randi()) (randi());
                  mixup (n - 1) (if randi () < dim 
                                 then convolute a
                                 else a)
                end
        in
            mixup (width * width + 1000) a
        end
        

    (* PERF much more efficient to just use the "solve" procedure that's
       already written, and leave it in the solved state if it finds a
       solution! *)
    (* XX this comment is inaccurate now *)
    (* generate a doku by a randomized hill-climbing approach. we always
       maintain the row and column invariants (for a "predoku").
       However, the boxes may not be legal. Define the badness as the
       number of total violations of the form Bn(i) = Bn(i') where
       i<i'. When badness=0, the predoku is a doku.

        (1) generate a random predoku
        (2) if badness=0, succeed
        (2) repeatedly swap rows or columns, but only when the resulting
            predoku has badness better than the current badness.
        (3) if no swaps improve the badness, try again from step 1.
       *)

    fun hillclimb () = 
        let
            val a = predoku ()

            val set = set a
            val get = get a

            fun sub i = Array.sub(a, i)
            fun subrc r c = sub (r * width + c)
            fun up i x = Array.update(a, i, x)
            fun uprc r c x = up (r * width + c) x

            fun getbad () =
                let 
                    val z = ref 0

                    fun findbadcolumn c =
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
                              if x = x' then z := !z + 1
                              else ()
                            end)
                       end)
                      
                    fun findbadrow r =
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
                              if x = x' then z := !z + 1
                              else ()
                            end)
                       end)


                    fun findbadbox b =
                        Util.for 0 (width - 2)
                        (fn i =>
                         let 
                             val x = get b (i div dim) (i mod dim)
                         in
                             Util.for (i + 1) (width - 1)
                             (fn i' =>
                              let val x' = get b (i' div dim) (i' mod dim)
                              in
                                  if x = x' then z := !z + 1
                                  else ()
                              end)
                         end);
                in
                    Util.for 0 (width - 1) findbadcolumn;
                    Util.for 0 (width - 1) findbadrow;
                    Util.for 0 (width - 1) findbadbox;
                    !z
                end

            fun swap sc sd =
               let 
                 val t = sub sc
               in
                 up sc (sub sd);
                 up sd t
               end

            (* success! *)
            fun climb 0 _ _ _ = a
              (* local minimum! *)
              | climb bad 0 _ _ =
                let in
                    print (Int.toString bad ^ ".. ");
                    hillclimb ()
                end
              | climb bad rem sc sd =
                let
                    (* the ops are reversible *)
                    val _ = swap sc sd
                    val newbad = getbad()
                in
                    if newbad < bad
                    (* success! *)
                    then 
                      let in 
                        if newbad < 16
                        then 
                          print ("new badness: " ^ Int.toString newbad ^ 
                                 "\n")
                        else ();
                        go ()
                      end
                    else 
                        let in
                            (* undo it *)
                            swap sc sd;
                            if sd < (width * width - 1)
                            then climb bad (rem - 1) sc (sd + 1)
                            else if sc < (width * width - 2)
                                 then 
                                     let in
                                         climb bad (rem - 1) 
                                                 (sc + 1) (sc + 2)
                                     end
                                 else 
                                     let in
                                         climb bad (rem - 1) 0 1
                                     end
                        end
                end

            and go () =
                let 
                    (* start at a random row/column, random offset *)
                    val sc = randi() * width + randi ()
                    val sd = randi() * width + randi ()
                    val (sc, sd) = if sc < sd then (sc, sd)
                                   else (sd, sc)
                in
                    (* PERF don't need to recompute badness *)
                    climb (getbad()) (width * width *
                                      (width * width - 1) div 2) sc sd
                end
        in
            go ()
        end

    fun make () =
        let
            val _ = print "hill climbing to find solved state...\n"
            val a = hillclimb ();
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
                    val strainedn = ref (dim + 1)
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

                    (* if there are no free squares, it is solvable
                       (solved) *)
                    if !strainedi = ~1
                    then 
                        let in
                            (* print "board is solved!\n"; *)
                            false
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
                                    b andalso
                                    let in
                                        up (!strainedi) d;
                                        unsolvable ()
                                    end) true (!strainedl))
                            before up (!strainedi) 0
                        end
                end 

            (* memoize isforced below *)
            val known_underconstrained = 
                Array.array(width * width, false)

            (* search for elements that are currently forced, and 
               remove them from the board. This is an eager procedure,
               so if there are no forced elements, we're done. note,
               because we never make the puzzle tighter than it
               already is, an element that is not forced never becomes
               forced again. *)
            fun search_reduce () =
                let
                    (* is the cell at index i forced? this is
                       where most of the computation happens,
                       since we need to solve the puzzle in
                       the forward direction in order to decide
                       if a position is forced. 

                       assumes the board is solvable as-is.
                       *)
                    exception Unforced
                    fun isforced i =
                        if Array.sub(known_underconstrained, i)
                        then false (* removing this is known to
                                      give multiple solutions *)
                        else
                            let
                                val old = sub i
                            in
                                (*
                                print ("is #" ^ Int.toString i ^
                                " unforced?\n");
                                *)
                                let in
                                    (* now, for every other choice,
                                       it must be unsolvable. *)
                                    Util.for 1 width
                                    (fn try =>
                                     let
                                         val new = I.fromInt try
                                     in
                                         (*
                                         print 
                                         (" > set #" ^ Int.toString i ^ 
                                          " to " ^ I.toString new ^ "\n");
                                         *)

                                         up i new;
                                         if new <> old
                                            andalso legalmove false i new
                                            andalso not (unsolvable ())
                                         then raise Unforced
                                         else ()
                                     end);
                                    up i old;
                                    (* print ".. Forced!\n"; *)
                                    true
                                end handle Unforced =>
                                    let in
                                        (* print ".. Unforced.\n"; *)
                                        (* leave table unchanged *)
                                        up i old;
                                        Array.update(known_underconstrained,
                                                     i, true);
                                        false
                                    end
                            end
                 
                    (* first one to try, then count backwards
                       until we have looked at everything *)
                    val i = randi() * width + randi()

                    fun try 0 = (* all are unforced! *)
                        let in
                            print "Found minimal puzzle.\n";
                            prdoku a
                        end
                      | try n =
                        let
                            val ii = ((i + n) mod (width * width))
                        in
                            (* must not already be free, and must
                               be forced if we are going to free it. *)
                            if sub ii > 0 andalso isforced ii
                            then
                                let in
                                    print ("(position #" ^ Int.toString ii ^
                                           " is forced..)\n");
                                    up ii 0;
                                    prdoku a;
                                    checklegal a;
                                    search_reduce()
                                end
                            else try (n - 1)
                        end
                in
                    try (width * width)
                end
                
            (* perform safe reductions as long as possible.
               these are reductions that are easy to see
               leave the puzzle with a unique solution. 

               [example: removing a single number from a 
                full row, column, or box]

               PERF: improving this phase should help a lot, since
               analytical reduction is much faster than reduction
               based on search. However, it only indirectly leads to
               good puzzles, since these kinds of gaps are trivial to
               solve.
               
               *)
            fun safe_reduce () =
                let
                    (* just remove one randomly from each column. *)
                    fun onecol c = uprc (randi()) c 0

                    (* if the row is full, also remove from it *)
                    exception OK
                    fun onerow r =
                        let in
                            Util.for 0 (width - 1)
                            (fn c =>
                             case subrc r c of
                                 0 : I.int => raise OK
                               | _ => ());
                            uprc r (randi()) 0
                        end handle OK => ()
                            
                    fun onebox b =
                        let 
                            val u = randi ()
                        in
                            Util.for 0 (width - 1)
                            (fn i =>
                             case get b (i div dim) (i mod dim) of
                                 0 : I.int => raise OK
                               | _ => ());
                            set b (u div dim) (u mod dim) 0
                        end handle OK => ()
                in
                    Util.for 0 (width - 1) onecol;
                    Util.for 0 (width - 1) onerow;
                    Util.for 0 (width - 1) onebox
                end

            val sol =
                let in
                    checklegal a;
                    print "solved state:\n";
                    prdoku a;
                    Array.fromList (Array.foldr op:: nil a)
                end
        in
            
            safe_reduce ();
            print "safely reduced:\n";
            prdoku a;

            search_reduce();
            
            print "\n=========================\nsolution:\n";
            prdoku sol;

            print "\nfinal puzzle:\n";
            prdoku a;

            ()
        end
   
   fun init a = 
     let
       
     in
       Util.for 0 (width - 1)
       (fn box =>
        Util.for 0 (dim - 1)
        (fn r =>
         Util.for 0 (dim - 1)
         (fn c =>
          set a box r c (10000 * box +
                         100 * r +
                         c)
          )))
     end


    val n = 
        case CommandLine.arguments () of
            [ns] => 
                (case Int.fromString ns of
                     SOME n => n
                   | NONE => 1)
          | _ => 1

    val _ =
        Util.for 0 (n - 1)
        (fn _ => make ())

end

structure SD = SuDoku