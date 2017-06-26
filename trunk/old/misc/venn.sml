
(* Well, never got anywhere with this... *)

(* This program solves equations to create Venn diagrams with certain
   constraints.

   It is specialized to the case of a 3-set Venn diagram. The 2-set
   case is easy to do by hand, and the n>3 case needs funny shapes.
   We also assume that the intersection of any pair of these sets
   is non-empty, since otherwise it is easily decomposed into
   independent 2-set cases (well, usually).

   Call these three sets A,B,C. We know the diagram looks something
   like this:


                 ,-----,-----,
               ,`    ,' `,    `,
              /     /     \     \
             ,     ,  AB   ,     ,
             |  A  |       |  B  |
             '     ',-----,'     '
              \   ,'\ ABC /`,   /
               `,/ AC`, ,'BC \,'
                ,`-----'-----',
                |             |
                '             '
                 \     C     /
                  `,       ,'
                    `-----'
   
   What this program does is generate such diagrams, maintaining the
   relative sizes of the sets A,B,C and their intersecting regions.
   The input is thus the sizes of the areas labeled A, AB, ABC, AC, B,
   BC, and C, above which are all independent. (A is *not* the total
   size of A; that is A + AB + ABC + AC.) None of these sizes can be
   zero.

   Is it always possible to generate such a diagram? In fact, the
   answer is no. Fixing every other area, note that we cannot change
   the relative size of B (for instance) without affecting the sizes
   of AB, BC, and ABC as well. Therefore, we create such diagrams
   by using ellipses instead of circles.

   The solution is as follows, then. First, without considering C,
   solve for the positions and radii of A and B. The solution is
   unique (up to scaling). wlog, call the larger of the two circles A
   and fix its radius as 1.0 and center at 0,0. We also assume that
   B's center lies on the X axis.

   The solution is thus defined by Rab (the distance from the center
   of A to the center of B, which is the x coordinate of B's center)
   and the radius of B.

   Now we want to add the ellipse C and satisfy the constraints.

                |
                |
                |,-----,-----,
               ,|    ,' `,    `,
              / |   /  AB \     \
             ,  |  ,  Rab  ,     ,
             +  A--+-------+--B  |
             ' 0,0 ',-----,'     '
              \   ,'\ ABC /`,   /
               `,/ AC`, ,'BC \,'
                ,`-----'-----',
                |             |
                '             '
                 \     C     /
                  `,       ,'
                    `-----'

   It's best if we can satisfy these constraints one at a time.
   
   ...
*)