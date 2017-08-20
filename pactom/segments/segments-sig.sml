signature SEGMENTS =
sig

  exception Segments of string

  datatype segment =
    Segment of { name: string,
                 (* Must be at least two gates, the start and end. 
                    The runner must pass through each gate in the
                    correct direction, in order. 
                    
                    A gate is represented as a start and end point.
                    Thinking of the first point as being South and
                    the second as North. Then the gate's direction
                    is West to East. *)
                 gates: (LatLon.pos * LatLon.pos) Vector.vector }

  (* Parse a KML file (Google Earth) with certain conventions.
     Each segment should be given as a folder containing two or more
     polygons. The polygons should be five-sided, with a flag-shape like
     this:

                   B
                   |`,
                   |  `,
                   |    C
                   |   /
                   |  D
                   |   \
                   |    E
                   |  ,`
                   |,`
                   A

    The order of the points in the polygon doesn't matter, nor does the
    winding direction (here they just have arbitrary names A-E). This
    flag represents the gate A-B; that edge is the line to cross, and
    the other points indicate the passing direction (which of the two
    perpendicular normals of A-B is desired, if you will). The point
    D should be the only concavity. 

    The polygons should be named code:start, code:1, code:2, ..., code:end,
    for some consistent string "code" (usually the same as the containing
    folder). Other metadata like color is ignored. *)
  val parse_file : string -> segment list

end
