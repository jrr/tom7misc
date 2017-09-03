signature GEOM =
sig

  exception Geom of string

  (* If the two line segments intersect at a single point, return
     return SOME (x, y) where (x,y) is that point. *)
  val intersection : { seg1 : (real * real) * (real * real),
                       seg2 : (real * real) * (real * real) } ->
    (real * real) option

end