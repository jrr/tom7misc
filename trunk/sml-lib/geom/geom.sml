structure Geom :> GEOM =
struct

  exception Geom of string

  fun intersection { seg1 : (real * real) * (real * real),
                     seg2 : (real * real) * (real * real) } =
    let
      val ((p0x, p0y), (p1x, p1y)) = seg1
      val ((p2x, p2y), (p3x, p3y)) = seg2

      val s1x = p1x - p0x
      val s1y = p1y - p0y
      val s2x = p3x - p2x
      val s2y = p3y - p2y

      val l1 = p0x - p2x
      val l2 = p0y - p2y
      val denom = ~ s2x * s1y + s1x * s2y

      val s = (~ s1y * l1 + s1x * l2) / denom
    in
      if s >= 0.0 andalso s <= 1.0
      then
        let
          val t = (s2x * l2 - s2y * l1) / denom
        in
          if t >= 0.0 andalso t <= 1.0
          then SOME (p0x + (t * s1x),
                     p0y + (t * s1y))
          else NONE
        end
      else NONE
    end

end