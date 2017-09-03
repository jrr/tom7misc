structure OSM :> OSM =
struct
  exception OSM of string

  datatype tree = datatype XML.tree

  structure IntMap = SplayMapFn(type ord_key = int
                                val compare = Int.compare)
  datatype highway =
      Residential
    | Primary
    | Secondary
    | Tertiary
    | Service
    | Steps
    | Foot
    | Motorway
    | MotorwayLink
    | Unclassified
    | Other of string

  fun highway_compare (l, r) =
    case (l, r) of
      (Residential, Residential) => EQUAL
    | (Residential, _) => LESS
    | (_, Residential) => GREATER
    | (Primary, Primary) => EQUAL
    | (Primary, _) => LESS
    | (_, Primary) => GREATER
    | (Secondary, Secondary) => EQUAL
    | (Secondary, _) => LESS
    | (_, Secondary) => GREATER
    | (Tertiary, Tertiary) => EQUAL
    | (Tertiary, _) => LESS
    | (_, Tertiary) => GREATER
    | (Service, Service) => EQUAL
    | (Service, _) => LESS
    | (_, Service) => GREATER
    | (Steps, Steps) => EQUAL
    | (Steps, _) => LESS
    | (_, Steps) => GREATER
    | (Foot, Foot) => EQUAL
    | (Foot, _) => LESS
    | (_, Foot) => GREATER
    | (Motorway, Motorway) => EQUAL
    | (Motorway, _) => LESS
    | (_, Motorway) => GREATER
    | (MotorwayLink, MotorwayLink) => EQUAL
    | (MotorwayLink, _) => LESS
    | (_, MotorwayLink) => GREATER
    | (Unclassified, Unclassified) => EQUAL
    | (Unclassified, _) => LESS
    | (_, Unclassified) => GREATER
    | (Other s1, Other s2) => String.compare (s1, s2)

  fun highwayfromstring tys =
    case tys of
      "residential" => Residential
    | "primary" => Primary
    | "secondary" => Secondary
    | "tertiary" => Tertiary
    | "service" => Service
    | "steps" => Steps
    | "foot" => Foot
    | "motorway" => Motorway
    | "motorway_link" => MotorwayLink
    | "unclassified" => Unclassified
    | _ => Other tys

  type street = { pts : int Vector.vector, typ : highway, name : string option }
  structure StreetSet = SplaySetFn
    (type ord_key = street
     val compare : street Util.orderer =
       Util.lexicographic
       [Util.order_field #pts (Util.lex_vector_order Int.compare),
        Util.order_field #typ highway_compare,
        Util.order_field #name (Util.option_compare String.compare)])
  structure StreetSetUtil = SetUtil(structure S = StreetSet)

  datatype osm = O of { points : LatLon.pos IntMap.map,
                        streets : street Vector.vector }

  fun ++r = r := 1 + !r

  fun loadosms fl =
    let
      val bad_node = ref 0
      val not_highway = ref 0
      val empty_way = ref 0
      val num_points = ref 0
      val num_streets = ref 0
      val overlap_points = ref 0
      val points = ref (IntMap.empty : LatLon.pos IntMap.map)
      val streets = ref StreetSet.empty

      fun onefile f =
        let
          val x = XML.parsefile f
              handle XML.XML s =>
                  raise OSM ("Couldn't parse " ^ f ^ "'s xml: " ^ s)

          fun isway (Elem(("nd", _), _) :: l) = true
            | isway (_ :: rest) = isway rest
            | isway nil = false

          fun getattr attrs k = ListUtil.Alist.find op= attrs k

          fun gettag k nil = NONE
            | gettag k (Elem(("tag", attrs), _) :: l) =
              (case getattr attrs "k" of
                   NONE => gettag k l
                 | SOME keyname =>
                       if keyname = k
                       then (case getattr attrs "v" of
                                 NONE => (* error? *) gettag k l
                               | SOME v => SOME v)
                       else gettag k l)
            | gettag k (_ :: l) = gettag k l

          (* XXX need to filter out streets whose points are never loaded. *)

          fun process (Elem(("node", attrs), body)) =
            (case (getattr attrs "id",
                   getattr attrs "lat",
                   getattr attrs "lon") of
                 (SOME id, SOME lat, SOME lon) =>
                   (case (Int.fromString id,
                          Real.fromString lat,
                          Real.fromString lon) of
                      (SOME id, SOME lat, SOME lon) =>
                        (case IntMap.find(!points, id) of
                           NONE =>
                             let in
                               ++num_points;
                               points := IntMap.insert
                               (!points, id,
                                LatLon.fromdegs { lat = lat,
                                                  lon = lon })
                             end
                         | SOME _ => ++overlap_points)
                    | _ => raise OSM "non-numeric id/lat/lon??")
               | _ => ++bad_node)
            | process (Elem(("way", attrs), body)) =
               if isway body
               then (case gettag "highway" body of
                       NONE => ++not_highway
                     | SOME tys =>
                         let
                           val pts = ref nil
                           fun proc (Elem(("nd", [("ref", s)]), _)) =
                             (case Int.fromString s of
                                NONE => raise OSM "non-numeric nd ref?"
                              | SOME i => pts := i :: !pts)
                             | proc _ = ()
                         in
                           app proc body;
                           ++num_streets;
                           streets :=
                           StreetSet.add(!streets,
                                         { pts = Vector.fromList (rev (!pts)),
                                           typ = highwayfromstring tys,
                                           name = gettag "name" body })
                         end)
               else ++empty_way
            (* nb. whole thing is in an <osm> tag. Should descend under that
               and then expect these all to be top-level, rather than
               recursing *)
            | process (Elem((_, _), body)) = app process body
            | process _ = ()
        in
          process x
        end

      val () = app onefile fl
      val streets = StreetSetUtil.tovector (!streets)
    in
      (* XXX don't print this *)
      TextIO.output
      (TextIO.stdErr,
       "OSM files:      " ^ Int.toString (length fl) ^ "\n" ^
       "Bad nodes:      " ^ Int.toString (!bad_node) ^ "\n" ^
       "Not highway:    " ^ Int.toString (!not_highway) ^ "\n" ^
       "Empty way:      " ^ Int.toString (!empty_way) ^ "\n" ^
       "Streets seen:   " ^ Int.toString (!num_streets) ^ "\n" ^
       "Unique streets: " ^ Int.toString (Vector.length streets) ^ "\n" ^
       "Points:         " ^ Int.toString (!num_points) ^ "\n" ^
       "Repeated pts:   " ^ Int.toString (!overlap_points) ^ "\n");
      O { points = !points, streets = streets }
    end

  fun loadosm f = loadosms [f]
end
