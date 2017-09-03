signature OSM =
sig
  (* OpenStreetMap parsing. *)
  exception OSM of string

  structure IntMap : ORD_MAP where type Key.ord_key = int

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

  val highway_compare : highway * highway -> order

  datatype osm = O of { points : LatLon.pos IntMap.map,
                        streets : { pts : int Vector.vector,
                                    typ : highway,
                                    name : string option } Vector.vector }

  (* Load OSM data from the named file. Raises PacTom on some sorts of errors;
     ignores others. *)
  val loadosm : string -> osm
  (* Load multiple files at once, merging their contents. *)
  val loadosms : string list -> osm
end