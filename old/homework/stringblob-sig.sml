signature BLOB =
sig

  type blob

  val size : blob -> int
  val tostring : blob -> string
  val blob : string -> blob

  val tostringlist : blob -> string list

  val sub : blob * int -> char
  val subblob : blob * int * int -> blob
  val concat : blob list -> blob
  val ^ : blob * blob -> blob

  val implode : char list -> blob
  val explode : blob -> char list

  val map : (char -> char) -> blob -> blob
  val translate : (char -> blob) -> blob -> blob

  val compare : blob * blob -> order

  val <= : blob * blob -> bool
  val < : blob * blob -> bool
  val >= : blob * blob -> bool
  val > : blob * blob -> bool

  val eq : blob * blob -> bool

end