(* Simplified interface for reading non-enormous XML documents into
   a datatype.
   Tom Murphy VII, 2009.
   This file only: Use and distribute freely.
*)

signature XML =
sig

  exception XML of string

  (* name and value *)
  type attribute = string * string
  (* tag name, attributes *)
  type tag = string * attribute list

  datatype tree =
      Text of string
    | Elem of tag * tree list

  (* Parses a file on disk, returning the (normalized) tree or raising
     the exception XML. *)
  val parsefile : string -> tree

  (* Same, but from a string in memory. *)
  val parsestring : string -> tree

  (* Removes empty text nodes. Concatenates sibling text nodes. *)
  val normalize : tree -> tree


  (* Utilities. *)

  (* firstleaf tag tree
     Get the first leaf matching the tag name, where a leaf is a <tag>only
     applied to a series of text nodes</tag> or
     <totheemptytree></totheemptytree> (then treated as the empty
     string). A leaf may have attributes, but they are ignored. Interior
     nodes are not returned, even if they have the correct tag name.

     Expects the tree to be normalized.

     To avoid traversing the tree multiple times to find multiple leaves,
     consider getleaves below. *)
  val firstleaf : string -> tree -> string option

  (* Get all of the leaves in the tree (see above), Interior nodes and
     text outside of leaves are ignored. The results are collated by
     the tag name in the order that they appear in the document.

     Expects the tree to be normalized. *)
  val getleaves : tree -> (string * string list) list

  (* Get the first attribute matching the given name, or NONE
     if none match. *)
  val getattr : attribute list -> string -> string option

  (* Render as a string, favoring compactness but with newlines for
     readability. Does not include XML doctype. *)
  val tostring : tree -> string

end