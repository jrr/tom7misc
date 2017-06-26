
(* The External Language *)

structure EL =
struct

    datatype exp_ =

        Constant of constant
      | Var of string
      | Float of real

      | App of exp * exp

      | Let of dec * exp

      | Record of (string * exp) list
      (* vector constructor *)
      | Vector of exp list
      (* #label/typ exp *)
      | Proj of string * typ * exp
     
      | Andalso of exp * exp
      | Orelse of exp * exp
      | Andthen of exp * exp
      | Otherwise of exp * exp
      | If of exp * exp * exp

      | Seq of exp * exp
      | Constrain of exp * typ

      | Spawn of exp
      | Submit of exp
      | Syncall of exp

      | Raise of exp
      | Handle of exp * (pat * exp) list

      (* XXX not totally implemented: *)

      | Case of exp list * (pat list * exp) list

    and constant =
        CInt of int
      | CString of string
      | CChar of char

    and pat =
        PVar of string
      | PWild
      | PAs of string * pat
      | PRecord of (string * pat) list
      | PConstrain of pat * typ
      | PConstant of constant
      | PApp of string * pat

    and typ =
        TVar of string
      | TApp of typ list * string
      | TRec of (string * typ) list
      | TArrow of typ * typ

    and dec_ =
        (* wish we had refinements here. 
           val pat cannot contain PConstant or PApp *)
        (* val (a, b) loop = Util.loop : a -> b *)
        Val of string list * pat * exp
      | Do of exp
      | Type of string list * string * typ

      (* fun (a, b, c) f p1 p2 p3 : t1 = e1
           |           f p1 p2 p3 : t2 = e2
         and g p1 p2 : t3 = e3
           | ... *)
      | Fun of (string list * string * 
                (pat list * typ option * exp) list) list

      (* datatype (a, b, c) t = A of t | B of b | C of t1
         and                u = D of u | E of t *)
      | Datatype of string list * 
                    (string * (string * typ option) list) list
      | Tagtype of string
        (* newtag Fail of string in exn *)
      | Newtag of string * typ option * string
        (* just means newtag E of TO in "exn" *)
      | Exception of string * typ option

    (* fixity decls are handled at parse time *)

    withtype exp = exp_ * Pos.pos
    and dec = dec_ * Pos.pos

end
