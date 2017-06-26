
structure IL =
struct

    type label = string
    type var = Variable.var
        
    (* types : classifiers for values *)
    datatype typ =
        TVar of var
      | TRec of (label * typ) list
      (* bool true => total 
         functions are n-ary.
         *)
      | Arrow of bool * typ list * typ
      | Sum of (label * typ) list
      (* pi_n (mu  v_0 . typ_0
               and v_1 . typ_1
               and ...)
         0 <= n < length l, length l > 0.

         when unrolling, choose nth arm and
         substitute:

         typ_n [ (pi_0 mu .. and ..) / v_0,
                 (pi_1 mu .. and ..) / v_1,
                 ... ]
         *)
      | Mu of int * (var * typ) list 
      | Evar of ebind ref

      | TVec of typ

      | Task of typ
      | TRef of typ

      | TTag of typ * var

    (* type constructors *)
    and con =
        Typ of typ
      | Lambda of typ list -> typ

    (* existential *)
    and ebind =
        Free of int
      | Bound of typ

    (* polymorphic type *)
    and 'a poly =
        Mono of 'a
      | Quant of var * 'a poly

    and exp =
        Var of var
      | Int of int
      | String of string
      | Char of char
      (* application is n-ary *)
      | App of exp * exp list
      | Appt of exp * typ

      | Record of (label * exp) list
      (* #lab/typ e *)
      | Proj of label * typ * exp
      | Raise of typ * exp
      | Handle of exp * var * exp
      | Get of exp
      | Set of exp * exp
      | Seq of exp * exp
      | Let of dec * exp
      | Roll of typ * exp
      | Unroll of exp

      (* tag v with t *)
      | Tag of exp * exp
      (* tagtype, object, var (for all arms), branches, def *)
      | Tagcase of typ * exp * var * (var * exp) list * exp

      (* apply a primitive to some expressions and types *)
      | Primapp of Primop.primop * exp list * typ list

      (* sum type, object, var (for all arms), branches, default.
         the label/exp list need not be exhaustive.
         *)
      | Sumcase of typ * exp * var * (label * exp) list * exp
      | Inject of typ * label * exp

      (* these for ConCert *)
      | Spawn of exp
      | Submit of exp
      | Syncall of exp

      (* these not implemented yet *)
      | Forget of exp
      | Relaxi of exp
      | Exit of exp

    and dec =
        Do of exp
        (* quantifiers on the outside -- no poly recursion *)
      | Fix of {name : var,
                arg  : var list,
                dom  : typ list,
                cod  : typ,
                body : exp} list poly
      | Val of (var * typ * exp) poly
      | Tagtype of var
        (* tag of typ in tagtype *)
      | Newtag of var * typ * var

    (* the kind is the number of curried arguments. 0 is kind T. *)
    withtype kind = int

    datatype tystatus = Regular | Extensible
    datatype idstatus = 
        Normal 
      | Constructor 
      (* the var is the tag, in scope, that should be used
         to deconstruct this tagged expression *)
      | Tagger of var 
      | Primitive of Primop.primop

end
