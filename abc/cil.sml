structure CIL =
struct

  datatype loc = Local of string | Global of string

  datatype value =
    Var of string
  | WordLiteral of Word32.word
  | StringLiteral of string

  (* Post-elaboration type *)
  datatype typ =
    Pointer of typ
    (* Like & in C++. Essentially a C lvalue. *)
  | Reference of typ
  | Struct of (string * typ) list
  (* All our computation is with 32-bit values. But loads and
     stores need to know the width. *)
  | Word32
  | Word16
  | Word8
  (* TODO functions *)

  (* All binary operators are on 32-bit words. *)
  datatype exp =
    Plus of value * value
  | Minus of value * value
  | Times of value * value
  | SignedDivision of value * value
  | UnsignedDivision of value * value
    (* Recall that C does not define the behavior of shifts on negative values,
       so these are unsigned. *)
  | LeftShift of value * value
  | RightShift of value * value
  | Greater of value * value
  | GreaterEq of value * value
  | Less of value * value
  | LessEq of value * value
  (* These are all bitwise. && and || are compiled away. *)
  | And of value * value
  | Or of value * value
  | Xor of value * value
    (* XXX signed vs unsigned mod? *)
  | Mod of value * value
  | Not of value
  | Complement of value
  | Negate of value
  | Address of value
  | Dereference of value
  | Subscript of value * value
  | Member of value * string
  | Call of string * value list
  | Read of loc

  datatype stmt =
    (* and type? *)
    Bind of string * exp * stmt
    (* and type? *)
  | Store of loc * value * stmt
    (* cond, true-branch, rest *)
  | If of value * stmt * stmt
  | Label of string * stmt
  | Goto of string
  | Return of value
  | End

  datatype program =
    Program of { functions : (string * ((string * typ) list * typ * stmt)) list,
                 (* The stmt here is initialization code, which is expected
                    to write to (only) this global, and should not depend on
                    any globals. Could be empty. *)
                 globals : (string * (typ * stmt)) list }
end
