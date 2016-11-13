structure CIL =
struct

  datatype loc = Local of string | Global of string

  datatype value =
    Var of string
  | AddressLiteral of loc
  | WordLiteral of Word32.word
    (* Maybe should be pointer to bytes? *)
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
  (* XXX maybe recursive instances of value should really just be a variable,
     basically, a register? *)
  datatype exp =
    Value of value
  | Plus of value * value
  | Minus of value * value
  | Times of value * value
  | SignedDivision of value * value
  | UnsignedDivision of value * value
    (* Recall that C does not define the behavior of shifts on negative values,
       so these are unsigned. *)
  | LeftShift of value * value
  | RightShift of value * value
    (* XXX we need signed versions of greater, greatereq, less, lesseq *)
  | Greater of value * value
  | GreaterEq of value * value
  | Less of value * value
  | LessEq of value * value
  | Eq of value * value
  | Neq of value * value
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
  | Call of value * value list
    (* XXX needs type/width? *)
  | Read of value

  datatype stmt =
    (* and type? *)
    Bind of string * exp * stmt
  (* Store(address, value, rest)
     XXX and type/width? *)
  | Store of value * value * stmt
    (* cond, true-branch, rest *)
  (* | If of value * stmt * stmt *)
  (* | Label of string * stmt *)
  | GotoIf of value * string * stmt
  | Return of value
  | End

  datatype function =
    Func of { args : (string * typ) list,
              ret : typ,
              body : stmt,
              blocks : (string * stmt) list }

  datatype global =
    Glob of { typ : typ,
              init : stmt,
              blocks : (string * stmt) list }

  datatype program =
    Program of { functions : (string * function) list,
                 (* The stmt here is initialization code, which is expected
                    to write to (only) this global, and should not depend on
                    any globals. Could be empty. *)
                 globals : (string * global) list }

  fun typtos (Pointer t) = "(" ^ typtos t ^ " ptr)"
    | typtos (Reference t) = "(" ^ typtos t ^ " ref)"
    | typtos (Struct t) = "... TODO STRUCT ..."
    | typtos Word32 = "w32"
    | typtos Word16 = "w16"
    | typtos Word8 = "w8"

  fun loctos (Local l) = l
    | loctos (Global l) = "GLOBAL " ^ l

  fun valtos (Var v) = v
    | valtos (WordLiteral w) = "0x" ^ Word32.toString w
    (* XXX heuristic for data, etc. *)
    | valtos (StringLiteral s) = "\"" ^ String.toString s ^ "\""
    | valtos (AddressLiteral a) = "ADDR(" ^ loctos a ^ ")"

  fun exptos (Value v) = valtos v
    | exptos (Plus (a, b)) = valtos a ^ " + " ^ valtos b
    | exptos (Minus (a, b)) = valtos a ^ " - " ^ valtos b
    | exptos (Times (a, b)) = valtos a ^ " * " ^ valtos b
    | exptos (SignedDivision (a, b)) = valtos a ^ " -/ " ^ valtos b
    | exptos (UnsignedDivision (a, b)) = valtos a ^ " +/ " ^ valtos b
    | exptos (LeftShift (a, b)) = valtos a ^ " << " ^ valtos b
    | exptos (RightShift (a, b)) = valtos a ^ " >> " ^ valtos b
    | exptos (Greater (a, b)) = valtos a ^ " > " ^ valtos b
    | exptos (GreaterEq (a, b)) = valtos a ^ " >= " ^ valtos b
    | exptos (Less (a, b)) = valtos a ^ " < " ^ valtos b
    | exptos (LessEq (a, b)) = valtos a ^ " <= " ^ valtos b
    | exptos (Eq (a, b)) = valtos a ^ " == " ^ valtos b
    | exptos (Neq (a, b)) = valtos a ^ " != " ^ valtos b
    | exptos (And (a, b)) = valtos a ^ " & " ^ valtos b
    | exptos (Or (a, b)) = valtos a ^ " | " ^ valtos b
    | exptos (Xor (a, b)) = valtos a ^ " ^ " ^ valtos b
    | exptos (Mod (a, b)) = valtos a ^ " % " ^ valtos b
    | exptos (Not a) = "!" ^ valtos a
    | exptos (Complement a) = "~" ^ valtos a
    | exptos (Negate a) = "-" ^ valtos a
    | exptos (Address a) = "&" ^ valtos a
    | exptos (Dereference a) = "*" ^ valtos a
    | exptos (Subscript (a, b)) = valtos a ^ "[" ^ valtos b ^ "]"
    | exptos (Member (a, s)) = "(" ^ valtos a ^ ")." ^ s
    | exptos (Call (f, vl)) = "CALL " ^ valtos f ^ "(" ^ StringUtil.delimit ", " (map valtos vl) ^ ")"
    | exptos (Read addr) = "READ " ^ valtos addr

  fun stmttos (Bind (var, e, s)) =
        "  " ^ var ^ " = " ^ exptos e ^ "\n" ^ stmttos s
    | stmttos (Store (dest, v, s)) =
        "  " ^ valtos dest ^ " := " ^ valtos v ^ "\n" ^ stmttos s
    | stmttos (GotoIf (v, lab, s)) =
        "  if " ^ valtos v ^ " goto " ^ lab ^ "\n" ^ stmttos s
    | stmttos (Return v) = "  return " ^ valtos v
    | stmttos End = "  end"

  fun progtos (Program { functions, globals }) =
    let
      fun blocktos (lab, s) =
        lab ^ ":\n" ^
        stmttos s

      fun func (s, Func {args, ret, body, blocks}) =
        "FUNC " ^ s ^ "(" ^ StringUtil.delimit ", "
        (map (fn (s, t) => s ^ " : " ^ typtos t) args) ^ ") : " ^ typtos ret ^ " =\n" ^
        stmttos body ^ "\n" ^
        StringUtil.delimit "\n" (map blocktos blocks)

      fun glob (s, Glob {typ, init, blocks}) = "GLOBAL " ^ s ^ " : " ^ typtos typ ^ " =\n" ^
        stmttos init ^
        StringUtil.delimit "\n" (map blocktos blocks)
    in
      StringUtil.delimit "\n" (map func functions) ^ "\n\n" ^
      StringUtil.delimit "\n" (map glob globals) ^ "\n"
    end

end
