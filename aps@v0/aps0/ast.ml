type op = Not
        | And
        | Eq
        | Or
        | Lt
        | Add
        | Sub
        | Mul
        | Div

type atom = TBool | TInt

type typ = ASTType of atom | ASTTypeFunc of typ list * typ


type arg = ASTArg of string * typ

type expr = ASTInt of int
          | ASTBool of bool
          | ASTId of string
          | ASTBinPrim of op * expr * expr
          | ASTUnPrim of op * expr
          | ASTIf of expr * expr * expr
          | ASTApp of expr * expr list
          | ASTAbs of arg list * expr


type cmds = ASTEcho of expr
         | ASTConst of string * typ * expr
         | ASTFun of string * typ * arg list * expr
         | ASTFunRec of string * typ * arg list * expr 

type prog = ASTProg of cmds list 

type apsvalue = ValIm of int | Closure of expr * (string * apsvalue) list * string list * (semanticFunc) | ClosureRec of string * expr * (string * apsvalue) list * string list * (semanticFunc)
and semanticFunc = string list -> apsvalue list -> (string * apsvalue) list -> (string * apsvalue) list

let string_of_prog prog = match prog with
  | ASTProg(_) -> "prog"

let string_of_arg arg = match arg with
  | ASTArg(_,_) -> "arg"

let string_of_atom atom = match atom with
  | TBool -> "bool"
  | TInt -> "int"

let string_of_cmds cmds = match cmds with
  | ASTEcho(_) -> "echo"
         | ASTConst(_,_,_) -> "const"
         | ASTFun(_,_,_,_) -> "fun"
         | ASTFunRec(_,_,_,_) -> "funrec"

let string_of_atom atom = match atom with
  |TBool -> "bool"
  |TInt -> "int"

let string_of_op op = match op with
  Not -> "not"
  | And -> "and"
  | Eq -> "eq"
  | Or -> "or"
  | Lt -> "lt"
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
