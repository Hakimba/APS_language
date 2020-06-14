type op = Not
        | And
        | Eq
        | Or
        | Lt
        | Add
        | Sub
        | Mul
        | Div

and atom = TBool | TInt | TVoid

and typ = ASTType of atom | ASTTypeFunc of typ list * typ


and arg = ASTArg of string * typ

and expr = ASTInt of int
          | ASTBool of bool
          | ASTId of string
          | ASTBinPrim of op * expr * expr
          | ASTUnPrim of op * expr
          | ASTIf of expr * expr * expr
          | ASTApp of expr * expr list
          | ASTAbs of arg list * expr

and cmd = ASTDec of dec 
          |ASTStat of stat

and stat = ASTEcho of expr
            | ASTSet of string * expr
            | ASTIfStat of expr * prog * prog
            | ASTWhile of expr * prog
            | ASTCall of string * expr list

and dec = ASTConst of string * typ * expr
         | ASTFun of string * typ * arg list * expr
         | ASTFunRec of string * typ * arg list * expr
         | ASTVar of string * typ
         | ASTProc of string * arg list * prog
         | ASTProcRec of string * arg list * prog

and prog = ASTProg of cmd list 

and apsvalue = ValIm of int 
              | ClosureFunc of expr * (string * apsvalue) list * string list * (semanticFunc) 
              | ClosureProc of prog * (string * apsvalue) list * string list * (semanticFunc)
              | ClosureFuncRec of string * expr * (string * apsvalue) list * string list * (semanticFunc)
              | ClosureProcRec of string * prog * (string * apsvalue) list * string list * (semanticFunc)
              | AddId of string (*adresse lien mémoire environnement*)

and semanticFunc = string list -> apsvalue list -> (string * apsvalue) list -> (string * apsvalue) list
and memory = Any | Val of apsvalue

let string_of_apsvalue value = match value with
  ValIm(_) -> " valim "
  | ClosureFunc(_,_,_,_)-> " closurefunc "
  | ClosureProc(_,_,_,_)-> " closureproc "
  | ClosureFuncRec(_,_,_,_,_) -> " closurefuncrec "
  | ClosureProcRec(_,_,_,_,_) -> " closureproccrec "
  | AddId(_) -> " addid "

let string_of_prog prog = match prog with
  | ASTProg(_) -> "prog"

let string_of_arg arg = match arg with
  | ASTArg(_,_) -> "arg"

let string_of_atom atom = match atom with
  | TBool -> "bool"
  | TInt -> "int"
  | TVoid -> "void"

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
