open Ast

let visitTypeAtom atom = match atom with
  |_ -> string_of_atom atom

let rec visitTypeConstr typ = match typ with
  |ASTType(atom) -> visitTypeAtom atom
  |ASTTypeFunc(ltyp,typ) -> "arrow(" ^ (visitTypes ltyp) ^ ","^(visitTypeConstr typ)^")" 
                         
and visitTypes typs = "[" ^ (String.concat ", " (List.map visitTypeConstr typs)) ^ "]"

let visitArg arg = match arg with
  ASTArg(str,typ) -> "("^ "\"" ^ str ^ "\"" ^ "," ^ (visitTypeConstr typ) ^ ")"

let rec visitExpr e = 
  match e with
  ASTInt(int) -> string_of_int int
  |ASTBool(bool) -> string_of_bool bool
  |ASTId(id) -> "\"" ^ id ^ "\""
  |ASTBinPrim(op,opg,opd) -> (string_of_op op) ^ "(" ^ visitExpr opg ^ "," ^ visitExpr opd ^ ")"
  |ASTUnPrim(op,operand) -> (string_of_op op) ^ "(" ^ visitExpr operand ^ ")"
  |ASTApp(expr,exprs) -> "app(" ^ (visitExpr expr) ^ ", [" ^ (String.concat ", " (List.map visitExpr exprs)) ^ "])"
  |ASTAbs(args,expr) -> "abs([" ^ (String.concat "," (List.map visitArg args)) ^"],"^ visitExpr expr ^")"
  |ASTIf(cond,th,alternant) -> "if(" ^ visitExpr cond ^ "," ^visitExpr th ^ "," ^visitExpr alternant ^ ")"


let visitCmds cmds = match cmds with
  ASTEcho n -> "echo(" ^ visitExpr n ^")"
  |ASTConst(id,typs,e) -> "const(\"" ^ id ^ "\"," ^visitTypeConstr typs ^ "," ^ visitExpr e ^ ")"
  |ASTFun(id,typs,args,e) ->  "fun(\"" ^ id ^ "\","^ (visitTypeConstr typs) ^ ",[" ^ (String.concat "," (List.map visitArg args)) ^ "]," ^ visitExpr e ^ ")"
  |ASTFunRec(id,typs,args,e) -> "funRec(\"" ^ id ^ "\"," ^ visitTypeConstr typs ^ ",[" ^  (String.concat "," (List.map visitArg args)) ^ "]," ^ visitExpr e ^ ")"

let visitProg p = match p with 
    ASTProg le -> print_string ("prog([" ^ (String.concat "," (List.map visitCmds le)) ^ "]).")

let _ = let lexbuf = Lexing.from_channel stdin in
  let progAst = Parser.main Lexer.token lexbuf in
  visitProg progAst; print_newline();;
