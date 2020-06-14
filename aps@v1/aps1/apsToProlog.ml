open Ast

let visitTypeAtom atom = match atom with
  |_ -> string_of_atom atom

let rec visitTypeConstr typ = match typ with
  |ASTType(atom) -> visitTypeAtom atom
  |ASTTypeFunc(ltyp,typ) -> "arrow(" ^ (visitTypes ltyp) ^ ","^(visitTypeConstr typ)^")" 
                         
and visitTypes typs = "[" ^ (String.concat ", " (List.map visitTypeConstr typs)) ^ "]"

and visitArg arg = match arg with
  ASTArg(str,typ) -> "("^ "\"" ^ str ^ "\"" ^ "," ^ (visitTypeConstr typ) ^ ")"

and visitExpr e = 
  match e with
  ASTInt(int) -> string_of_int int
  |ASTBool(bool) -> string_of_bool bool
  |ASTId(id) -> "\"" ^ id ^ "\""
  |ASTBinPrim(op,opg,opd) -> (string_of_op op) ^ "(" ^ visitExpr opg ^ "," ^ visitExpr opd ^ ")"
  |ASTUnPrim(op,operand) -> (string_of_op op) ^ "(" ^ visitExpr operand ^ ")"
  |ASTApp(expr,exprs) -> "app(" ^ (visitExpr expr) ^ ", [" ^ (String.concat ", " (List.map visitExpr exprs)) ^ "])"
  |ASTAbs(args,expr) -> "abs([" ^ (String.concat "," (List.map visitArg args)) ^"],"^ visitExpr expr ^")"
  |ASTIf(cond,th,alternant) -> "if(" ^ visitExpr cond ^ "," ^visitExpr th ^ "," ^visitExpr alternant ^ ")"


and visitStat st  = match st with
  ASTEcho n -> "echo(" ^ visitExpr n ^")"
  |ASTIfStat(expr,bk1,bk2) -> "ifst("^visitExpr expr^","^visitProg bk1^","^visitProg bk2^")"
  |ASTWhile(expr,bk) -> "while("^visitExpr expr^","^visitProg bk^")"
  |ASTCall(id, exprs)-> "call(\""^id^"\",["^ (String.concat ", " (List.map visitExpr exprs)) ^ "])"
  |ASTSet(id,expr) -> "set(\""^id^"\","^visitExpr expr^")"


and visitDec dec = match dec with 
  ASTConst(id,typs,e) -> "const(\"" ^ id ^ "\"," ^visitTypeConstr typs ^ "," ^ visitExpr e ^ ")"
  |ASTFun(id,typs,args,e) ->  "fun(\"" ^ id ^ "\","^ (visitTypeConstr typs) ^ ",[" ^ (String.concat "," (List.map visitArg args)) ^ "]," ^ visitExpr e ^ ")"
  |ASTFunRec(id,typs,args,e) -> "funRec(\"" ^ id ^ "\"," ^ visitTypeConstr typs ^ ",[" ^  (String.concat "," (List.map visitArg args)) ^ "]," ^ visitExpr e ^ ")"
  |ASTVar(id,typ) -> "var(\""^id^"\","^visitTypeConstr typ^")"
  |ASTProc(id,args,prog) -> "proc(\""^id^"\",["^(String.concat "," (List.map visitArg args))^"],"^visitProg prog^")"
  |ASTProcRec(id,args,prog) -> "procRec(\""^id^"\",["^(String.concat "," (List.map visitArg args))^"],"^visitProg prog^")"

and visitCmds cmds = match cmds with
  ASTDec(dec) -> visitDec dec
  | ASTStat(st) -> visitStat st
  
and visitProg p = match p with 
    ASTProg le -> "prog([" ^ (String.concat "," (List.map visitCmds le)) ^ "])"

let _ = let lexbuf = Lexing.from_channel stdin in
  let progAst = Parser.main Lexer.token lexbuf in
  ignore(print_string((visitProg progAst)^".")); print_newline();;
