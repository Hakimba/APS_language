open Ast

let rec constrEnv names vals env = match vals with
	[] -> env
	| hd::tl -> constrEnv (List.tl names) tl ((List.hd names,hd) :: env)

let rec getInEnv key env = match env with
	[] -> failwith "getInEnv : Not_Found"
	| (k,v)::tl -> if key = k then v else getInEnv key tl

let extractNamesArgs args =
	let rec extractNamesArgs_aux l acc = match l with
	[] -> acc
	| hd::tl -> let visitArg arg = match arg with
					ASTArg(str,typ) -> str in
				extractNamesArgs_aux tl ((visitArg hd) :: acc)  in 
	extractNamesArgs_aux args []


let v apsval = match apsval with
	ValIm(n) -> n
	|_ -> failwith "v work with ValIm value"

let unaryPrimitives op ope = match op with 
	"not" -> if((v ope) = 0) then ValIm(1) else ValIm(0)
	|_ -> failwith "Unary operator not supported"

let binaryPrimitives op l r = match op with 
	"and" -> if((v l) = 0) then ValIm(0) else r
  | "eq"  -> if ((v l) = (v r)) then ValIm(1) else ValIm(0)
  | "or"  -> if((v l) = 1) then ValIm(1) else r
  | "lt"  -> if ((v l) < (v r)) then ValIm(1) else ValIm(0)
  | "add" -> ValIm((v l) + (v r))
  | "sub" -> ValIm((v l) - (v r))
  | "mul" -> ValIm((v l) * (v r))
  | "div" -> ValIm((v l) / (v r))
  |_ -> failwith "Binary operator not supported"


let rec whichApp app values context = match app with
	 Closure(e,env,names_args,semantic_fun) -> let (_,flux) = context in evalExpr ((semantic_fun names_args values env),flux) e
	|ClosureRec(id,e,env,names_args,semantic_fun) -> let new_env = constrEnv [id] [ClosureRec(id,e,env,names_args,semantic_fun)] env in  let (_,flux) = context in evalExpr ((semantic_fun names_args values new_env),flux) e
	| _ -> failwith "not a closure"

and evalExpr context e = 
  match e with
  ASTInt(n) -> ValIm(n)
  |ASTBool(b) -> if(b) then ValIm(1) else ValIm(0)
  |ASTId(id) -> let (env,_) = context in getInEnv id env
  |ASTBinPrim(op,opg,opd) -> binaryPrimitives (string_of_op op) (evalExpr context opg ) (evalExpr context opd )
  |ASTUnPrim(op,operand) -> unaryPrimitives (string_of_op op) (evalExpr context operand )
  |ASTApp(expr,exprs) -> whichApp (evalExpr context expr) (List.map (evalExpr context) exprs) context
  |ASTAbs(args,expr) -> let (env,_) = context in let names = extractNamesArgs args in Closure(expr,env,names,constrEnv)
  |ASTIf(cond,th,alternant) -> if (v (evalExpr context cond ) = 1) then evalExpr context th  else evalExpr context alternant 


let evalCmds context cmd = match cmd with
  ASTEcho n -> let (env,flux) = context in (env,(evalExpr context n) :: flux)
  |ASTConst(id,typs,e) -> let (env,flux) = context in let v  = evalExpr context e  in ((id,v) :: env,flux)
  |ASTFun(id,typs,args,e) ->  let (env,flux) = context in let names = extractNamesArgs args in  ((id,Closure(e,env,names,constrEnv)) :: env,flux)
  |ASTFunRec(id,typs,args,e) -> let (env,flux) = context in let names = extractNamesArgs args in ((id,ClosureRec(id,e,env,names,constrEnv)) :: env,flux)

let evalProg prog context = match prog with
	ASTProg cmds -> let (env,flux) = List.fold_left evalCmds context cmds in List.iter (function x -> Printf.printf "%d\n" (v x)) flux


let _ = let lexbuf = Lexing.from_channel stdin in
  let progAst = Parser.main Lexer.token lexbuf in
  evalProg progAst ([],[]); print_newline();;