open Ast

let cnt = ref(0);;

let  rev_list l = 
  let rec rev_list_aux l_aux acc = match l_aux with 
    [] -> acc
    | hd::tl-> rev_list_aux tl (hd :: acc) in
    rev_list_aux l []

let rec constrEnv names vals env = match vals with
	[] -> env
	| hd::tl -> constrEnv (List.tl names) tl ((List.hd names,hd) :: env)

and getInMem key mem = match mem with
  [] -> failwith ("getInMem : "^key^"Not_Found")
  | (k,v)::tl -> if key = k then v else getInMem key tl

and reAffect addrid value mem = 
  let rec reAffect_aux acc l = match l with
    [] -> acc
    | (k,v)::tl -> if k = addrid then reAffect_aux ((k,Val(value)) :: acc) tl 
                   else reAffect_aux ((k,v) :: acc) tl
  in  reAffect_aux [] mem

and ifDefined id value mem = match value with
  Val(v) -> v
  |Any -> failwith (""^id^" must be initialized before use")
  

and addrOrVal id value mem = match value with
  AddId(ad) -> ifDefined id (getInMem ad mem) mem
  | _ -> value

and isAddr value = match value with
  AddId(ad) -> true
  | _ -> false

and isValIm value = match value with
  ValIm(v) -> true
  |_ -> false

and getAddId addr = match addr with
  AddId(id) -> id
  |_ -> failwith "not a addrid" 

and getInEnv key env = match env with
	[] -> failwith ("getInEnv : "^key^"Not_Found")
	| (k,v)::tl -> if key = k then v else getInEnv key tl

and extractNamesArgs args =
	let rec extractNamesArgs_aux l acc = match l with
	[] -> acc
	| hd::tl -> let visitArg arg = match arg with
					ASTArg(str,typ) -> str in
				extractNamesArgs_aux tl ((visitArg hd) :: acc)  in
	extractNamesArgs_aux args []


and v apsval = match apsval with
	ValIm(n) -> n
  | ClosureFunc(_,_,_,_) -> failwith "closure func"
  | ClosureProc(_,_,_,_) -> failwith "proc closure\n"
  | ClosureFuncRec(_,_,_,_,_) -> failwith "func rec \n"
  | ClosureProcRec (_,_,_,_,_) -> failwith "proc rec closure\n"
  | AddId(_) -> failwith "addid\n"

and unaryPrimitives op ope = match op with 
	"not" -> if((v ope) = 0) then ValIm(1) else ValIm(0)
	|_ -> failwith "Unary operator not supported"

and binaryPrimitives op l r = match op with 
	"and" -> if((v l) = 0) then ValIm(0) else r
  | "eq"  -> if ((v l) = (v r)) then ValIm(1) else ValIm(0)
  | "or"  -> if((v l) = 1) then ValIm(1) else r
  | "lt"  -> if ((v l) < (v r)) then ValIm(1) else ValIm(0)
  | "add" -> ValIm((v l) + (v r))
  | "sub" -> ValIm((v l) - (v r))
  | "mul" -> ValIm((v l) * (v r))
  | "div" -> ValIm((v l) / (v r))
  |_ -> failwith "Binary operator not supported"



and whichFunc app values context = match app with
   ClosureFunc(e,env,names_args,semantic_fun) -> let (_,mem,flux) = context in evalExpr ((semantic_fun names_args values env),mem,flux) e
  |ClosureFuncRec(id,e,env,names_args,semantic_fun) -> let new_env = constrEnv [id] [ClosureFuncRec(id,e,env,names_args,semantic_fun)] env 
                                                       in  let (_,mem,flux) = context in evalExpr ((semantic_fun names_args values new_env),mem,flux) e
  | _ -> failwith "not a ClosureFunc"

and whichProc app values context = match app with
  ClosureProc(prog,env,names_args,semantic_fun) -> let (_,mem,flux) = context in evalProg prog ((semantic_fun names_args values env),mem,flux)
  |ClosureProcRec(id,prog,env,names_args,semantic_fun) -> let new_env = constrEnv [id] [ClosureProcRec(id,prog,env,names_args,semantic_fun)] env
                                                          in  let (_,mem,flux) = context in evalProg prog ((semantic_fun names_args values new_env),mem,flux)
  | _ -> failwith "not a ClosureProc"

and evalExpr context e = 
  match e with
  ASTInt(n) -> ValIm(n)
  |ASTBool(b) -> if(b) then ValIm(1) else ValIm(0)
  |ASTId(id) -> let (env,mem,_) = context in addrOrVal id (getInEnv id env) mem
  |ASTBinPrim(op,opg,opd) -> binaryPrimitives (string_of_op op) (evalExpr context opg ) (evalExpr context opd )
  |ASTUnPrim(op,operand) -> unaryPrimitives (string_of_op op) (evalExpr context operand )
  |ASTApp(expr,exprs) -> whichFunc (evalExpr context expr) (List.map (evalExpr context) exprs) context
  |ASTAbs(args,expr) -> let (env,_,_) = context in let names = extractNamesArgs args in ClosureFunc(expr,env,names,constrEnv)
  |ASTIf(cond,th,alternant) -> if (v (evalExpr context cond ) = 1) then evalExpr context th  else evalExpr context alternant 

and evalStat context st = match st with
   ASTEcho n ->  let (env,mem,flux) = context in (env,mem,(evalExpr context n) :: flux)
  |ASTIfStat(expr,bk1,bk2) -> if (v (evalExpr context expr ) = 1) then evalProg bk1 context  else evalProg bk2 context
  |ASTWhile(expr,bk) -> if (v (evalExpr context expr ) = 0) then context
                        else
                          let new_ctx = evalProg bk context in
                          evalStat new_ctx (ASTWhile(expr,bk))
  |ASTCall(id, exprs)-> let (env,mem,flux) = context in whichProc (getInEnv id env) (List.map (evalExpr context) exprs) context
  |ASTSet(id,expr) -> 
                      let (env,mem,flux) = context in let addrid = getInEnv id env 
                      in if isAddr addrid then 
                            let addr = getAddId addrid 
                            in let v = evalExpr context expr
                            in if isValIm v then (env,(reAffect addr v mem),flux)
                               else context
                         else context
  
and evalDec context dec = match dec with
  ASTConst(id,typs,e) -> let (env,mem,flux) = context in let v  = evalExpr context e  in ((id,v) :: env,mem,flux)
  |ASTFun(id,typs,args,e) ->  let (env,mem,flux) = context in let names = extractNamesArgs args in ((id,ClosureFunc(e,env,(rev_list names),constrEnv)) :: env,mem,flux)
  |ASTFunRec(id,typs,args,e) -> let (env,mem,flux) = context in let names = extractNamesArgs args in ((id,ClosureFuncRec(id,e,env,(rev_list names),constrEnv)) :: env,mem,flux)
  |ASTVar(id,typ) -> let (env,mem,flux) = context  in let name_addr = "a"^(string_of_int(!cnt)) in incr cnt;((id,AddId(name_addr)) :: env,(name_addr,Any) :: mem,flux)
  |ASTProc(id,args,prog) -> let (env,mem,flux) = context in let names = extractNamesArgs args in  ((id,ClosureProc(prog,env,(rev_list names),constrEnv)) :: env,mem,flux)
  |ASTProcRec(id,args,prog) -> let (env,mem,flux) = context in let names = extractNamesArgs args in ((id,ClosureProcRec(id,prog,env,(rev_list names),constrEnv)) :: env,mem,flux)

and evalCmds context cmd = match cmd with
  ASTStat(st) -> evalStat context st
  |ASTDec(dec)-> evalDec context dec
  
and evalProg prog context = match prog with
	ASTProg cmds -> List.fold_left evalCmds context cmds 


let _ = let lexbuf = Lexing.from_channel stdin in
  let progAst = Parser.main Lexer.token lexbuf in
  let (env,mem,flux) =  evalProg progAst ([],[],[]) in List.iter (function x -> Printf.printf " %d " (v x)) (List.rev flux); print_newline();;





