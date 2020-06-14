checkTypes([], _, []).
checkTypes([E|Es],Env,[T|Ts]) :- typeExpr(E,Env,T) ,checkTypes(Es,Env,Ts).

memT(X,[(X,T)|_],T).
memT(X,[_|Xs],T) :- memT(X,Xs,T).

assoc(K,[(K,V)|_],V).
assoc(K,[_|KVS],V) :- assoc(K,KVS,V).

concat([],YS,YS).
concat([X|XS],YS,[X|ZS]) :- concat(XS,YS,ZS).

tronc([Expr],[(_,Expr)]).
tronc([X|Expr],[(_,X)|ITS]) :- tronc(Expr,ITS).

typeExpr(N,_,int) :- integer(N).
typeExpr(true,_,bool).
typeExpr(false,_,bool).
typeExpr(Str,Env,T) :- string(Str), memT(Str,Env,T).
typeExpr(not(Op),Env,T) :- typeExpr(Op,Env,T), T == bool.
typeExpr(and(OpL,OpR),Env,T) :- typeExpr(OpL,Env,T),typeExpr(OpR,Env,T), T == bool.
typeExpr(or(OpL,OpR),Env,T) :- typeExpr(OpL,Env,T),typeExpr(OpR,Env,T), T == bool.
typeExpr(eq(OpL,OpR),Env,bool) :- typeExpr(OpL,Env,T),typeExpr(OpR,Env,T), T == int.
typeExpr(lt(OpL,OpR),Env,bool) :- typeExpr(OpL,Env,T),typeExpr(OpR,Env,T), T == int.
typeExpr(add(OpL,OpR),Env,T) :- typeExpr(OpL,Env,T),typeExpr(OpR,Env,T), T == int.
typeExpr(sub(OpL,OpR),Env,T) :- typeExpr(OpL,Env,T),typeExpr(OpR,Env,T), T == int.
typeExpr(mul(OpL,OpR),Env,T) :- typeExpr(OpL,Env,T),typeExpr(OpR,Env,T), T == int.
typeExpr(div(OpL,OpR),Env,T) :- typeExpr(OpL,Env,T),typeExpr(OpR,Env,T), T == int.
typeExpr(abs(Exprs,Expr),Env,arrow(Targs,T)) :- tronc(Targs,Exprs), concat(Exprs,Env,Res), typeExpr(Expr,Res,T).
typeExpr(if(Cond,Cons,Alter),Env,T2) :- typeExpr(Cond,Env,bool) , typeExpr(Cons,Env,T2), typeExpr(Alter,Env,T2).
typeExpr(app(E,Es),Env,T) :-  checkTypes(Es,Env,Ts), typeExpr(E,Env,arrow(Ts,T)).

typeStat(echo(Expr),Env,void) :-  typeExpr(Expr,Env,T), T == int.

typeDec(const(X,T,E),Env,[(X,T)|Env]) :- typeExpr(E,Env,T).
typeDec(fun(X,T,Args,Expr),Env,[(X,arrow(Targs,T))|Env]) :- concat(Args,Env,Res), tronc(Targs,Args), typeExpr(Expr,Res,T).
typeDec(funRec(X,T,Args,Expr),Env,[(X,arrow(Targs,T))|Env]) :- concat(Args,Env,Res), tronc(Targs,Args), typeExpr(Expr,[(X,arrow(Targs,T))|Res],T).

typeCmds([Dec|Cs],Env,void) :- typeDec(Dec,Env,NewEnv), typeCmds(Cs,NewEnv,void).
typeCmds([Stat|Cs],Env,void) :- typeStat(Stat,Env,void), typeCmds(Cs,Env,void).
typeCmds([],_,void).

typeProg(prog(Cs),void) :- typeCmds(Cs,[],void).

main_stdin :- 
	read(user_input,T),
	typeProg(T,R),
	print(R),
	nl.
