%{ open Ast %}

%token CONST PRINT FUN REC ECHO TRUE FALSE NOT AND OR EQ LT ADD SUB MUL DIV IF COMMA SEMICOLON LPAR RPAR LBRACKET RBRACKET STAR ARROW COLON TBOOL TINT COLON
%token EOF
%token <int> INT
%token <bool> TRUE
%token <bool> FALSE
%token <string> ID
%start main
%type <Ast.prog> main
%%

main:
      prog {$1}
;

prog: LBRACKET cmds RBRACKET {ASTProg($2)}
;

cmds:
     | ECHO expr {[ASTEcho($2)]}
  | CONST ID typ expr SEMICOLON cmds {ASTConst($2,$3,$4) :: $6}
     | FUN ID typ LBRACKET args RBRACKET expr SEMICOLON cmds {ASTFun($2,$3,$5,$7) :: $9} 
     | FUN REC ID typ LBRACKET args RBRACKET expr SEMICOLON cmds {ASTFunRec($3,$4,$6,$8) :: $10}
     | ECHO expr SEMICOLON cmds {ASTEcho($2) :: $4}
;
typ:   TBOOL {ASTType(Ast.TBool)}
      | TINT {ASTType(Ast.TInt)}
      | LPAR typs ARROW typ RPAR {ASTTypeFunc($2,$4)};

typs: typ {[$1]}
      | typ STAR typs {$1 :: $3}

arg: ID COLON typ {ASTArg($1,$3)};

args: arg {[$1]}
  | arg COMMA args {$1 :: $3};

expr: INT {ASTInt($1)}
  | TRUE {ASTBool($1)}
  | FALSE {ASTBool($1)}
  | LPAR IF expr expr expr RPAR {ASTIf($3,$4,$5)}
  | LPAR NOT expr RPAR {ASTUnPrim(Ast.Not,$3)}
  | LPAR AND expr expr RPAR {ASTBinPrim(Ast.And,$3,$4)}
  | LPAR OR expr expr RPAR {ASTBinPrim(Ast.Or,$3,$4)}
  | LPAR EQ expr expr RPAR {ASTBinPrim(Ast.Eq,$3,$4)}
  | LPAR LT expr expr RPAR {ASTBinPrim(Ast.Lt,$3,$4)}
  | LPAR ADD expr expr RPAR {ASTBinPrim(Ast.Add,$3,$4)}
  | LPAR SUB expr expr RPAR {ASTBinPrim(Ast.Sub,$3,$4)}
  | LPAR MUL expr expr RPAR {ASTBinPrim(Ast.Mul,$3,$4)}
  | LPAR DIV expr expr RPAR {ASTBinPrim(Ast.Div,$3,$4)}
  | ID {ASTId($1)}
  | LBRACKET args RBRACKET expr {ASTAbs($2,$4)}
  | LPAR expr exprs RPAR {ASTApp($2,$3)}
;

exprs: expr {[$1]}
  | expr exprs {$1 :: $2}
;
