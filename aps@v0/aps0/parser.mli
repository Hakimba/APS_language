type token =
  | CONST
  | PRINT
  | FUN
  | REC
  | ECHO
  | TRUE of (bool)
  | FALSE of (bool)
  | NOT
  | AND
  | OR
  | EQ
  | LT
  | ADD
  | SUB
  | MUL
  | DIV
  | IF
  | COMMA
  | SEMICOLON
  | LPAR
  | RPAR
  | LBRACKET
  | RBRACKET
  | STAR
  | ARROW
  | COLON
  | TBOOL
  | TINT
  | EOF
  | INT of (int)
  | ID of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
