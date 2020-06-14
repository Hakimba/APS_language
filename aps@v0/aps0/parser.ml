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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 39 "parser.ml"
let yytransl_const = [|
  257 (* CONST *);
  258 (* PRINT *);
  259 (* FUN *);
  260 (* REC *);
  261 (* ECHO *);
  264 (* NOT *);
  265 (* AND *);
  266 (* OR *);
  267 (* EQ *);
  268 (* LT *);
  269 (* ADD *);
  270 (* SUB *);
  271 (* MUL *);
  272 (* DIV *);
  273 (* IF *);
  274 (* COMMA *);
  275 (* SEMICOLON *);
  276 (* LPAR *);
  277 (* RPAR *);
  278 (* LBRACKET *);
  279 (* RBRACKET *);
  280 (* STAR *);
  281 (* ARROW *);
  282 (* COLON *);
  283 (* TBOOL *);
  284 (* TINT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  262 (* TRUE *);
  263 (* FALSE *);
  285 (* INT *);
  286 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\003\000\003\000\005\000\
\005\000\005\000\007\000\007\000\008\000\006\000\006\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\009\000\
\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\002\000\006\000\009\000\010\000\004\000\001\000\
\001\000\005\000\001\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\006\000\004\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\001\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\000\018\000\000\000\000\000\
\016\000\029\000\000\000\002\000\000\000\008\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\000\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\031\000\013\000\
\030\000\015\000\012\000\000\000\004\000\000\000\000\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\000\000\
\010\000\000\000\000\000\019\000\000\000\000\000\000\000\005\000\
\006\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\057\000\042\000\039\000\043\000\040\000\
\058\000"

let yysindex = "\001\000\
\240\254\000\000\009\255\000\000\000\000\229\254\000\255\065\255\
\244\254\237\254\239\254\237\254\000\000\000\000\048\255\241\254\
\000\000\000\000\255\254\000\000\237\254\000\000\000\000\065\255\
\237\254\011\255\065\255\065\255\065\255\065\255\065\255\065\255\
\065\255\065\255\065\255\065\255\065\255\005\255\012\255\020\255\
\009\255\015\255\025\255\032\255\031\255\241\254\052\255\065\255\
\065\255\065\255\065\255\065\255\065\255\065\255\065\255\065\255\
\065\255\053\255\237\254\065\255\241\254\000\000\237\254\237\254\
\009\255\241\254\057\255\000\000\054\255\055\255\060\255\061\255\
\063\255\068\255\070\255\071\255\065\255\000\000\000\000\000\000\
\000\000\000\000\000\000\072\255\000\000\073\255\065\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\076\255\
\000\000\065\255\067\255\000\000\079\255\009\255\009\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\077\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\078\255\
\000\000\074\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\081\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\220\255\248\255\024\000\227\255\040\000\000\000\
\047\000"

let yytablesize = 104
let yytable = "\019\000\
\021\000\001\000\010\000\011\000\062\000\003\000\037\000\022\000\
\023\000\006\000\020\000\007\000\025\000\008\000\038\000\044\000\
\067\000\041\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\085\000\012\000\059\000\082\000\
\046\000\024\000\060\000\026\000\086\000\061\000\063\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\045\000\064\000\065\000\081\000\066\000\013\000\014\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\104\000\105\000\015\000\096\000\016\000\013\000\014\000\
\068\000\079\000\088\000\089\000\017\000\018\000\099\000\087\000\
\090\000\091\000\080\000\092\000\015\000\102\000\016\000\084\000\
\093\000\101\000\094\000\095\000\097\000\017\000\018\000\098\000\
\100\000\103\000\011\000\003\000\014\000\032\000\083\000\078\000"

let yycheck = "\008\000\
\020\001\001\000\030\001\004\001\041\000\022\001\015\000\027\001\
\028\001\001\001\023\001\003\001\030\001\005\001\030\001\024\000\
\046\000\019\001\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\065\000\030\001\026\001\061\000\
\022\001\010\000\023\001\012\000\066\000\018\001\024\001\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\025\000\025\001\019\001\060\000\022\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\102\000\103\000\020\001\077\000\022\001\006\001\007\001\
\021\001\021\001\021\001\021\001\029\001\030\001\087\000\023\001\
\021\001\021\001\059\000\021\001\020\001\019\001\022\001\064\000\
\021\001\098\000\021\001\021\001\021\001\029\001\030\001\023\001\
\021\001\019\001\025\001\023\001\023\001\021\001\063\000\057\000"

let yynames_const = "\
  CONST\000\
  PRINT\000\
  FUN\000\
  REC\000\
  ECHO\000\
  NOT\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  IF\000\
  COMMA\000\
  SEMICOLON\000\
  LPAR\000\
  RPAR\000\
  LBRACKET\000\
  RBRACKET\000\
  STAR\000\
  ARROW\000\
  COLON\000\
  TBOOL\000\
  TINT\000\
  EOF\000\
  "

let yynames_block = "\
  TRUE\000\
  FALSE\000\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 14 "parser.mly"
           (_1)
# 222 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 17 "parser.mly"
                             (ASTProg(_2))
# 229 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 21 "parser.mly"
                 ([ASTEcho(_2)])
# 236 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 22 "parser.mly"
                                     (ASTConst(_2,_3,_4) :: _6)
# 246 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 23 "parser.mly"
                                                             (ASTFun(_2,_3,_5,_7) :: _9)
# 257 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 24 "parser.mly"
                                                                 (ASTFunRec(_3,_4,_6,_8) :: _10)
# 268 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 25 "parser.mly"
                                (ASTEcho(_2) :: _4)
# 276 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
             (ASTType(Ast.TBool))
# 282 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
             (ASTType(Ast.TInt))
# 288 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 29 "parser.mly"
                                 (ASTTypeFunc(_2,_4))
# 296 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 31 "parser.mly"
          ([_1])
# 303 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 32 "parser.mly"
                      (_1 :: _3)
# 311 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 34 "parser.mly"
                  (ASTArg(_1,_3))
# 319 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 36 "parser.mly"
          ([_1])
# 326 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 37 "parser.mly"
                   (_1 :: _3)
# 334 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 39 "parser.mly"
          (ASTInt(_1))
# 341 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 40 "parser.mly"
         (ASTBool(_1))
# 348 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 41 "parser.mly"
          (ASTBool(_1))
# 355 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                                (ASTIf(_3,_4,_5))
# 364 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                       (ASTUnPrim(Ast.Not,_3))
# 371 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                            (ASTBinPrim(Ast.And,_3,_4))
# 379 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                           (ASTBinPrim(Ast.Or,_3,_4))
# 387 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                           (ASTBinPrim(Ast.Eq,_3,_4))
# 395 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                           (ASTBinPrim(Ast.Lt,_3,_4))
# 403 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                            (ASTBinPrim(Ast.Add,_3,_4))
# 411 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                            (ASTBinPrim(Ast.Sub,_3,_4))
# 419 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                            (ASTBinPrim(Ast.Mul,_3,_4))
# 427 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                            (ASTBinPrim(Ast.Div,_3,_4))
# 435 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
       (ASTId(_1))
# 442 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                                (ASTAbs(_2,_4))
# 450 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 54 "parser.mly"
                         (ASTApp(_2,_3))
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
            ([_1])
# 465 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 58 "parser.mly"
               (_1 :: _2)
# 473 "parser.ml"
               : 'exprs))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
