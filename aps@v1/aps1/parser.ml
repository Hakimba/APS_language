type token =
  | TVOID
  | VAR
  | PROC
  | SET
  | IFStat
  | WHILE
  | CALL
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
# 46 "parser.ml"
let yytransl_const = [|
  257 (* TVOID *);
  258 (* VAR *);
  259 (* PROC *);
  260 (* SET *);
  261 (* IFStat *);
  262 (* WHILE *);
  263 (* CALL *);
  264 (* CONST *);
  265 (* PRINT *);
  266 (* FUN *);
  267 (* REC *);
  268 (* ECHO *);
  271 (* NOT *);
  272 (* AND *);
  273 (* OR *);
  274 (* EQ *);
  275 (* LT *);
  276 (* ADD *);
  277 (* SUB *);
  278 (* MUL *);
  279 (* DIV *);
  280 (* IF *);
  281 (* COMMA *);
  282 (* SEMICOLON *);
  283 (* LPAR *);
  284 (* RPAR *);
  285 (* LBRACKET *);
  286 (* RBRACKET *);
  287 (* STAR *);
  288 (* ARROW *);
  289 (* COLON *);
  290 (* TBOOL *);
  291 (* TINT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  269 (* TRUE *);
  270 (* FALSE *);
  292 (* INT *);
  293 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\005\000\
\008\000\008\000\008\000\008\000\010\000\010\000\011\000\009\000\
\009\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\004\000\003\000\
\003\000\003\000\004\000\007\000\008\000\003\000\006\000\007\000\
\001\000\001\000\001\000\005\000\001\000\003\000\003\000\001\000\
\003\000\001\000\001\000\001\000\006\000\004\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\001\000\004\000\
\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\044\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\028\000\000\000\
\000\000\026\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\002\000\000\000\000\000\019\000\000\000\017\000\
\018\000\014\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\000\000\000\009\000\000\000\
\000\000\000\000\005\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\043\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\030\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\023\000\040\000\025\000\000\000\000\000\022\000\
\000\000\000\000\015\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\000\000\000\000\000\000\020\000\016\000\
\029\000\000\000\012\000\013\000"

let yydgoto = "\002\000\
\004\000\005\000\015\000\016\000\017\000\062\000\063\000\069\000\
\058\000\070\000\059\000"

let yysindex = "\005\000\
\245\254\000\000\085\255\000\000\000\000\238\254\247\254\239\254\
\250\254\250\254\241\254\244\254\248\254\250\254\002\255\007\255\
\008\255\026\255\254\254\018\255\250\254\000\000\000\000\091\255\
\012\255\000\000\000\000\245\254\245\254\250\254\026\255\020\255\
\026\255\000\000\000\000\085\255\085\255\000\000\026\255\000\000\
\000\000\000\000\030\255\012\255\000\000\250\254\250\254\250\254\
\250\254\250\254\250\254\250\254\250\254\250\254\250\254\250\254\
\029\255\033\255\039\255\245\254\000\000\250\254\000\000\250\254\
\026\255\045\255\000\000\000\000\047\255\048\255\012\255\049\255\
\053\255\250\254\250\254\250\254\250\254\250\254\250\254\250\254\
\250\254\250\254\054\255\026\255\250\254\012\255\000\000\000\000\
\000\000\055\255\012\255\026\255\026\255\064\255\245\254\000\000\
\057\255\070\255\071\255\072\255\073\255\075\255\088\255\094\255\
\250\254\000\000\000\000\000\000\000\000\012\255\089\255\000\000\
\095\255\245\254\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\096\255\099\255\250\254\000\000\000\000\
\000\000\250\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\100\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\101\255\000\000\000\000\024\255\000\000\000\000\
\000\000\000\000\000\000\000\000\093\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\244\255\233\255\000\000\000\000\247\255\204\255\249\255\
\221\255\034\000\000\000"

let yytablesize = 131
let yytable = "\028\000\
\029\000\019\000\032\000\083\000\034\000\001\000\022\000\023\000\
\072\000\088\000\042\000\045\000\067\000\068\000\056\000\060\000\
\061\000\003\000\018\000\021\000\024\000\030\000\025\000\064\000\
\031\000\066\000\038\000\020\000\033\000\026\000\027\000\035\000\
\036\000\037\000\043\000\094\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\044\000\087\000\
\057\000\042\000\109\000\042\000\039\000\042\000\089\000\111\000\
\065\000\090\000\071\000\040\000\041\000\084\000\085\000\086\000\
\097\000\098\000\099\000\100\000\101\000\102\000\103\000\104\000\
\105\000\091\000\125\000\108\000\107\000\092\000\095\000\093\000\
\096\000\106\000\115\000\110\000\116\000\113\000\006\000\007\000\
\008\000\009\000\010\000\011\000\012\000\114\000\013\000\124\000\
\014\000\117\000\118\000\119\000\120\000\128\000\121\000\022\000\
\023\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\122\000\131\000\024\000\126\000\025\000\
\132\000\123\000\127\000\129\000\021\000\112\000\026\000\027\000\
\130\000\003\000\024\000"

let yycheck = "\009\000\
\010\000\011\001\011\001\056\000\014\000\001\000\013\001\014\001\
\044\000\062\000\018\000\021\000\036\000\037\000\024\000\028\000\
\029\000\029\001\037\001\037\001\027\001\037\001\029\001\031\000\
\037\001\033\000\001\001\037\001\037\001\036\001\037\001\030\001\
\026\001\026\001\037\001\071\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\029\001\060\000\
\037\001\026\001\086\000\028\001\027\001\030\001\064\000\091\000\
\037\001\065\000\029\001\034\001\035\001\033\001\030\001\025\001\
\074\000\075\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\029\001\110\000\085\000\084\000\031\001\030\001\032\001\
\028\001\028\001\095\000\029\001\028\001\093\000\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\030\001\010\001\105\000\
\012\001\028\001\028\001\028\001\028\001\114\000\028\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\028\001\126\000\027\001\030\001\029\001\
\130\000\028\001\028\001\028\001\032\001\092\000\036\001\037\001\
\030\001\030\001\030\001"

let yynames_const = "\
  TVOID\000\
  VAR\000\
  PROC\000\
  SET\000\
  IFStat\000\
  WHILE\000\
  CALL\000\
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
# 262 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 17 "parser.mly"
                             (ASTProg(_2))
# 269 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 22 "parser.mly"
          ( [ASTStat(_1)] )
# 276 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 23 "parser.mly"
                       ( ASTDec _1 :: _3 )
# 284 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 24 "parser.mly"
                         ( ASTStat _1 :: _3 )
# 292 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
              (ASTEcho(_2))
# 299 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 29 "parser.mly"
                            (ASTIfStat(_2,_3,_4))
# 308 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 30 "parser.mly"
                      (ASTWhile(_2,_3))
# 316 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 31 "parser.mly"
                    (ASTCall(_2,_3))
# 324 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                  (ASTSet(_2,_3))
# 332 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                         (ASTConst(_2,_3,_4))
# 341 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                                              (ASTFun(_2,_3,_5,_7))
# 351 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                                                  (ASTFunRec(_3,_4,_6,_8))
# 361 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 38 "parser.mly"
                  (ASTVar(_2,_3))
# 369 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 39 "parser.mly"
                                           (ASTProc(_2,_4,_6))
# 378 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 40 "parser.mly"
                                               (ASTProcRec(_3,_5,_7))
# 387 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
             (ASTType(Ast.TBool))
# 393 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
             (ASTType(Ast.TInt))
# 399 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
              (ASTType(Ast.TVoid))
# 405 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 45 "parser.mly"
                                 (ASTTypeFunc(_2,_4))
# 413 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 47 "parser.mly"
          ([_1])
# 420 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 48 "parser.mly"
                      (_1 :: _3)
# 428 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 50 "parser.mly"
                  (ASTArg(_1,_3))
# 436 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 52 "parser.mly"
          ([_1])
# 443 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 53 "parser.mly"
                   (_1 :: _3)
# 451 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 55 "parser.mly"
          (ASTInt(_1))
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 56 "parser.mly"
         (ASTBool(_1))
# 465 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 57 "parser.mly"
          (ASTBool(_1))
# 472 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                                (ASTIf(_3,_4,_5))
# 481 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                       (ASTUnPrim(Ast.Not,_3))
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                            (ASTBinPrim(Ast.And,_3,_4))
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                           (ASTBinPrim(Ast.Or,_3,_4))
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                           (ASTBinPrim(Ast.Eq,_3,_4))
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                           (ASTBinPrim(Ast.Lt,_3,_4))
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                            (ASTBinPrim(Ast.Add,_3,_4))
# 528 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                            (ASTBinPrim(Ast.Sub,_3,_4))
# 536 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                            (ASTBinPrim(Ast.Mul,_3,_4))
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                            (ASTBinPrim(Ast.Div,_3,_4))
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
       (ASTId(_1))
# 559 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                (ASTAbs(_2,_4))
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 70 "parser.mly"
                         (ASTApp(_2,_3))
# 575 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
            ([_1])
# 582 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 74 "parser.mly"
               (_1 :: _2)
# 590 "parser.ml"
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
