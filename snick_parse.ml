type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | FLOAT_CONST of (string)
  | STRING_CONST of (string)
  | IDENT of (string)
  | BOOL
  | INT
  | FLOAT
  | ASSIGN
  | READ
  | WRITE
  | VAL
  | REF
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | EQ
  | LT
  | GT
  | LTEQ
  | GTEQ
  | NONEQ
  | PLUS
  | MINUS
  | MUL
  | DIVIDE
  | UMINUS
  | SEMICOLON
  | COMMA
  | DOTDOT
  | OR
  | AND
  | NOT
  | END
  | PROC
  | WHILE
  | DO
  | OD
  | IF
  | THEN
  | ELSE
  | FI
  | EOF

open Parsing;;
# 12 "snick_parse.mly"
open Snick_ast
# 51 "snick_parse.ml"
let yytransl_const = [|
  262 (* BOOL *);
  263 (* INT *);
  264 (* FLOAT *);
  265 (* ASSIGN *);
  266 (* READ *);
  267 (* WRITE *);
  268 (* VAL *);
  269 (* REF *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* LBRAC *);
  273 (* RBRAC *);
  274 (* EQ *);
  275 (* LT *);
  276 (* GT *);
  277 (* LTEQ *);
  278 (* GTEQ *);
  279 (* NONEQ *);
  280 (* PLUS *);
  281 (* MINUS *);
  282 (* MUL *);
  283 (* DIVIDE *);
  284 (* UMINUS *);
  285 (* SEMICOLON *);
  286 (* COMMA *);
  287 (* DOTDOT *);
  288 (* OR *);
  289 (* AND *);
  290 (* NOT *);
  291 (* END *);
  292 (* PROC *);
  293 (* WHILE *);
  294 (* DO *);
  295 (* OD *);
  296 (* IF *);
  297 (* THEN *);
  298 (* ELSE *);
  299 (* FI *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL_CONST *);
  258 (* INT_CONST *);
  259 (* FLOAT_CONST *);
  260 (* STRING_CONST *);
  261 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\006\000\006\000\007\000\
\007\000\008\000\008\000\009\000\009\000\009\000\005\000\010\000\
\010\000\012\000\012\000\013\000\013\000\014\000\014\000\011\000\
\011\000\015\000\015\000\016\000\016\000\016\000\016\000\021\000\
\021\000\021\000\020\000\020\000\022\000\022\000\023\000\023\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\025\000\
\025\000\025\000\026\000\026\000\026\000\027\000\027\000\028\000\
\028\000\028\000\018\000\018\000\029\000\029\000\029\000\029\000\
\019\000\017\000\017\000\017\000\000\000"

let yylen = "\002\000\
\001\000\002\000\000\000\004\000\004\000\002\000\000\000\003\000\
\004\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\000\000\003\000\006\000\002\000\000\000\004\000\003\000\002\000\
\001\000\002\000\001\000\003\000\002\000\002\000\004\000\003\000\
\001\000\000\000\003\000\001\000\003\000\001\000\002\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\001\000\002\000\001\000\003\000\
\001\000\001\000\001\000\004\000\001\000\001\000\001\000\001\000\
\001\000\005\000\007\000\005\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\069\000\000\000\000\000\002\000\000\000\017\000\
\007\000\000\000\000\000\000\000\004\000\000\000\012\000\013\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\025\000\000\000\027\000\000\000\010\000\011\000\005\000\006\000\
\000\000\000\000\000\000\000\000\029\000\061\000\062\000\063\000\
\064\000\000\000\000\000\000\000\057\000\000\000\000\000\038\000\
\000\000\000\000\000\000\053\000\055\000\058\000\000\000\000\000\
\000\000\024\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\054\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\018\000\028\000\000\000\000\000\031\000\000\000\
\060\000\056\000\000\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\000\052\000\000\000\000\000\
\000\000\009\000\000\000\068\000\000\000\066\000\000\000\000\000\
\020\000\000\000\000\000\019\000\067\000\000\000\022\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\008\000\010\000\012\000\032\000\033\000\
\022\000\011\000\023\000\024\000\105\000\113\000\025\000\026\000\
\027\000\045\000\084\000\062\000\063\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\013\255\055\255\000\000\065\255\000\000\
\000\000\047\255\018\255\111\255\000\000\106\255\000\000\000\000\
\000\000\080\255\085\255\085\255\085\255\120\255\067\255\000\000\
\000\000\118\255\000\000\146\255\000\000\000\000\000\000\000\000\
\089\255\085\255\085\255\134\255\000\000\000\000\000\000\000\000\
\000\000\085\255\113\255\107\255\000\000\126\255\139\255\000\000\
\110\255\077\255\049\255\000\000\000\000\000\000\226\254\059\255\
\034\255\000\000\000\000\085\255\170\255\126\255\022\255\003\255\
\242\254\000\000\110\255\085\255\085\255\107\255\107\255\107\255\
\107\255\107\255\107\255\107\255\107\255\107\255\107\255\067\255\
\067\255\000\000\000\000\000\000\126\255\148\255\000\000\085\255\
\000\000\000\000\139\255\000\000\077\255\077\255\077\255\077\255\
\077\255\077\255\049\255\049\255\000\000\000\000\025\255\004\255\
\036\255\000\000\126\255\000\000\067\255\000\000\149\255\150\255\
\000\000\011\255\179\255\000\000\000\000\153\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\195\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\187\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\162\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\255\050\255\119\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\171\255\002\255\000\000\
\123\000\219\255\144\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\044\255\000\000\000\000\
\000\000\000\000\133\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\174\255\141\255\000\000\000\000\
\000\000\000\000\143\000\000\000\244\255\013\000\038\000\063\000\
\088\000\113\000\169\255\194\255\000\000\000\000\000\000\000\000\
\000\000\000\000\051\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\054\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\171\000\000\000\186\255\000\000\000\000\000\000\236\255\000\000\
\000\000\245\255\000\000\241\255\170\000\138\000\139\000\176\000\
\115\001\029\000\092\000\178\000\000\000"

let yytablesize = 446
let yytable = "\028\000\
\090\000\068\000\058\000\046\000\055\000\056\000\037\000\080\000\
\014\000\103\000\104\000\028\000\001\000\018\000\019\000\014\000\
\036\000\068\000\036\000\089\000\018\000\019\000\014\000\015\000\
\016\000\017\000\065\000\018\000\019\000\014\000\036\000\036\000\
\088\000\036\000\018\000\019\000\087\000\111\000\114\000\036\000\
\020\000\034\000\036\000\021\000\085\000\109\000\110\000\020\000\
\005\000\082\000\021\000\088\000\112\000\117\000\020\000\023\000\
\034\000\021\000\033\000\007\000\033\000\020\000\083\000\108\000\
\021\000\032\000\034\000\032\000\028\000\028\000\023\000\014\000\
\107\000\033\000\078\000\079\000\018\000\019\000\009\000\034\000\
\032\000\013\000\058\000\058\000\036\000\038\000\039\000\040\000\
\041\000\036\000\068\000\028\000\028\000\058\000\015\000\016\000\
\017\000\028\000\042\000\081\000\076\000\077\000\028\000\020\000\
\099\000\100\000\021\000\038\000\039\000\040\000\041\000\036\000\
\043\000\038\000\039\000\040\000\041\000\036\000\044\000\034\000\
\042\000\035\000\029\000\030\000\057\000\031\000\042\000\070\000\
\071\000\072\000\073\000\074\000\075\000\059\000\043\000\059\000\
\059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
\059\000\059\000\059\000\059\000\059\000\035\000\059\000\059\000\
\008\000\008\000\060\000\008\000\059\000\068\000\050\000\059\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\101\000\102\000\069\000\050\000\050\000\086\000\050\000\
\050\000\106\000\116\000\115\000\118\000\050\000\119\000\048\000\
\050\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\001\000\059\000\015\000\048\000\048\000\030\000\
\048\000\048\000\065\000\061\000\064\000\091\000\048\000\092\000\
\049\000\048\000\049\000\049\000\049\000\049\000\049\000\049\000\
\049\000\049\000\049\000\067\000\066\000\000\000\049\000\049\000\
\000\000\049\000\049\000\000\000\000\000\000\000\000\000\049\000\
\000\000\047\000\049\000\047\000\047\000\047\000\047\000\047\000\
\047\000\047\000\000\000\000\000\000\000\000\000\000\000\047\000\
\047\000\000\000\047\000\047\000\000\000\000\000\000\000\000\000\
\047\000\000\000\041\000\047\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\000\000\000\000\000\000\000\000\000\000\
\041\000\041\000\000\000\041\000\041\000\000\000\000\000\000\000\
\000\000\041\000\000\000\043\000\041\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\000\000\000\000\000\000\000\000\
\000\000\043\000\043\000\000\000\043\000\043\000\000\000\000\000\
\000\000\000\000\043\000\000\000\045\000\043\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\000\000\000\000\000\000\
\000\000\000\000\045\000\045\000\000\000\045\000\045\000\000\000\
\000\000\000\000\000\000\045\000\000\000\044\000\045\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\000\000\000\000\
\000\000\000\000\000\000\044\000\044\000\000\000\044\000\044\000\
\000\000\000\000\000\000\000\000\044\000\000\000\046\000\044\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\000\000\
\000\000\000\000\000\000\000\000\046\000\046\000\000\000\046\000\
\046\000\000\000\000\000\000\000\000\000\046\000\000\000\042\000\
\046\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\000\000\040\000\000\000\040\000\000\000\042\000\042\000\000\000\
\042\000\042\000\000\000\039\000\000\000\039\000\042\000\040\000\
\040\000\042\000\040\000\040\000\000\000\035\000\000\000\035\000\
\040\000\039\000\039\000\040\000\039\000\039\000\000\000\000\000\
\000\000\000\000\039\000\035\000\035\000\039\000\035\000\000\000\
\000\000\000\000\000\000\000\000\035\000\000\000\000\000\035\000\
\093\000\094\000\095\000\096\000\097\000\098\000"

let yycheck = "\011\000\
\015\001\032\001\023\000\019\000\020\000\021\000\018\000\038\001\
\005\001\080\000\081\000\023\000\001\000\010\001\011\001\005\001\
\015\001\032\001\017\001\017\001\010\001\011\001\005\001\006\001\
\007\001\008\001\042\000\010\001\011\001\005\001\029\001\030\001\
\030\001\032\001\010\001\011\001\015\001\002\001\109\000\038\001\
\037\001\015\001\041\001\040\001\060\000\042\001\043\001\037\001\
\036\001\016\001\040\001\030\001\017\001\043\001\037\001\002\001\
\030\001\040\001\015\001\005\001\017\001\037\001\029\001\039\001\
\040\001\015\001\017\001\017\001\080\000\081\000\017\001\005\001\
\088\000\030\001\026\001\027\001\010\001\011\001\014\001\030\001\
\030\001\035\001\103\000\104\000\005\001\001\001\002\001\003\001\
\004\001\005\001\032\001\103\000\104\000\114\000\006\001\007\001\
\008\001\109\000\014\001\041\001\024\001\025\001\114\000\037\001\
\076\000\077\000\040\001\001\001\002\001\003\001\004\001\005\001\
\028\001\001\001\002\001\003\001\004\001\005\001\034\001\014\001\
\014\001\016\001\012\001\013\001\005\001\015\001\014\001\018\001\
\019\001\020\001\021\001\022\001\023\001\015\001\028\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\029\001\029\001\030\001\016\001\032\001\033\001\
\012\001\013\001\009\001\015\001\038\001\032\001\015\001\041\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\078\000\079\000\033\001\029\001\030\001\005\001\032\001\
\033\001\030\001\029\001\031\001\002\001\038\001\030\001\015\001\
\041\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\000\000\009\001\035\001\029\001\030\001\029\001\
\032\001\033\001\029\001\033\000\035\000\068\000\038\001\069\000\
\015\001\041\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\044\000\043\000\255\255\029\001\030\001\
\255\255\032\001\033\001\255\255\255\255\255\255\255\255\038\001\
\255\255\015\001\041\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\255\255\255\255\255\255\255\255\255\255\029\001\
\030\001\255\255\032\001\033\001\255\255\255\255\255\255\255\255\
\038\001\255\255\015\001\041\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\255\255\255\255\255\255\
\029\001\030\001\255\255\032\001\033\001\255\255\255\255\255\255\
\255\255\038\001\255\255\015\001\041\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\255\255\255\255\255\255\255\255\
\255\255\029\001\030\001\255\255\032\001\033\001\255\255\255\255\
\255\255\255\255\038\001\255\255\015\001\041\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\255\255\255\255\255\255\
\255\255\255\255\029\001\030\001\255\255\032\001\033\001\255\255\
\255\255\255\255\255\255\038\001\255\255\015\001\041\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\255\255\255\255\
\255\255\255\255\255\255\029\001\030\001\255\255\032\001\033\001\
\255\255\255\255\255\255\255\255\038\001\255\255\015\001\041\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\255\255\
\255\255\255\255\255\255\255\255\029\001\030\001\255\255\032\001\
\033\001\255\255\255\255\255\255\255\255\038\001\255\255\015\001\
\041\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\255\255\015\001\255\255\017\001\255\255\029\001\030\001\255\255\
\032\001\033\001\255\255\015\001\255\255\017\001\038\001\029\001\
\030\001\041\001\032\001\033\001\255\255\015\001\255\255\017\001\
\038\001\029\001\030\001\041\001\032\001\033\001\255\255\255\255\
\255\255\255\255\038\001\029\001\030\001\041\001\032\001\255\255\
\255\255\255\255\255\255\255\255\038\001\255\255\255\255\041\001\
\070\000\071\000\072\000\073\000\074\000\075\000"

let yynames_const = "\
  BOOL\000\
  INT\000\
  FLOAT\000\
  ASSIGN\000\
  READ\000\
  WRITE\000\
  VAL\000\
  REF\000\
  LPAREN\000\
  RPAREN\000\
  LBRAC\000\
  RBRAC\000\
  EQ\000\
  LT\000\
  GT\000\
  LTEQ\000\
  GTEQ\000\
  NONEQ\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIVIDE\000\
  UMINUS\000\
  SEMICOLON\000\
  COMMA\000\
  DOTDOT\000\
  OR\000\
  AND\000\
  NOT\000\
  END\000\
  PROC\000\
  WHILE\000\
  DO\000\
  OD\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL_CONST\000\
  INT_CONST\000\
  FLOAT_CONST\000\
  STRING_CONST\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'procs) in
    Obj.repr(
# 57 "snick_parse.mly"
          ( {procs = List.rev _1})
# 361 "snick_parse.ml"
               : Snick_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 61 "snick_parse.mly"
               ( _2 :: _1)
# 369 "snick_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "snick_parse.mly"
    ( [] )
# 375 "snick_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'head) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'body) in
    Obj.repr(
# 66 "snick_parse.mly"
                       ( {head = _2 ; body = _3} )
# 383 "snick_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    Obj.repr(
# 71 "snick_parse.mly"
      (begin 
          ProcHead (_1, List.rev _3);
          end)
# 393 "snick_parse.ml"
               : 'head))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 76 "snick_parse.mly"
                 (_2 :: _1)
# 401 "snick_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "snick_parse.mly"
    ( [] )
# 407 "snick_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'indicator) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'beantype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "snick_parse.mly"
                             (Param (_1, _2, _3))
# 416 "snick_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'indicator) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'beantype) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 81 "snick_parse.mly"
                                   (Param (_1, _2, _3))
# 425 "snick_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "snick_parse.mly"
        (IndiVal)
# 431 "snick_parse.ml"
               : 'indicator))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "snick_parse.mly"
        (IndiRef)
# 437 "snick_parse.ml"
               : 'indicator))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "snick_parse.mly"
         ( Bool )
# 443 "snick_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "snick_parse.mly"
        ( Int )
# 449 "snick_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "snick_parse.mly"
          ( Float )
# 455 "snick_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 94 "snick_parse.mly"
              (ProcBody (List.rev _1, List.rev _2))
# 463 "snick_parse.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 98 "snick_parse.mly"
               ( _2 :: _1 )
# 471 "snick_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "snick_parse.mly"
    ( [] )
# 477 "snick_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'beantype) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 102 "snick_parse.mly"
                             ( DeclVar (_1,  _2) )
# 485 "snick_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'beantype) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'intervals) in
    Obj.repr(
# 103 "snick_parse.mly"
                                                   ( DecArr (_1, _2, List.rev _4))
# 494 "snick_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'intervals) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'interval) in
    Obj.repr(
# 106 "snick_parse.mly"
                       ( _2 :: _1)
# 502 "snick_parse.ml"
               : 'intervals))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "snick_parse.mly"
    ( [])
# 508 "snick_parse.ml"
               : 'intervals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 110 "snick_parse.mly"
                                     (Interval (_1, _3))
# 516 "snick_parse.ml"
               : 'interval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "snick_parse.mly"
                               (Interval (_1, _3))
# 524 "snick_parse.ml"
               : 'interval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 114 "snick_parse.mly"
               ( _2 :: _1)
# 532 "snick_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 115 "snick_parse.mly"
         ( _1 :: [] )
# 539 "snick_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'astmt) in
    Obj.repr(
# 118 "snick_parse.mly"
                    ( Atomic _1 )
# 546 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cstmt) in
    Obj.repr(
# 119 "snick_parse.mly"
          ( Composite _1 )
# 553 "snick_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 122 "snick_parse.mly"
                         ( Assign (_1, _3))
# 561 "snick_parse.ml"
               : 'astmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 123 "snick_parse.mly"
                ( Read _2 )
# 568 "snick_parse.ml"
               : 'astmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "snick_parse.mly"
               ( Write _2 )
# 575 "snick_parse.ml"
               : 'astmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprlist) in
    Obj.repr(
# 125 "snick_parse.mly"
                                 ( ProcCall ( _1, List.rev _3) )
# 583 "snick_parse.ml"
               : 'astmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "snick_parse.mly"
                        (_3 :: _1)
# 591 "snick_parse.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "snick_parse.mly"
         (_1 :: [])
# 598 "snick_parse.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "snick_parse.mly"
    ( [] )
# 604 "snick_parse.ml"
               : 'exprlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 133 "snick_parse.mly"
                  (Ebinop (_1, Op_or, _3))
# 612 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 134 "snick_parse.mly"
          (_1)
# 619 "snick_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 137 "snick_parse.mly"
                    (Ebinop (_1, Op_and, _3))
# 627 "snick_parse.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 138 "snick_parse.mly"
          (_1)
# 634 "snick_parse.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 141 "snick_parse.mly"
              (Eunop (Op_not, _2))
# 641 "snick_parse.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 142 "snick_parse.mly"
          (_1)
# 648 "snick_parse.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 145 "snick_parse.mly"
                   (Ebinop (_1, Op_eq, _3))
# 656 "snick_parse.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 146 "snick_parse.mly"
                      (Ebinop (_1, Op_nEq, _3))
# 664 "snick_parse.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 147 "snick_parse.mly"
                   (Ebinop (_1, Op_lt, _3))
# 672 "snick_parse.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 148 "snick_parse.mly"
                     (Ebinop (_1, Op_ltEq, _3))
# 680 "snick_parse.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 149 "snick_parse.mly"
                   (Ebinop (_1, Op_gt, _3))
# 688 "snick_parse.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 150 "snick_parse.mly"
                     (Ebinop (_1, Op_gtEq, _3))
# 696 "snick_parse.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 151 "snick_parse.mly"
          ( _1)
# 703 "snick_parse.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 154 "snick_parse.mly"
                     (Ebinop (_1, Op_add, _3))
# 711 "snick_parse.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 155 "snick_parse.mly"
                      (Ebinop (_1, Op_sub, _3))
# 719 "snick_parse.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 156 "snick_parse.mly"
          (_1)
# 726 "snick_parse.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 159 "snick_parse.mly"
                    (Ebinop (_1, Op_mul, _3))
# 734 "snick_parse.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 160 "snick_parse.mly"
                       (Ebinop (_1, Op_div, _3))
# 742 "snick_parse.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 161 "snick_parse.mly"
          (_1)
# 749 "snick_parse.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 164 "snick_parse.mly"
                 (Eunop (Op_minus, _2))
# 756 "snick_parse.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 165 "snick_parse.mly"
          (_1)
# 763 "snick_parse.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 168 "snick_parse.mly"
                       (_2)
# 770 "snick_parse.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 169 "snick_parse.mly"
           (Elval _1)
# 777 "snick_parse.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 170 "snick_parse.mly"
          (Const _1)
# 784 "snick_parse.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 174 "snick_parse.mly"
          ( LId ( _1))
# 791 "snick_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprlist) in
    Obj.repr(
# 176 "snick_parse.mly"
          (match _3 with
              | [] -> raise (Except "empty list between []\n")
              | _ -> LField ( _1, List.rev _3))
# 801 "snick_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 181 "snick_parse.mly"
               ( Ebool _1 )
# 808 "snick_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 182 "snick_parse.mly"
              ( Eint _1 )
# 815 "snick_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 183 "snick_parse.mly"
                ( Efloat (float_of_string _1))
# 822 "snick_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "snick_parse.mly"
                 ( EString _1)
# 829 "snick_parse.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "snick_parse.mly"
       ( Rexpr _1 )
# 836 "snick_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 190 "snick_parse.mly"
                          ( IfThenFi (_2, List.rev _4))
# 844 "snick_parse.ml"
               : 'cstmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 191 "snick_parse.mly"
                                     (IfThenElseFi (_2, List.rev _4, List.rev _6))
# 853 "snick_parse.ml"
               : 'cstmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 192 "snick_parse.mly"
                           (WhileDo (_2, List.rev _4))
# 861 "snick_parse.ml"
               : 'cstmt))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Snick_ast.program)
