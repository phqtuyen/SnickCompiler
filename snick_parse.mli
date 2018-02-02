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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Snick_ast.program
