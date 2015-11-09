type token =
  | SEMI
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | LOGAND
  | LOGOR
  | UEDGE
  | REDGE
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | DEF
  | IN
  | BOOL
  | NUM
  | STRING
  | NODE
  | GRAPH
  | LIST
  | DICT
  | TRUE
  | FALSE
  | INF
  | LITERAL of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
