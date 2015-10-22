%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token UEDGE REDGE
%token RETURN IF ELSE FOR WHILE DEF IN 
%token BOOL NUM STRING NODE GRAPH
%token <string> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc IN
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   DEF LT ID GT ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $5;
	 formals = $7;
	 locals = List.rev $10;
	 body = List.rev $11 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
|  prim_decl_list { $1 }
|  node_decl_list { $1 }
|  graph_decl_list { $1 }

/* primitive typenames */
prim_type:
| BOOL { "bool" }
| NUM { "num" }
| STRING { "string" }

/* chained primitives can assign using "=" */
prim_decl_list:
| prim_type ID SEMI { [$2] }
| prim_type ID ASSIGN expr SEMI { [$2] } /* assignment and declaration */
| prim_decl_list COMMA ID ASSIGN expr SEMI { $3 :: $1 }
| prim_decl_list COMMA ID SEMI { $3 :: $1 }

/* chained nodes can be assigned using "(value)" */
node_decl_list:
| NODE ID SEMI { [$2] }
| NODE ID LPAREN expr RPAREN SEMI { [$2] }
| node_decl_list COMMA ID LPAREN expr RPAREN SEMI { $3 :: $1 }
| node_decl_list COMMA ID SEMI { $3 :: $1 }

graph_decl_list:
| GRAPH ID SEMI { [$2] }
| graph_decl_list COMMA ID SEMI { $3 :: $1 }


edge_ops:
|  ID UEDGE ID { Undir($1, $3) }
|  ID REDGE ID { Dir($1, $3) }
|  ID REDGE LBRACKET expr RBRACKET ID { DirVal($1, $6, $4) }
|  ID UEDGE LBRACKET expr RBRACKET ID { UndirVal($1, $6, $4) }
|  ID LBRACKET expr RBRACKET UEDGE LBRACKET expr RBRACKET ID 
   { BidirVal($3, $1, $9, $7) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN ID IN ID RPAREN stmt
     { For($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
