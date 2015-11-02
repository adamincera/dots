%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token UEDGE REDGE
%token RETURN IF ELSE FOR WHILE DEF IN
%token BOOL NUM STRING NODE GRAPH LIST DICT
%token TRUE FALSE INF
%token <string> LITERAL
%token <string> ID
%token EOF

%nonassoc NOCALL
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
|  /* nothing */ { {Vars : []; Funcs : []; Cmds : []} }
|  decls vdecl { {Vars : concat($2, $1.Vars); Funcs: $1.Funcs; Cmds : $1.Cmds} }
|  decls fdecl { {Vars : $1.Vars; Funcs: concat($2, $1.Funcs); Cmds : $1.Cmds} }
|  decls stmt  { {Vars : $1.Vars; Funcs: $1.Funcs; Cmds : $2 :: $1.Cmds} } 

fdecl:
   DEF ID ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $3;
	 formals = $5;
	 locals = List.rev $8;
	 body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

dict_formal_list:
    ID COLON expr { $1 }
|   dict_formal_list COMMA ID COLON expr { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { concat($2, $1) }

vdecl:
|  prim_decl_prefix SEMI { List.rev $1 }
|  node_decl_prefix SEMI { List.rev $1 }
|  graph_decl_prefix SEMI { List.rev $1 }
|  list_decl_prefix SEMI { List.rev $1 }
|  dict_decl_prefix SEMI { List.rev $1 }

/* primitive typenames */
prim_type:
| BOOL   { "bool" }
| NUM    { "num" }
| STRING { "string" }

data_type:
| prim_type { $1 }
| DICT  { "dict" }
| LIST  { "list" }
| NODE  { "node" }
| GRAPH { "graph" }

/* chained primitive declarations can be assigned using "=" */
prim_decl_prefix:
| prim_type ID { [$2] }
| prim_type ID ASSIGN expr { [$2] } /* assignment and declaration */
| prim_decl_prefix COMMA ID ASSIGN expr { $3 :: $1 }
| prim_decl_prefix COMMA ID { $3 :: $1 }

/* chained node declarations can be assigned using "(value)" */
node_decl_prefix:
| NODE ID { [$2] }
| NODE ID LPAREN expr RPAREN { [$2] }
| node_decl_prefix COMMA ID LPAREN expr RPAREN { $3 :: $1 }
| node_decl_prefix COMMA ID { $3 :: $1 }

/* chained graph declarations can be assigned using "{ edge op list }" */
graph_decl_prefix:
| GRAPH ID { [$2] }
| GRAPH ID ASSIGN LBRACE edge_op_list RBRACE { [$2] }
| graph_decl_prefix COMMA ID { $3 :: $1 }

list_decl_prefix:
| LIST LT data_type GT ID { [$3] }
| LIST LT data_type GT ID ASSIGN LBRACKET formal_list RBRACKET { [$3] }
| list_decl_prefix COMMA LT data_type GT ID { $4 :: $1 }
| list_decl_prefix COMMA LT data_type GT ID ASSIGN LBRACKET formal_list RBRACKET { $4 :: $1 }

dict_decl_prefix:
| DICT LT data_type COMMA data_type GT ID { [$7] }
| DICT LT data_type COMMA data_type GT ID ASSIGN LBRACE dict_formal_list RBRACE { [$7] }
| dict_decl_prefix COMMA DICT LT data_type COMMA data_type GT ID { $9 :: $1 }

/* comma separated list of operations on nodes
 * for use with graph declarations 
 */
edge_op_list:
| ID { [$1] }
| edge_op { [$1] }
| edge_op_list COMMA edge_op { $3 :: $1 }

edge_op:
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
  | edge_op SEMI { Edgeop($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN ID IN ID RPAREN LBRACE vdecl_list stmt_list RBRACE
     { For($3, $5, $7) }
  | WHILE LPAREN expr RPAREN LBRACE vdecl_list stmt_list RBRACE { While($3, $5) }

/*
expr_opt:
    * nothing * { Noexpr }
  | expr          { $1 }
*/

expr:
  | LITERAL          { Literal($1) }
  | INF              { Literal("INF")}
  | TRUE             { Boolean(True) }
  | FALSE            { Boolean(False) }
  | ID LBRACKET expr RBRACKET { Access($1, $3) }
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
  | ID DOT ID %prec NOCALL { MemberVar($1, $3) }
  | ID DOT ID LPAREN actuals_opt RPAREN { MemberCall($1, $3, $5) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
