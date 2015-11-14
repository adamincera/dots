%{ open Ast %}

/* Punctuation Tokens */
%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT
/* Arithmetic Operation Tokens */
%token PLUS MINUS TIMES DIVIDE
/* Assignment Operator */
%token ASSIGN
/* Comparative Operators  */
%token EQ NEQ LT LEQ GT GEQ
/* Logical Operators */
%token LOGAND LOGOR
/* Node Operators */
%token UEDGE REDGE
/* Function Keyword Tokens */
%token RETURN IF ELSE FOR WHILE DEF IN
/* Punctuation Tokens */
%token BOOL NUM STRING NODE GRAPH LIST DICT
/* Boolean Operations */
%token TRUE FALSE INF

%token <string> NUM_LIT
%token <string> STR_LIT
%token <string> ID
%token EOF

/* Order of Operation */
%nonassoc NOCALL
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left LOGOR
%left LOGAND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program                        /* start symbol */
%type <Ast.program> program           /* return type program object */

%%

                    /* START PROGRAM */

program:
  decls EOF { $1 }

decls:
|  /* nothing */ { {funcs = []; cmds = []} }
/*|  decls vdecl { {vars = $1.vars @ $2; funcs = $1.funcs; cmds = $1.cmds} }*/
|  decls fdecl { {funcs = $2 :: $1.funcs; cmds = $1.cmds} }
|  decls stmt  { {funcs = $1.funcs; cmds = $2 :: $1.cmds} } 

/////////////////////////////////////////////////////////////////////////////
                    /* FUNCTIONS */
/////////////////////////////////////////////////////////////////////////////

/* (1)def (2)func (3)<funcName> ( (5)arg1,...argN ) { (8) <local variables> (9) <body> } */
fdecl:
   DEF ID ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { 
      fname = $3;
      formals = $5;
      body = List.rev $8 
    } }

/* Optional Formal Args */
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

/* Dictionary Formal Arguments */
dict_formal_list:
    ID COLON expr { [DictAssign($1, $3)] }                                      /*   w : 5        */
|   literal COLON expr {  [DictAssign(string_of_expr $1, $3)] }                                /*   "hello" : 5  */
|   dict_formal_list COMMA ID COLON expr { DictAssign($3, $5) :: $1 }           /*   w : 5        */
|   dict_formal_list COMMA literal COLON expr { DictAssign(string_of_expr $3, $5) :: $1 }

/* comma separated list of operations on nodes
 * for use with graph declarations 
 */
 /*   Edge Operations       */
edge_op_list:
| edge_op { [$1] }
| edge_op_list COMMA edge_op { $3 :: $1 }

edge_op:
/* ID { NoOp($1) }*/
|  ID UEDGE ID { Undir($1, $3) }                                /*   x -- y         */
|  ID REDGE ID { Dir($1, $3) }                                  /*   x --> y        */
|  ID REDGE LBRACKET expr RBRACKET ID { DirVal($1, $6, $4) }    /*   x -->[5] y     */
|  ID UEDGE LBRACKET expr RBRACKET ID { UndirVal($1, $6, $4) }  /*   x --[5] y      */
|  ID LBRACKET expr RBRACKET UEDGE LBRACKET expr RBRACKET ID    /*   x [3]--[5] y   */
   { BidirVal($3, $1, $9, $7) }                                 

/////////////////////////////////////////////////////////////////////////////
                          /* VARIABLES */
/////////////////////////////////////////////////////////////////////////////

/* Literals */
literal:
| NUM_LIT { NumLiteral($1) }
| STR_LIT { StrLiteral($1) }

/* Primitive Typenames */
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

vdecl:
|  prim_decl_prefix SEMI { $1 }
|  node_decl_prefix SEMI { $1 }
|  graph_decl_prefix SEMI { $1 }
|  list_decl_prefix SEMI { $1 }
|  dict_decl_prefix SEMI { $1 }

/* PRIMITIVE INITIALIZERS */
prim_decl_prefix:
| prim_type ID { Vdecl($1, $2) }                                                   /* num x */
| prim_type ID ASSIGN expr { Block([Vdecl($1, $2); Expr(Assign($2, $4))]) }                                       /* MOVE THESE  */

/* NODE INITIALIZERS */
node_decl_prefix:
| NODE ID { Vdecl("node", $2) }                                                        /* node x;    */
| NODE ID LPAREN expr RPAREN { Block([Vdecl("node", $2); Expr(Assign($2, $4))]) }                                     /* node x("Chicago") */

/* GRAPH INITIALIZERS */
graph_decl_prefix:
| GRAPH ID { Vdecl("graph", $2) }                                                       /*  graph g;    */
| GRAPH ID ASSIGN LBRACE edge_op_list RBRACE { Block([Vdecl("graph", $2); Expr(AssignList($2, $5))]) }                     /*  graph g = { x --[5] y; y -->[3] z; }  */

list_decl_prefix:
| LIST LT data_type GT ID { Vdecl("list", $3) }                                                              /*  list<node> min; */ 
| LIST LT data_type GT ID ASSIGN LBRACKET actuals_list RBRACKET { Block([ListDecl($3, $5); Expr(AssignList($5, $8))]) }                        /*  list<node> min_path = { x, y, z; }; */

dict_decl_prefix:
| DICT LT data_type COMMA data_type GT ID { DictDecl($3, $5, $7) }                                         /* dict<node, num> parents; */ 
| DICT LT data_type COMMA data_type GT ID ASSIGN LBRACE dict_formal_list RBRACE { Block([DictDecl($3, $5, $7); Expr(AssignList($7, $10))]) }   /* dict<node, num> parents = { x; y; z; }; */

   
/////////////////////////////////////////////////////////////////////////////
                                  /* STATEMENTS */
/////////////////////////////////////////////////////////////////////////////

/* list of statements */
stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/* statements are defined inside functions */
stmt:
   expr SEMI { Expr($1) } 
  | RETURN expr SEMI { Return($2) } 
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN ID IN ID RPAREN LBRACE stmt_list RBRACE
     { For($3, $5, $8) }
  | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }
  | vdecl { $1 }

/////////////////////////////////////////////////////////////////////////////
                          /* EXPRESSIONS */
/////////////////////////////////////////////////////////////////////////////

/*
assign_expr:
  | ID ASSIGN expr   { Assign($1, $3) }
*/

expr:
  | literal          { $1 }
  | INF              { NumLiteral("INF") }
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
  | expr LOGAND expr { LogAnd($1, $3) }
  | expr LOGOR expr { LogOr($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | ID DOT ID %prec NOCALL { MemberVar($1, $3) }
  | ID DOT ID LPAREN actuals_opt RPAREN { MemberCall($1, $3, $5) }
  | LPAREN expr RPAREN { $2 }

/*
expr_opt:
    * nothing * { Noexpr }
  | expr          { $1 }
*/

/////////////////////////////////////////////////////////////////////////////
                              /* actuals */
/////////////////////////////////////////////////////////////////////////////
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
