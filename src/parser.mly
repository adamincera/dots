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
 | /* nothing */ { { cmds = [] } }
 | decls stmt  { { cmds = $2 :: $1.cmds } }


/////////////////////////////////////////////////////////////////////////////
                    /* FUNCTIONS */
/////////////////////////////////////////////////////////////////////////////

/* (1)def (2)func (3)<funcName> ( (5)arg1,...argN ) { (8) <local variables> (9) <body> } */
fdecl:
   DEF f_data_type ID LPAREN formals_opt RPAREN LBRACE non_func_stmt_list RBRACE
     { Fdecl({ 
              rtype = $2;
              fname = $3;
              formals = $5;
              body = List.rev $8 
    }) }

/* Optional Formal Args */
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    f_data_type ID                   { [($1, $2)] }
  | formal_list COMMA f_data_type ID { ($3, $4) :: $1 }

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
/* bug with bidirectional weighted edges: */
/*|  ID LBRACKET expr RBRACKET UEDGE LBRACKET expr RBRACKET ID */   /*   x [3]--[5] y   */
   /*{ BidirVal($3, $1, $9, $7) }                                 */

/////////////////////////////////////////////////////////////////////////////
                          /* VARIABLES */
/////////////////////////////////////////////////////////////////////////////

/* Literals */
literal:
| NUM_LIT { NumLiteral($1) }
| STR_LIT { StrLiteral($1) }
| list_literal {$1}
| dict_literal {$1}

list_literal:
| LBRACKET actuals_opt RBRACKET { ListLiteral($2) }

dict_literal:
| LBRACE tuples_opt RBRACE { DictLiteral($2)}


/* Primitive Typenames */
prim_type:
| BOOL   { "bool" }
| NUM    { "num" }
| STRING { "string" }

data_type:
| prim_type { $1 }
| DICT LT data_type COMMA data_type GT  { "dict" }
| LIST LT data_type GT  { "list" }
| NODE  { "node" }
| GRAPH { "graph" }

f_data_type:
| prim_type { Basic($1) }
| DICT LT data_type COMMA data_type GT  { Dict($3,$5) }
| LIST LT data_type GT  { List($3) }
| NODE { Basic("node") }
| GRAPH { Basic("graph") } 

vdecl:
|  prim_decl_prefix SEMI { $1 }
|  node_decl_prefix SEMI { $1 }
|  graph_decl_prefix SEMI { $1 }
|  list_decl_prefix SEMI { $1 }
|  dict_decl_prefix SEMI { $1 }

/* PRIMITIVE INITIALIZERS */
prim_decl_prefix:
| prim_type ID { Vdecl($1, $2) }                                                   /* num x */
| prim_type ID ASSIGN expr { Block([Vdecl($1, $2); Assign(Id($2), $4)]) }        /* MOVE THESE  */

/* NODE INITIALIZERS */
node_decl_prefix:
| NODE ID { Block[Vdecl("node", $2); NodeDef($2, Noexpr)] }                                                        /* node x;    */
| NODE ID LPAREN expr RPAREN { Block([Vdecl("node", $2); NodeDef($2, $4)]) }      /* node x("chicago") */                           /* node x("Chicago") */

/* GRAPH INITIALIZERS */
graph_decl_prefix:
| GRAPH ID { Vdecl("graph", $2) }                                                       /*  graph g;    */
/*| GRAPH ID ASSIGN LBRACE edge_op_list RBRACE { Block([Vdecl("graph", $2); AssignList($2, $5)]) } */                    /*  graph g = { x --[5] y; y -->[3] z; }  */
| GRAPH ID ASSIGN LBRACE edge_op_list RBRACE { Block([Vdecl("graph", $2); GraphDef($2,$5)]) }    /*  graph g = { x --[5] y; y -->[3] z; }  */


list_decl_prefix:
| LIST LT data_type GT ID { ListDecl($3, $5) }                                                              /*  list<node> min; */ 
| LIST LT data_type GT ID ASSIGN expr { Block([ListDecl($3, $5); Assign(Id($5), $7)]) }                        /*  list<node> min_path = { x, y, z; }; */

dict_decl_prefix:
| DICT LT data_type COMMA data_type GT ID { DictDecl($3, $5, $7) }                                         /* dict<node, num> parents; */ 
| DICT LT data_type COMMA data_type GT ID ASSIGN expr { Block([DictDecl($3, $5, $7); Assign(Id($7), $9)]) } /* dict<node, num> parents = { x; y; z; }; */

   
/////////////////////////////////////////////////////////////////////////////
                                  /* STATEMENTS */
/////////////////////////////////////////////////////////////////////////////

/* statements are defined inside functions or executed like a script */
/* a statement is just an action. ex. x = 5; */

stmt:
  | non_func_stmt {$1}
  | func_stmt {$1}

non_func_stmt: 
  | expr SEMI { Expr($1) } 
  | log_expr SEMI { Expr($1) }
  | edge_op SEMI { Expr($1) }
  | ID ASSIGN expr SEMI { Assign(Id($1), $3) }
  /*| access_expr ASSIGN expr SEMI { AccessAssign($1, $3) } */
  | expr LBRACKET expr RBRACKET ASSIGN expr SEMI { AccessAssign($1, $3, $6) } 
  | RETURN expr SEMI { Return($2) } 
  /* | LBRACE stmt_list RBRACE { Block(List.rev $2) } */
  | IF LPAREN log_expr RPAREN LBRACE stmt_list RBRACE %prec NOELSE { If($3, Block($6), Block([])) }
  | IF LPAREN log_expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE   { If($3, Block($6), Block($10)) }
  | FOR LPAREN ID IN expr RPAREN LBRACE stmt_list RBRACE
     { For($3, $5, $8) }
  | WHILE LPAREN log_expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }
  | vdecl { $1 }

func_stmt:
    | fdecl {$1}

/* list of statements */
stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

non_func_stmt_list:
  /* nothing */  { [] }
  | non_func_stmt_list non_func_stmt { $2 :: $1 }

/////////////////////////////////////////////////////////////////////////////
                          /* EXPRESSIONS */
/////////////////////////////////////////////////////////////////////////////


log_expr: 
  | expr EQ  expr { Binop($1, Equal, $3) }
  | expr NEQ  expr { Binop($1, Neq,   $3) }
  | expr LT    expr { Binop($1, Less,  $3) }
  | expr LEQ   expr { Binop($1, Leq,   $3) }
  | expr GT  expr { Binop($1, Greater,  $3) }
  | expr GEQ  expr { Binop($1, Geq,   $3) }
  | log_expr LOGAND log_expr { Binop($1, LogAnd, $3) }
  | log_expr LOGOR log_expr { Binop($1, LogOr, $3) }

expr:
  | access_expr { $1 }
  | nacc_expr { $1 }

nacc_expr: /* non access exprs */  
  | expr DOT ID LPAREN actuals_opt RPAREN { MemberCall($1, $3, $5) }
  | LPAREN expr RPAREN { $2 }
  | term               { $1 }

access_expr:
  | expr LBRACKET expr RBRACKET { Access($1, $3) }

term : 
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | term PLUS   term { Binop($1, Add,   $3) }
  | term MINUS  term { Binop($1, Sub,   $3) }
  | term TIMES  term { Binop($1, Mult,  $3) }
  | term DIVIDE term { Binop($1, Div,   $3) }
  | atom             { $1 }

atom:
  literal          { $1 }
  | INF              { NumLiteral("INF") }
  | TRUE             { Boolean(True) }
  | FALSE            { Boolean(False) }
  | ID               { Id($1) }


/////////////////////////////////////////////////////////////////////////////
                              /* actuals */
/////////////////////////////////////////////////////////////////////////////
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

tuples_opt:
 /* nothing*/  {[]} 
 | tuples_list {List.rev $1}

/* for dictionary assignment */
tuples_list:
    expr COLON expr { [($1, $3)]  }
  | tuples_list COMMA expr COLON expr { ($3, $5) :: $1 }

/* arguments to a function */
actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 } 
