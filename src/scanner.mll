{ open Parser }

let num = ['0'-'9']+
let num_regex = '-'?(num*'.'num+) | (num+('.'num*)?)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { multicomment lexbuf }      (* Multi-Line Comments *)
| "#"      { comment lexbuf }           (* Single-Line Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '.'      { DOT }
| "&&"     { LOGAND }
| "||"     { LOGOR }
| "--"     { UEDGE }
| "-->"    { REDGE }
| "=="     { EQ }
| "!="     { NEQ }
| "<"      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "def"    { DEF }
| "if"     { IF }
| "else"   { ELSE }
| "true"   { TRUE }
| "INF"    { INF }
| "false"  { FALSE }
| "in"     { IN }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "bool"   { BOOL }
| "num"    { NUM }
| "string" { STRING }
| "node"   { NODE }
| "graph"  { GRAPH }
| "list"   { LIST }
| "dict"   { DICT }
<<<<<<< HEAD
| ['0'-'9']+ as lxm { NUM_LIT(lxm) } (* num literal *)
=======
| num_regex as lxm { NUM_LIT(lxm) } (* num literal *)
>>>>>>> 82761b54a30454e7502440490f92a0a034a37e24
| '"' ([^'"']* as lxm) '"' { STR_LIT(lxm) }               (* string literals *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multicomment = parse
  "*/" { token lexbuf }
| _    { multicomment lexbuf }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }
