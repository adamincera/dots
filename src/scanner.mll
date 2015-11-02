{ open Parser }

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
| ['0'-'9']+ as lxm { LITERAL(lxm) } (* num literal *)
| "\".*\"" as lxm  { LITERAL(lxm) }                (* string literals *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multicomment = parse
  "*/" { token lexbuf }
| _    { multicomment lexbuf }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }
