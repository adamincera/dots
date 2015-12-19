(* this defines semantically typed dots ast *)
module StringMap = Map.Make(String)

type dataType = | Num | String | Bool 
                | Graph | Node 
                | List of dataType (* val type *)
                | Dict of dataType * dataType (* key type, val type *)
                | Void



type s_expr =
    NumLiteral of string  * dataType                         (* 5 *)
  | StrLiteral of string  * dataType                         (* "Hello" *)
  | ListLiteral of s_expr list * dataType                    (* [2.5, 3, x] *)
  | DictLiteral of (s_expr * s_expr) list * dataType         (* [(Hello, 15)] *)
  | Boolean of Ast.bool * dataType                           (* True *)
  | Id of string * dataType                                  (* x *)
  | Binop of s_expr * Ast.op * s_expr * dataType             (* x + y *)
  | Call of string * s_expr list * dataType
  | Access of s_expr * s_expr  * dataType                    (* for dict and list element access *)
  | MemberCall of s_expr * string * s_expr list * dataType   (* parent variable, accessed funct, parameters *)
  | Undir of string * string  * dataType                     (* id, id *)
  | Dir of string * string  * dataType                       (* id, id *)
  | UndirVal of string * string * s_expr * dataType          (* id, id, weight *)
  | DirVal of string * string * s_expr * dataType            (* id, id, weight *)
  | BidirVal of s_expr * string * string * s_expr * dataType (* weight, id, id, weight *)
  | NoOp of string * dataType
  | Noexpr

type s_stmt =
    Block of s_stmt list
  | Expr of s_expr
  | Vdecl of dataType * string
  | NodeDef of string * s_expr * dataType (* (node id, item id, datatype) *)
  | GraphDef of string * s_expr list
  | Assign of s_expr * s_expr * dataType                     (* x = 5; *)
  | AccessAssign of s_expr * s_expr * dataType (* a[5] = 10  where first thing is an access expr *)
  | Return of s_expr * dataType                         (* return x (dataType) *)
  | If of s_expr * s_stmt * s_stmt             (* if (boolean) stmt; *)
  | For of string * s_expr * s_stmt list       (* temp var, iterable var, var decls, stmts *)
  | While of s_expr * s_stmt list              (* condition, var decls, stmt list *)
  | Fdecl of s_fdecl and 

 s_fdecl = {
    s_fname : string;
    s_rtype : dataType; 
    (*formals : (string * string) list; *)
    s_formals : (dataType * string) list;
    s_body : s_stmt list;
  }

(* program: ist of vars, function defs, commands not within a function
s_funcs : s_fdecl list;
   (* | AssignList of string * s_expr list           when a list of expressions is assigned to a variable 
  | DictAssign of string * (s_expr * s_expr) list                (variable name, list of tuples of key value pairs) *)
                s_cmds : s_stmt list
 *)
type program = { s_cmds : s_stmt list } 

let rec dt_to_str = function
| Num -> "num"
| String -> "string"
| Bool -> "bool"
| List(dt) -> "list<" ^ (dt_to_str dt) ^ ">"
| Dict(dtk, dtv) -> "dict<" ^ (dt_to_str dtk) ^ ", " ^ (dt_to_str dtv) ^ ">"
| Graph -> "graph"
| Node -> "node"
| Void -> "void"

(* end Sast *)