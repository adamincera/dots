open Ast

module StringMap = Map.Make(String)

type dataType = Num | String | Bool | Graph | Node | Dict | List | Void

type s_expr =
    NumLiteral of string  * dataType                (* 5 *)
  | StrLiteral of string  * dataType                (* "Hello" *)
  | Boolean of Ast.bool * dataType                      (* True *)
  | LogAnd of s_expr * s_expr * dataType            (* for use with && symbol *)
  | LogOr of s_expr * s_expr * dataType             (* for use with || symbol *)
  | Id of string * dataType                         (* x *)
  | Binop of s_expr * op * s_expr * dataType        (* x + y *)
  | Assign of string * s_expr * dataType         (* x = 5; *)
  | AssignList of string * s_expr list * dataType   (* when a list of expressions is assigned to a variable *)
  | DictAssign of  expr * s_expr * dataType * dataType     (* key, value *)
  | Call of string * s_expr list * dataType
  | Access of string * s_expr  * dataType          (* for dict and list element access *)
  | MemberVar of string * string   * dataType      (* parent variable, the accessed member *)
  | MemberCall of string * string * s_expr list * dataType (* parent variable, accessed funct, parameters *)
  | Undir of string * string  * dataType           (* id, id *)
  | Dir of string * string  * dataType             (* id, id *)
  | UndirVal of string * string * s_expr * dataType (* id, id, weight *)
  | DirVal of string * string * s_expr * dataType   (* id, id, weight *)
  | BidirVal of s_expr * string * string * s_expr * dataType (* weight, id, id, weight *)
  | NoOp of string * dataType
  | Noexpr

type s_stmt =
    Block of s_stmt list
  | Expr of s_expr
  | Vdecl of dataType * string
  | ListDecl of  dataType * string             (* type <id> *)
  | DictDecl of dataType * dataType * string     (* type <id, id> *)
  | Return of s_expr                          (* return x (dataType) *)
  | If of s_expr * s_stmt * s_stmt             (* if (boolean) stmt; *)
  | For of string * string * s_stmt list       (* temp var, iterable var, var decls, stmts *)
  | While of s_expr * s_stmt list              (* condition, var decls, stmt list *)

type s_fdecl = {
    s_fname : string;
    s_rtype : dataType; 
    (*formals : (string * string) list; *)
    s_formals : (dataType * string) list;
    s_body : s_stmt list;
  }

(* program: ist of vars, function defs, commands not within a function *)
type program = { s_funcs : s_fdecl list;
                s_cmds : s_stmt list }

(* end Sast *)