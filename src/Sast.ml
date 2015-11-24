open Ast

type s_expr =
    S_NumLiteral of string                 (* 5 *)
  | S_StrLiteral of string                 (* "Hello" *)
  | S_Boolean of bool                      (* True *)
  | S_LogAnd of s_expr * s_expr            (* for use with && symbol *)
  | S_LogOr of s_expr * s_expr             (* for use with || symbol *)
  | S_Id of string * dataType              (* x *)
  | S_Binop of s_expr * op * s_expr        (* x + y *)
  | S_Assign of string * s_expr            (* x = 5; *)
  | S_AssignList of string * s_expr list * dataType   (* when a list of expressions is assigned to a variable *)
  | S_DictAssign of  string * s_expr * dataType      (* key, value *)
  | S_Call of string * s_expr list
  | S_Access of string * s_expr            (* for dict and list element access *)
  | S_MemberVar of string * s_expr         (* parent variable, the accessed member *)
  | S_MemberCall of string * s_expr * s_expr list (* parent variable, accessed funct, parameters *)
  | S_Undir of s_expr * s_expr             (* id, id *)
  | S_Dir of s_expr * s_expr               (* id, id *)
  | S_UndirVal of s_expr * s_expr * string (* id, id, weight *)
  | S_DirVal of s_expr * s_expr * string   (* id, id, weight *)
  | S_BidirVal of string * s_expr * s_expr * string (* weight, id, id, weight *)
  | S_NoOp of string
  | S_Noexpr

type s_stmt =
    Block of s_stmt list
  | S_Expr of s_expr * dataType
  | S_ListDecl of  dataType * string             (* type <id> *)
  | S_DictDecl of dataType * string * string     (* type <id, id> *)
  | S_Return of s_expr * dataType                (* return x (dataType) *)
  | S_If of s_expr * s_stmt * s_stmt             (* if (boolean) stmt; *)
  | S_For of s_expr * s_expr * s_stmt list       (* temp var, iterable var, var decls, stmts *)
  | S_While of s_expr * s_stmt list              (* condition, var decls, stmt list *)

type s_vdecl =
  | S_var_decl of dataType * s_expr                   (* type id *)

type s_fdecl = {
    fname : string;
    rtype : dataType; 
    (*formals : (string * string) list; *)
    formals : s_vdecl list;
    (*locals : string list;*)
    body : s_stmt list;
  }

(* program: ist of vars, function defs, commands not within a function *)
type program = { funcs : s_fdecl list;
                cmds : s_stmt list }