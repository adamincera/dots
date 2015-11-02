type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
type bool = True | False

type expr =
    Literal of int
  | bool
  | LogAnd of expr * expr (* for use with && symbol *)
  | LogOr of expr * expr (* for use with || symbol *)
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Access of string * expr (* for dict and list element access *)
  | MemberVar of string * string (* parent variable, the accessed member *)
  | MemberCall of string * string * expr list (* parent variable, accessed funct, parameters *)
  | Noexpr

(* b/c nums can be either float or int just treat them as strings *)
type edge_expr =
| Undir of string * string (* id, id *)
| UndirVal of string * string * string (* id, id, weight *)
| DirVal of string * string * string (* id, id, weight *)
| BidirVal of string * string * string * string (* weight, id, id, weight *)
| NoOp of string

type stmt =
    Block of stmt list
  | Expr of expr
  | Edgeop of edge_expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * string list * stmt list (* temp var, iterable var, var decls, stmts *)
  | While of expr * string list * stmt list (* condition, var decls, stmt list *)

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

(* program: ist of vars, function defs, commands not within a function *)
type program = {Vars : string list; Funcs : func_decl list;
                Cmds : stmt list}

(* type program = string list * func_decl list *)

(* prepends prelist at the head of postlst *)
let rec base_concat postlst = function
  | [] -> postlst
  | hd :: tl -> base_concat (hd :: postlst) tl 
in
let concat prelst postlst = base_concat postlst (List.rev prelst)

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, s) ->
      "for (" ^ string_of_expr e1  ^ " in " ^ string_of_expr e2 
      ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "type " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^
  String.concat "\n" (List.map string_of_stmt cmds)