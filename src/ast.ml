type op = | Add | Sub | Mult | Div 
          | Equal | Neq | Less | Leq | Greater | Geq 
          | LogAnd (* && *)
          | LogOr (* || *)

type bool = True | False

type fun_dt = 
  | Basic of (string)
  | List of (string)
  | Dict of (string * string)

type expr =
    NumLiteral of string
  | StrLiteral of string
  | ListLiteral of expr list (* ex. [1, 3, 42.33] *)
  | DictLiteral of (expr * expr) list (* ex. [(key, value)] *) 
  | Boolean of bool
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list 
  | Access of expr * expr (* for dict and list element access, node.in[node2] *)
  | MemberCall of expr * string * expr list (* expr that evaluates to parent variable, accessed funct, parameters *)
  | Undir of string * string (* id, id *)
  | Dir of string * string (* id, id *)
  | UndirVal of string * string * expr (* id, id, weight *)
  | DirVal of string * string * expr (* id, id, weight *)
  | BidirVal of expr * string * string * expr (* weight, id, id, weight *)
  | NoOp of string
  | Noexpr


type stmt =
    Block of stmt list
  | Expr of expr
  | Vdecl of string * string (* (type, id) *)
  | ListDecl of  string * string (* elem_type, id *)
  | DictDecl of string * string * string (* key_type, elem_type, id *)
  | Assign of expr * expr
  | AccessAssign of expr * expr * expr (* a[5] = 10 where first expr is an access expr *)
  | NodeDef of string * expr (* (node id, what goes inside parens) of item *)
(*   | AssignList of string * expr  *)(* when a list of expressions is assigned to a variable *)
  | GraphDef of string * expr list (* id EdgeOp list - in form of undir dir - *)
  | Return of expr
  | If of expr * stmt * stmt
  | For of string * expr * stmt list (* temp var, iterable var, var decls, stmts *)
  | While of expr * stmt list (* condition, var decls, stmt list *)
  | Fdecl of func_decl and

   func_decl = {
    rtype : fun_dt; 
    fname : string;
    formals : (fun_dt * string) list;
    (*locals : string list;*)
    body : stmt list;
  }

type program =  { cmds: stmt list }

(*/////////////////////////////////////////////////////////////////////////////
                              /* PRETTY PRINTER */
///////////////////////////////////////////////////////////////////////////// *)

(* prepends prelist at the head of postlst *)
let rec base_concat postlst = function
  | [] -> postlst
  | hd :: tl -> base_concat (hd :: postlst) tl 

let concat prelst postlst = base_concat postlst (List.rev prelst)

let rec string_of_expr = function
    NumLiteral(l) -> l
  | StrLiteral(l) -> "\"" ^ l ^ "\""
  | ListLiteral(el) -> "[" ^ String.concat "," (List.map string_of_expr el) ^ "]"
  | DictLiteral(el) -> "[" ^ String.concat "," (List.map (fun f -> "(" ^ (string_of_expr (fst f)) ^ " : " ^ (string_of_expr (snd f)) ^ ")" ) el)
  | Boolean(b) -> if b = True then "true" else "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	         Add -> "+"  
          | Sub -> "-" 
          | Mult -> "*" 
          | Div -> "/"
          | Equal -> "==" 
          | Neq -> "!="
          | Less -> "<" 
          | Leq -> "<=" 
          | Greater -> ">" 
          | Geq -> ">="
          | LogAnd -> "&&"
          | LogOr -> "||"
        ) ^ " " ^

      string_of_expr e2
  | Undir (s1, s2) -> s1 ^ " -- " ^ s2  
  | Dir (s1, s2) -> s1 ^ " --> " ^ s2
  | UndirVal (s1, s2, w) -> s1 ^ " --[" ^ string_of_expr w ^ "] " ^ s2 
  | DirVal (s1, s2, w) -> s1 ^ " -->[" ^ string_of_expr w ^ "] " ^ s2
  | BidirVal (w1, s1, s2, w2) -> s1 ^ " [" ^ string_of_expr w1 ^ "]--[" ^ string_of_expr w2 ^ "] " ^ s2 
  | NoOp (s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Access (e, e1) -> string_of_expr e ^ "[" ^ string_of_expr e1 ^ "]"
  | MemberCall (e1, s2, el) -> string_of_expr e1 ^ "." ^ s2 ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec f_type_to_string = function 
  | Basic(t) -> t
  | List(t) -> "list <" ^ t ^ ">"
  | Dict(kt,vt) -> "dict <" ^ kt ^ "," ^ vt ^ ">"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Vdecl(dt, id) -> dt ^ " " ^ id ^ ";\n";
  | ListDecl(dt, id) -> "list <" ^ dt ^ "> " ^ id ^ ";\n"
  | DictDecl(kdt, vdt, id) -> "dict <" ^ kdt ^ ", " ^ vdt ^ "> " ^ id ^ ";\n"
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e ^ ";"
  | AccessAssign(e1, e2, e3) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "] = " ^ string_of_expr e3 ^ ";\n"
  | NodeDef(v, e) -> v ^ "(" ^ string_of_expr e ^ ")" (* (node id, what goes inside parens) of item *)
  | GraphDef(v, el) -> v ^ " = { " ^ String.concat "," (List.map string_of_expr el) ^ "};"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, sl) ->
      "for (" ^ e1  ^ " in " ^ string_of_expr e2 
      ^ ") { " ^ String.concat "\n" (List.map string_of_stmt sl) ^ " }"
  | While(e, sl) -> "while (" ^ string_of_expr e ^ ") {" ^ String.concat "\n" (List.map string_of_stmt sl) ^ " }"
  | Fdecl(f) ->  string_of_fdecl f and 

  string_of_fdecl fdecl =
  "def " ^ (f_type_to_string fdecl.rtype) ^ " " ^ fdecl.fname ^ "(" ^ 
    (String.concat ", " (List.map (fun f ->(f_type_to_string (fst f)) ^ " " ^ snd f) fdecl.formals)) ^
     ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_vdecl id = "type " ^ id ^ ";\n"


let string_of_program (funcs,  cmds) =
  String.concat "\n" (List.map string_of_fdecl funcs) ^
  String.concat "\n" (List.map string_of_stmt cmds)
