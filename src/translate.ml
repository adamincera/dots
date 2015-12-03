module StringMap = Map.Make(String)

type cop = Add | Sub | Mult | Div | Equal | Neq | Less | Leq
           | Greater | Geq | LogAnd | LogOr

type ctype = | Float | Int | Cstring | Array of ctype

type cexpr = 
| Literal of ctype * string
| Id of int (* ids are ints ex. Id(2) -> v2 *)
| Binop of cexpr * cop * cexpr
| Assign of int * cexpr (* ex. Assign(2, 5) -> v2 = 5 *)
| Call of int * cexpr list (* Call(3, [Literal(5), Id(3)]) -> f3(5, v3) *)
| Access of int * cexpr (* array access: id[cexpr] *)
| Cast of ctype * cexpr (* ex. Cast(Int, Id(f1)) -> (int)(f1) *)
| Noexpr

type cstmt =
| Block of cstmt list
| Expr of cexpr
| Vdecl of  ctype * int (* (type, id) ex. Vdecl(Int, 2) -> int v2; *)
| Return of cexpr
| If of cexpr * cstmt list * cstmt list
| For of cexpr * cexpr * cexpr * cstmt list (* For(Assign(1, Literal(3)), Binop(Id(1), Less, Literal(10)), Assign(Id(1), Binop(Id(1), Add, Literal(1), list of stuff) -> for (v1 = 3, v1 < 10; v1 = v1 + 1 *)
| While of cexpr * cstmt list

type c_func = { crtype : string; (* c return type *)
                cfname : string; (* function name *)
                cformals : (string * string) list; (* (data type, id) list *)
                cbody : string list; (* yo this should be cstmt *)
              }

type cprogram = {
                    libs : string list; (* names of libraries for include statements *)
                    globals : cstmt list; (* global variables -- Note: should ONLY be Vdecl list *)
                    cfuncs : c_func list; 
                }

let string_of_cfunc func = 
    func.crtype ^ " " ^ func.cfname ^ " (" ^ 
    String.concat ", " (List.map (fun f -> fst f ^ " " ^ snd f) func.cformals) ^
     ")\n{\n" ^
    String.concat "\n" func.cbody ^
    "\n}\n"

(* 
   creates a variable declaration statement based on the variable's data type
   params --> id : variable name ; 2nd arg : variable type 
*)
let translate_vdecl id = function
| Sast.String -> "char *" ^ id ^ ";"
| Sast.Num -> "float " ^ id ^ ";"
| Sast.List(dtv) -> "list todo"
| Sast.Dict(dtk, dtv) -> "dict todo"
| x -> raise (Failure ("invalid type in var declaration"))

let rec type_to_str = function
| Float -> "float"
| Int -> "int"
| Cstring -> "char *"
| Array(dt) -> type_to_str dt ^ "[]"

let op_to_str = function
| Add -> "+"
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

let translate (libs, globals, cfuncs) =
    let rec translate_expr = function
    | Literal(dt, v) ->
       (match dt with
        | Float -> v
        | Int -> v
        | Cstring -> "\"" ^ v ^ "\""
        | Array(adt) -> v
       )

    | Id(id) -> "v" ^ string_of_int(id)
    | Binop(e1, op, e2) -> translate_expr e1 ^ " " ^ op_to_str(op) ^ " " ^ translate_expr e2
    | Assign(id, e) -> "v" ^ string_of_int(id) ^ " = " ^  translate_expr e
    | Call(id, el) -> "f" ^ string_of_int(id) ^ "(" ^ (String.concat "," (List.map translate_expr el)) ^ ")"
    | Access(id, e) -> "v" ^ string_of_int(id) ^ "[" ^ translate_expr e ^ "]"
    | Cast(dt, e) -> "(" ^ type_to_str dt ^ ")(" ^ translate_expr e ^ ")"
    | Noexpr -> ""
    in

    let rec translate_stmt = function
    | Block(sl) -> String.concat "\n" (List.map translate_stmt sl)
    | Expr(e) -> translate_expr e ^ ";"
    | Vdecl(dt, id) -> type_to_str dt ^ " v" ^ string_of_int(id) ^ ";"
    | Return(e) -> "return " ^ translate_expr e ^ ";"
    | If(cond, sl1, sl2) -> "if (" ^ translate_expr cond ^ ") {\n" ^
        String.concat "\n" (List.map translate_stmt sl1) ^
        "} else {\n" ^
        String.concat "\n" (List.map translate_stmt sl2) ^
        "}"
    | For(init, cond, incr, sl) -> "for (" ^ translate_expr init ^ "; " ^
        translate_expr cond ^ "; " ^
        translate_expr incr ^ ") {\n" ^
        String.concat "\n" (List.map translate_stmt sl) ^
        "}"
    | While(cond, sl) -> "while (" ^ translate_expr cond ^ ") {\n" ^
        String.concat "\n" (List.map translate_stmt sl) ^
        "}"
    in

    let translate_func func = 
        func.crtype ^ " " ^ func.cfname ^ " (" ^ 
        String.concat ", " (List.map (fun f -> fst f ^ " " ^ snd f) func.cformals) ^
        ")\n{\n" ^
        String.concat "\n" func.cbody ^
        "\n}\n"
    in
    
    print_endline("foo")
   (* List.map print_endline (List.map translate_expr cfuncs.cbody) *)

