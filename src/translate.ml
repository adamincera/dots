(* c AST is a library that handles c Asts pretty prints a c file *)
module StringMap = Map.Make(String)

(* type cop = Add | Sub | Mult | Div | Equal | Neq | Less | Leq
           | Greater | Geq | LogAnd | LogOr *)

type ctype = | Float | Int | Long | Cstring 
             | Array of ctype 
             | List of ctype
             | Graph
             | Node
             | Ptr of ctype (* pointer to a data type *)
             | Void
             | Entry

type cstmt =
| Literal of ctype * string
(* | ListLiteral of ctype * cstmt list     *)         (* [2.5, 3, x] *)
| DictLiteral of ctype * (cstmt * cstmt) list
| Id of ctype * string                           (* ids arget_cexpr_typee ints ex. Id(2) -> v2 *)
| Binop of ctype * cstmt * Ast.op * cstmt
| Assign of cstmt * cstmt                        (* ex. Assign(2, 5) -> v2 = 5 *)
| Call of ctype * string * cstmt list            (* return type of the function, function name, arguments *) (* Call(3, [Literal(5), Id(3)]) -> f3(5, v3) *)
| Access of ctype * cstmt * cstmt               (* array access: id[cexpr] *)
| Member of ctype * cstmt * string              (* id, member *)
| Cast of ctype * cstmt                          (* ex. Cast(Int, Id(f1)) -> (int)(f1) *)
| Deref of ctype * cstmt                                 (* ex. *var *)
| Ref of ctype * cstmt                                   (* ex. &var *)
| Block of cstmt list
| Expr of cstmt
| Vdecl of  ctype * string                       (* (type, id) ex. Vdecl(Int, 2) -> int v2; *)
| Return of cstmt
| If of cstmt * cstmt list * cstmt list
| For of cstmt * cstmt * cstmt * cstmt list      (* assign, condition, incr, body -> ex. for (v1 = 3, v1 < 10; v1 = v1 + 1 *)
| While of cstmt * cstmt list
| Assoc of cstmt (* wrap the expression in parentheses *)
| Nostmt

type c_func = { crtype : ctype; (* c return type *)
                cfname : string; (* function name *)
                cformals : (ctype * string) list; (* (data type, id) list *)
                cbody : cstmt list; (* yo this should be cstmt *)
              }

type cprogram = {
                    globals : cstmt list; (* global variables -- Note: should ONLY be Vdecl list *)
                    cfuncs : c_func list; 
                    (*main : cstmt list*) (* names of libraries for include statements *)
                    (*libs : string list;*) (* names of libraries for include statements *)
                }

let rec type_to_str = function
| Float -> "float"
| Int -> "int"
| Long -> "long"
| Cstring -> "char *"
| Array(dt) -> type_to_str dt ^ "[]"
| List(dt) -> "list_t *"
| Node -> "node_t *"
| Graph -> "graph_t *"
| Ptr(dt) -> type_to_str dt ^ "*"
| Void -> "void"
| Entry -> "entry_t"

let fmt_str = function 
| Float -> "%f"
| Int -> "%d"
| Cstring -> "%s"
| _ -> raise (Failure ("can't print other types directly"))

let rec get_cexpr_type = function
| Literal(dt, str) -> dt
(* | ListLiteral(dt, el) -> dt *)
| DictLiteral(dt, tl) -> dt
| Id(dt, id) -> dt
| Binop(dt, e1, op, e2) -> dt
| Assign(id, e1) -> Void
| Call(dt, id, el) -> dt
| Access(dt, id, e) -> dt
| Member(dt, stmt, m) -> dt
| Cast(dt, e) -> dt
| Ref(dt, e) -> dt
| Deref(dt, e) -> dt
| Assoc(e) -> get_cexpr_type e
| _ -> Void
(* | Noexpr -> Void *)

let rec stmt_type_to_str = function
| Literal(dt, str) -> "Literal<" ^ type_to_str dt ^ ">"
(* | ListLiteral(dt, el) -> "ListLiteral<" ^ type_to_str dt ^ ">" *)
| DictLiteral(dt, tl) -> "DictLiteral<" ^ type_to_str dt ^ ">"
| Id(dt, id) -> "Id<" ^ type_to_str dt ^ ">"
| Binop(dt, e1, op, e2) -> "Binop<" ^ type_to_str dt ^ ">"
| Assign(id, e1) -> "Assign<" ^ stmt_type_to_str id ^ ">"
| Call(dt, id, el) -> "Call<" ^ type_to_str dt ^ ">"
| Access(dt, id, e) -> "Access<" ^ type_to_str dt ^ ">"
| Member(dt, stmt, m) -> "Member<" ^ type_to_str dt ^ ">"
| Cast(dt, e) -> "Cast<" ^ type_to_str dt ^ ">"
| Ref(dt, e) -> "Ref<" ^ type_to_str dt ^ ">"
| Deref(dt, e) -> "Deref<" ^ type_to_str dt ^ ">"
| Block(sl) -> "Block"
| Expr(e) -> "Expr:" ^ stmt_type_to_str e
| Vdecl(dt, id) -> "Vdecl<" ^ type_to_str dt ^ ">"
| Return(e) -> "Return:" ^ stmt_type_to_str e
| If(cond, el1, el2) -> "If-then-Else"
| For(assign, cond, incr, sl) -> "For"
| While(cond, sl) -> "While"
| Assoc(e) -> stmt_type_to_str e
| Nostmt -> "Nostmt"

let op_to_str = function
| Ast.Add -> "+"
| Ast.Sub -> "-"
| Ast.Mult -> "*"
| Ast.Div -> "/"
| Ast.Equal -> "=="
| Ast.Neq -> "!="
| Ast.Less -> "<"
| Ast.Leq -> "<="
| Ast.Greater -> ">"
| Ast.Geq -> ">="
| Ast.LogAnd -> "&&"
| Ast.LogOr -> "||"

(* takes a c datatype and returns the print format string *)
let get_fmt_str = function
| Float -> "%.3f"
| Int -> "%d"
| Long -> "%l"
| Cstring -> "%s"
| Node -> raise (Failure "type node can't be directly printed")
| Entry | Graph -> raise (Failure "type requires iterable print handling")
| List(dt) | Array(dt) -> raise (Failure "type requires iterable print handling")
| Void -> raise (Failure ("can't directly print Void")) 
| Ptr(dt) -> raise (Failure "can't directly print pointer")

let cvar_cnt = ref 0

(*

     let get_fmt_val expr = 
                let expr_type = get_cexpr_type expr in
                (match expr_type with
                  | Float -> [("%.3f", translate_stmt expr)]
                  | Int -> [("%d", translate_stmt expr)]
                  | Long -> [("%l", translate_stmt expr)]
                  | Cstring -> [("%s", translate_stmt expr)]
                  | Node -> 
                      let addr = translate_stmt(Cast(Long, expr)) in
                      let expr_val = translate_stmt(Member(Cstring, translate_stmt expr, "data")) in
                      [("%s", "\"N-\""); ("%x", addr); ("%s", "\"(\""); ("%s", expr_val); ("%s", "\")\"")]
                  | Entry | Graph -> raise (Failure "type requires iterable print handling")
                  | List(dt) | Array(dt) -> raise (Failure "type requires iterable print handling")
                  | Void -> raise (Failure ("can't directly print Void (expr type: " ^ (stmt_type_to_str expr)))
                  | Ptr(dt) -> raise (Failure "can't directly print pointer")
                )
            in


let rec translate_expr = function
| Literal(dt, v) ->
   (match dt with
    | Float -> v
    | Int -> v
    | Cstring -> "\"" ^ v ^ "\""
    | Array(adt) -> v
    | Void -> if v = "NULL" then v else raise (Failure "Void lit should only be 'NULL'")
    | _ -> raise (Failure "invalid C literal type")
   )

| Id(dt, id) -> id
| Binop(dt, e1, op, e2) -> translate_expr e1 ^ " " ^ op_to_str(op) ^ " " ^ translate_expr e2
| Assign(target, e) -> (translate_expr target) ^ " = " ^  translate_expr e
| Call(dt, id, el) -> 
    (match id with
        | "f1" -> 
            (* fmt is all the format types so far: ex. %s%f%f *)
            (* vals is what will be put into the format vals: ex. "foo", 8.3, 8,3 *)
            (* takes expr to translate, data type of expr *)
            let get_fmt_val expr = 
                let expr_type = get_cexpr_type expr in
                (match expr_type with
                  | Float -> [("%.3f", translate_expr expr)]
                  | Int -> [("%d", translate_expr expr)]
                  | Long -> [("%l", translate_expr expr)]
                  | Cstring -> [("%s", translate_expr expr)]
                  | Node -> 
                      let addr = translate_expr(Cast(Long, expr)) in
                      let expr_val = translate_expr(Member(Cstring, translate_expr expr, "data")) in
                      [("%s", "\"N-\""); ("%x", addr); ("%s", "\"(\""); ("%s", expr_val); ("%s", "\")\"")]
                  | List | Entry | Graph -> raise (Failure "type requires iterable print handling")
                  | Array(dt) -> raise (Failure "type requires iterable print handling")
                  | Void -> raise (Failure "can't directly print Void")
                  | Ptr(dt) -> raise (Failure "can't directly print pointer")
                )
            in

            let rec build_str fmt_val_list = function
            | [] -> fmt_val_list
            | hd :: tl -> build_str ((get_fmt_val hd) @ fmt_val_list) tl
            in

            let fmts = build_str [] el
            in

            "printf(" ^  
            "\"" ^ String.concat "" (List.map (fun t -> fst t) fmts) ^ "\", " ^ (* format string *)
            String.concat ", " (List.map (fun t -> snd t) fmts) ^ (* comma separated inputs to fmt string *)
            ")"
        | _ -> id ^ "(" ^ (String.concat ", " (List.map translate_expr el)) ^ ")"
    )
| Access(dt, id, e) -> id ^ "[" ^ translate_expr e ^ "]"
| Member(dt, id, m) -> id ^ "->" ^ m
| Cast(dt, e) -> "(" ^ type_to_str dt ^ ")(" ^ translate_expr e ^ ")"
| Ref(e) -> "&(" ^ translate_expr e ^ ")"
| Deref(e) -> "*(" ^ translate_expr e ^ ")"
| Noexpr -> ""
*)

let rec translate_stmt = function
| Literal(dt, v) ->
   (match dt with
    | Float -> v
    | Int -> v
    | Cstring -> "\"" ^ v ^ "\""
    | Array(adt) -> v
    | Void -> if v = "NULL" then v else raise (Failure "Void lit should only be 'NULL'")
    | _ -> raise (Failure "invalid C literal type")
   )
(* | ListLiteral(dt, el) -> translate_stmt (Literal(Cstring, "TODO: list literal")) *)
| DictLiteral(dt, el) -> translate_stmt (Literal(Cstring, "TODO: dict literal"))
| Id(dt, id) -> id
| Binop(dt, e1, op, e2) -> 
    (* check if either e1 is a string or e2 is a string: 
      different operation: concatenation 
    *)
  translate_stmt e1  ^ " " ^ op_to_str(op) ^ " " ^ translate_stmt e2
| Assign(target, e) -> (translate_stmt target) ^ " = " ^  translate_stmt e
| Call(dt, id, el) -> 
    
    (*
    (match id with
        | "printf" -> 
            (* fmt is all the format types so far: ex. %s%f%f *)
            (* vals is what will be put into the format vals: ex. "foo", 8.3, 8,3 *)
            (* takes expr to translate, data type of expr *)
            let rec build_str fmt_val_list = function
            | [] -> fmt_val_list
            | hd :: tl -> build_str ((get_fmt_str (get_cexpr_type hd), hd) :: fmt_val_list) tl
            in
            let fmts = build_str [] el
            in
            if fmts = [] then ""
            else 

              "printf(" ^  
              "\"" ^ String.concat "" (List.map (fun t -> fst t) fmts) ^ "\", " ^ (* format string *)
              String.concat ", " (List.map (fun t -> translate_stmt (snd t)) fmts) ^ (* comma separated inputs to fmt string *)
              ")"
       | _ -> id ^ "(" ^ (String.concat ", " (List.map translate_stmt el)) ^ ")" 
     )
*)
id ^ "(" ^ (String.concat ", " (List.map translate_stmt el)) ^ ")"

| Access(dt, id, e) -> (translate_stmt id) ^ "[" ^ (translate_stmt e) ^ "]"
(* | Member(dt, id, m) -> id ^ "->" ^ m *)
| Member(dt, id, m) -> (translate_stmt (Assoc(id))) ^ "->" ^ m 
| Cast(dt, e) -> "(" ^ type_to_str dt ^ ")(" ^ translate_stmt e ^ ")"
| Ref(dt, e) -> "&(" ^ translate_stmt e ^ ")"
| Deref(dt, e) -> "*(" ^ translate_stmt e ^ ")"
| Block(sl) -> String.concat "\n" (List.map translate_stmt sl)
| Expr(e) -> translate_stmt e ^ ";"
| Vdecl(dt, id) -> 
    (match dt with
     | Ptr(Ptr(Entry)) -> type_to_str dt ^ " " ^ id ^ " = NULL;"
     | List(vdt) -> type_to_str dt ^ " " ^ id ^ " = NULL;"
     | _ -> type_to_str dt ^ " " ^ id ^ ";"
    )
    
| Return(e) -> "return " ^ translate_stmt e ^ ";"
| If(cond, sl1, sl2) -> "if (" ^ translate_stmt cond ^ ") {\n" ^
    String.concat "\n" (List.map translate_stmt sl1) ^
    "\n} else {\n" ^
    String.concat "\n" (List.map translate_stmt sl2) ^
    "\n}"
| For(init, cond, incr, sl) -> "for (" ^ translate_stmt init ^ "; " ^
    translate_stmt cond ^ "; " ^
    translate_stmt incr ^ ") {\n" ^
    String.concat "\n" (List.map translate_stmt sl) ^
    "\n}"
| While(cond, sl) -> "while (" ^ translate_stmt cond ^ ") {\n" ^
    String.concat "\n" (List.map translate_stmt sl) ^
    "\n}"
| Assoc(e) -> "(" ^ (translate_stmt e) ^ ")"
| Nostmt -> ""


let translate_func func = 
    (type_to_str func.crtype) ^ " " ^ func.cfname ^ " (" ^ 
    String.concat ", " (List.map (fun f -> (type_to_str (fst f)) ^ " " ^ snd f) func.cformals) ^
    ")\n{\n" ^
    String.concat "\n" (List.map translate_stmt func.cbody) ^
    "\n}\n"

(* eventually won't be used by analyzer.ml *)
let string_of_cfunc func = 
    (type_to_str func.crtype) ^ " " ^ func.cfname ^ " (" ^ 
    String.concat ", " (List.map (fun f -> (type_to_str (fst f)) ^ " " ^ snd f) func.cformals) ^
     ")\n{\n" ^
    (String.concat "\n" (List.map translate_stmt func.cbody)) ^
    "\n}\n"

let translate_c (globals, cfuncs) = 
    (* "\"graph.h\"" *)
    let libs = ["<stdio.h>"; "<stdlib.h>"; "<string.h>"; 
                "<dict.h>"]
    in     

    (* now we are going to translate a program *)
    (String.concat "\n" (List.map (fun f -> "#include " ^ f) libs)) ^ 
    "\n" ^
    (String.concat "\n" (List.map translate_stmt globals)) ^ 
    "\n" ^
    (String.concat "\n" (List.map translate_func cfuncs))


   (* List.map print_endline (List.map translate_stmt cfuncs.cbody) *)

