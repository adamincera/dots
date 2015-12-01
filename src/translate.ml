open Ast
open Sast

module StringMap = Map.Make(String)

type cop = Add | Sub | Mult | Div | Equal | Neq | Less | Leq
           | Greater | Geq | LogAnd | LogOr

type c_func = { crtype : string;
                cfname : string;
                cformals : (string * string) list;
                cbody : string list; 
              }

type ctype = | Float | Int | Cstring | Array of ctype

type cexpr = 
| Literal of ctype * string
| Id of int
| Binop of cexpr * cop * cexpr
| Assign of int * cexpr
| Call of string * cexpr list
| Access of int * cexpr (* array access: id[cexpr] *)
| Noexpr

type cstmt =
| Block of cstmt list
| Expr of cexpr
| Vdecl of int * ctype (* id, type *)
| Return of cexpr
| If of cexpr * cstmt * cstmt
| For of cexpr * cexpr * cexpr * cstmt
| While of cexpr * cstmt

type cprogram = {
                    libs : string list; (* names of libraries for include statements *)
                    globals : cstmt list; (* global variables *)
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

let string_of_fname = function
| 1 -> "printf"
| x -> "f" ^ string_of_int(x)

let get_type_fmt = function
| "num" -> "%f"
| "string" -> "%s"
| "boolean" -> "%d"
| x -> raise (Failure ("unknown type: " ^ x))
