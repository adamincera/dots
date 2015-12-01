open Ast
open Sast

module StringMap = Map.Make(String)

type c_func = { crtype : string;
                cfname : string;
                cformals : (string * string) list;
                cbody : string list; }

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
| Sast.List(dtv) -> ""
| Sast.Dict(dtk, dtv) -> "TODO"
| x -> raise (Failure ("invalid type in var declaration"))

let string_of_fname = function
| 1 -> "printf"
| x -> "f" ^ string_of_int(x)

let get_type_fmt = function
| "num" -> "%f"
| "string" -> "%s"
| "boolean" -> "%d"
| x -> raise (Failure ("unknown type: " ^ x))

(*
let translate_boolop e1 e2 expr_fmt env = function
| Add
| x -> 
*)
