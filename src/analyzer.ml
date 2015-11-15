open Ast
open Translate

module StringMap = Map.Make(String)

(* "\"graph.h\"" *)
let headers = ["<stdio.h>"; "<stdlib.h>"; "<string.h>"]

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs


let translate (functions, cmds) = 
	let bff = [ "print"; "range";] in

	let num_func = 
		List.fold_left (fun acc x -> acc + 1) 0 bff in
	(* let built_in_functions = StringMap.add "print" (-1) StringMap.empty in *)
	let built_in_functions = string_map_pairs StringMap.empty
      (enum 1 1 (List.map (fun f -> f) bff)) in

    let function_indexes = string_map_pairs built_in_functions
      (enum 1 (1+num_func) (List.map (fun f -> f.fname) functions)) in

    let stmt_list = ["printf(\"hello world\")"] in

    let rec translate_expr = function 
    | StrLiteral(l) -> "\"" ^ l ^ "\""
    | Call(func_name, el) -> (try
          string_of_fname (StringMap.find func_name function_indexes) ^ 
          "(" ^ String.concat ", " (List.map translate_expr el) ^ ")"
          with Not_found -> raise (Failure ("undefined function " ^ func_name))
      )
    in

    let translate_stmt = function 
    | Expr(e) -> translate_expr e ^ ";"
    in

    let main_func = { crtype = "int";
                  cfname = "main";
                  cformals = [("int", "argc"); ("char**", "argv")];
                  cbody = List.map translate_stmt cmds}
    in

    print_endline ((String.concat "\n" (List.map (fun h -> "#include " ^ h) headers)) ^ "\n" ^
                   string_of_cfunc main_func)
    



(*
type func_decl = {
    rtype : string; 
    fname : string;
    formals : (string * string) list;
    (*locals : string list;*)
    body : stmt list;
  }
*)

(* translate version *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  translate (prg.funcs, prg.cmds)

(* pretty printing version *)
(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = string_of_program (prg.funcs, prg.cmds) in
  print_endline result;;
*)