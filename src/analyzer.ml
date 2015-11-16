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

(* 
   for use with maps where the value is an int 
   finds the max int value in the map
*)
let find_max_index map = 
	let bindings = StringMap.bindings map in
		let rec max cur = function
		| [] -> cur
		| hd :: tl -> if snd hd > cur then max (snd hd) tl else max cur tl
    in
    max 0 bindings


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

    let locals_types = ref StringMap.empty in
    let locals_indexes = ref StringMap.empty in


(*    | MemberCall(p_var, func_name, expr_list) -> (try
    	string_of_locals (StringMap.find p_var locals_indexes)  
    	with Not_found -> raise (Failure ("undefined variable " ^ s))
      )
          | LogAnd(e1, e2) -> (List.map translate_expr e1) ^ "&&" ^ (List.map translate_expr e2)
    | LogOr(e1, e2) -> (List.map translate_expr e1) ^ "||" ^ (List.map translate_expr e1)
        | Id(v) -> string_of_locals (StringMap.find v locals_indexes)

*)
    let rec translate_expr = function 
    | NumLiteral(l) -> l 
    | StrLiteral(l) -> "\"" ^ l ^ "\""
    | Boolean(b) -> if b = True then "true" else "false"
    | Id(v) -> 
      (try
           "l" ^ string_of_int(StringMap.find v !locals_indexes)
       with
       | Not_found -> raise (Failure("undeclared variable: " ^ v))
      )
    | Call(func_name, el) -> match func_name with
        | "print" ->
            let rec build_str fmt vals = function
		    | [] -> (fmt, vals)
		    | hd :: tl -> match hd with
		        | NumLiteral(n) as num -> build_str (fmt ^ "%d") (vals ^ "," ^ (translate_expr num)) tl
		        | StrLiteral(s) as str -> build_str (fmt ^ "%s") (vals ^ "," ^ translate_expr str) tl
		        | Id(v) as id -> (try
		                match StringMap.find v !locals_types with
		                | "num" -> build_str (fmt ^ "%f") (vals ^ "," ^ (translate_expr id)) tl
		                | "string" -> build_str (fmt ^ "%s") (vals ^ "," ^ (translate_expr id)) tl
		            with
		            | Not_found -> raise (Failure ("undefined variable: " ^ v)) )
            in
            let result = build_str "" "" el
            in
            "printf(\"" ^ fst result ^ "\"" ^ snd result ^ ")"

        | fname -> (try
               string_of_int(StringMap.find fname function_indexes) ^ 
               "(" ^ String.concat ", " (List.map translate_expr el) ^ ")"
            with Not_found -> raise (Failure ("undefined function " ^ fname))
           ) 
    in

    let translate_stmt = function 
    | Expr(e) -> translate_expr e ^ ";"
    | Vdecl(t, id) -> 
      (try 
    	StringMap.find id !locals_types; raise (Failure ("variable already declared: " ^ id))
    	with | Not_found -> locals_types := StringMap.add id t !locals_types; (* add type map *)
    						locals_indexes := StringMap.add id ((find_max_index !locals_indexes)+1) !locals_indexes; (* add index mapping *)
    						translate_vdecl ("l" ^ string_of_int(StringMap.find id !locals_indexes)) t   						
             | Failure(f) -> raise (Failure (f) ) )
    in

    let main_func = { crtype = "int";
                  cfname = "main";
                  cformals = [("int", "argc"); ("char**", "argv")];
                  cbody = List.map translate_stmt cmds}
    in

    print_endline ((String.concat "\n" (List.map (fun h -> "#include " ^ h) headers)) ^ "\n" ^
                   string_of_cfunc main_func )
    

(* How to print all the bindings in locals_types: 
print_endline ( "locals: " ^ List.fold_left (fun acc x -> acc ^ x ^ " ") "" (List.map (fun kv -> fst kv ^ ":" ^ snd kv) (StringMap.bindings !locals_types)));
*)


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
  translate (prg.funcs, List.rev prg.cmds)
  (* print_endline (String.concat "\n" (List.map string_of_stmt (List.rev prg.cmds))) *)
  

(* pretty printing version *)
(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = string_of_program (prg.funcs, prg.cmds) in
  print_endline result;;
*)