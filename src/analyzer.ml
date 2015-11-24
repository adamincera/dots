open Ast
open Translate

module StringMap = Map.Make(String)

(* "\"graph.h\"" *)
let headers = ["<stdio.h>"; "<stdlib.h>"; "<string.h>"]

type env = {loc_inds : int StringMap.t; 
                loc_types : string StringMap.t;
                func_inds : int StringMap.t}

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

(* the meat of the compiler *)
>>>>>>> 82761b54a30454e7502440490f92a0a034a37e24
let translate (functions, cmds) = 
	let bff = [ "print"; "range";] in

	let num_func = 
		List.fold_left (fun acc x -> acc + 1) 0 bff in
	(* let built_in_functions = StringMap.add "print" (-1) StringMap.empty in *)
	let built_in_functions = string_map_pairs StringMap.empty
      (enum 1 1 (List.map (fun f -> f) bff)) in

    let function_indexes = string_map_pairs built_in_functions
      (enum 1 (1+num_func) (List.map (fun f -> f.fname) functions)) in

<<<<<<< HEAD
    let stmt_list = ["printf(\"hello world\")"] in

    let locals_indexes = StringMap.empty in

=======
    let locals_types = ref StringMap.empty in
    let locals_indexes = ref StringMap.empty in

    (* returns the format string for each type *)
	let rec expr_fmt = function
	| NumLiteral(n) -> "%f"
	| StrLiteral(s) -> "%s"
	| Boolean(b) -> "%d"
	| LogAnd(e1, e2) -> "%d"
	| LogOr(e1, e2) -> "%d"
	| Id(v) -> (try
	                match StringMap.find v !locals_types with
	                | "num" -> "%f"
	                | "string" -> "%s"
	                | x -> raise (Failure ("undefined type: " ^ x))
	            with
	            | Not_found -> raise (Failure ("undefined variable: " ^ v))
	            | Failure(f) -> raise (Failure f) )
	| Binop(e1, op, e2) -> if not(expr_fmt e1 = expr_fmt e2) 
	                       then raise (Failure("operands are not of same type")) 
	                       else expr_fmt e1
	| Assign(v, e) -> "no-fmt"
	| AssignList(v, el) -> "no-fmt"
	| DictAssign(k, v) -> "no-fmt"
	| Call(v, el) -> "TODO - return type of func"
	| Access(v, e) -> "TODO - type of list / dict vals"
	| MemberVar(v, m) -> "TODO - type of member var"
	| MemberCall(v, f, el) -> "TODO - return type of member function"
	| Undir(v1, v2) -> "no-fmt"
	| Dir(v1, v2) -> "no-fmt"
	| UndirVal(v1, v2, w) -> "no-fmt"
	| DirVal(v1, v2, w) -> "no-fmt"
	| BidirVal(w1, v1, v2, w2) -> "no-fmt"
	| NoOp(s) -> "no-fmt"
	| Noexpr -> "no-fmt"
    in
>>>>>>> 82761b54a30454e7502440490f92a0a034a37e24

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
<<<<<<< HEAD
    | Call(func_name, el) -> (try
          string_of_fname (StringMap.find func_name function_indexes) ^ 
          "(" ^ String.concat ", " (List.map translate_expr el) ^ ")"
          with Not_found -> raise (Failure ("undefined function " ^ func_name))
      )
    in

    let translate_stmt = function 
    | Expr(e) -> translate_expr e ^ ";"
    | Vdecl(t, id) -> (try 
    	StringMap.find id locals_indexes; raise (Failure ("variable already declared " ^ id))
    	with Not_found -> StringMap.add id t locals_indexes; "foo")
=======
    | LogAnd(e1, e2) -> "TODO"
    | LogOr(e1, e2) -> "TODO"
    | Id(v) -> 
      (try
           "l" ^ string_of_int(StringMap.find v !locals_indexes)
       with
       | Not_found -> raise (Failure("undeclared variable: " ^ v))
      )
    | Binop(e1, op, e2) -> "TODO"
    | Assign(v, e) ->
        if not(expr_fmt (Id(v)) = expr_fmt e)
        then raise (Failure ("assignment expression not of type: " ^ expr_fmt (Id(v)) ))
        else (translate_expr (Id(v))) ^ " = " ^ (translate_expr e)
    | AssignList(v, el) -> "TODO"
	| DictAssign(k, v) -> "TODO"
    | Call(func_name, el) -> (match func_name with
        | "print" ->
            let rec build_str fmt vals = function
		    | [] -> (fmt, vals)
		    | hd :: tl -> build_str (fmt ^ (expr_fmt hd)) (vals ^ "," ^ (translate_expr hd)) tl
            in
            let result = build_str "" "" el
            in
            "printf(\"" ^ fst result ^ "\"" ^ snd result ^ ")"

        | fname -> (try
               string_of_int(StringMap.find fname function_indexes) ^ 
               "(" ^ String.concat ", " (List.map translate_expr el) ^ ")"
            with Not_found -> raise (Failure ("undefined function " ^ fname))
           ) )
    | Access(v, e) -> "TODO"
	| MemberVar(v, m) -> "TODO"
	| MemberCall(v, f, el) -> "TODO"
	| Undir(v1, v2) -> "TODO"
	| Dir(v1, v2) -> "TODO"
	| UndirVal(v1, v2, w) -> "TODO"
	| DirVal(v1, v2, w) -> "TODO"
	| BidirVal(w1, v1, v2, w2) -> "TODO"
	| NoOp(s) -> "TODO"
	| Noexpr -> "TODO"
    in

    let translate_stmt = function 
    | Block(sl) -> "TODO"
    | Expr(e) -> translate_expr e ^ ";"
    | Vdecl(t, id) -> 
      (try 
    	StringMap.find id !locals_types; raise (Failure ("variable already declared: " ^ id))
    	with | Not_found -> locals_types := StringMap.add id t !locals_types; (* add type map *)
    						locals_indexes := StringMap.add id ((find_max_index !locals_indexes)+1) !locals_indexes; (* add index mapping *)
    						translate_vdecl ("l" ^ string_of_int(StringMap.find id !locals_indexes)) t   						
             | Failure(f) -> raise (Failure (f) ) )
    | ListDecl(t, v) -> "TODO"
    | DictDecl(kt, vt, v) -> "TODO"
    | Return(e) -> "TODO"
    | If (cond, s1, s2) -> "TODO"
    | For (temp, iter, sl) -> "TODO"
    | While (cond, sl) -> "TODO"
>>>>>>> 82761b54a30454e7502440490f92a0a034a37e24
    in

    let main_func = { crtype = "int";
                  cfname = "main";
                  cformals = [("int", "argc"); ("char**", "argv")];
                  cbody = List.map translate_stmt cmds}
    in

    print_endline ((String.concat "\n" (List.map (fun h -> "#include " ^ h) headers)) ^ "\n" ^
<<<<<<< HEAD
                   string_of_cfunc main_func)
    



(*
type func_decl = {
    rtype : string; 
    fname : string;
    formals : (string * string) list;
    (*locals : string list;*)
    body : stmt list;
  }
=======
                   string_of_cfunc main_func )
    

(* How to print all the bindings in locals_types: 
print_endline ( "locals: " ^ List.fold_left (fun acc x -> acc ^ x ^ " ") "" (List.map (fun kv -> fst kv ^ ":" ^ snd kv) (StringMap.bindings !locals_types)));
>>>>>>> 82761b54a30454e7502440490f92a0a034a37e24
*)

(* translate version *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
<<<<<<< HEAD
  translate (prg.funcs, prg.cmds)
=======
  translate (prg.funcs, List.rev prg.cmds)
  (* print_endline (String.concat "\n" (List.map string_of_stmt (List.rev prg.cmds))) *)
  
>>>>>>> 82761b54a30454e7502440490f92a0a034a37e24

(* pretty printing version *)
(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = string_of_program (prg.funcs, prg.cmds) in
  print_endline result;;
*)