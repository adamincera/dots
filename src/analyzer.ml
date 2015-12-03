open Ast
open Sast
open Translate

module StringMap = Map.Make(String)

(* "\"graph.h\"" *)
let headers = ["<stdio.h>"; "<stdlib.h>"; "<string.h>"]

type translation_env = {
            var_inds : int StringMap.t ref list; (* var names to indices ex. x -> 1 so that we can just refer to it as v1 *)
            var_types : Sast.dataType StringMap.t ref list; (* maps a var name to its type  ex. x -> num *)
            func_inds : int StringMap.t ref list;
            func_types : Sast.dataType StringMap.t ref list; (* maps a func name to its return type *)
            return_type : Sast.dataType
    }

(* val enum : int -> 'a list -> (int * 'a) list *)
(* returns list of tuples mapping each elem of a list to consecutive 
   numbers starting from n and incrementing n by stride for each elem *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
(* takes list of tuples (value, key) and adds them to the given map *)
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

(* returns the value associated with a given key,
   traversing through the list of maps until it finds the
   first occurrence of the key, or raises an error if none of
   the maps contain that key

   intended for things like: finding the type of a variable
*)
let find_var var map_list =
  let rec finder var = function
  | m :: tl -> 
      (try StringMap.find var !m
       with
       | Not_found -> finder var tl)
  | [] -> raise (Not_found)
  in 
  finder var map_list

  let str_to_type = function
| "num" -> Sast.Num
| "string" -> Sast.String
| "bool" -> Sast.Bool
| "graph" -> Sast.Graph
| "node" -> Sast.Node
| "dict" -> Sast.Dict(Sast.Void, Sast.Void)
| "list" -> Sast.List(Sast.Void)
| "void" -> Sast.Void
| x -> raise (Failure ("unknown type: " ^ x))

(* converts a dataType to a string *)
let rec type_to_str = function
| Sast.Num -> "num"
| Sast.String -> "string"
| Sast.Bool -> "bool"
| Sast.Graph -> "graph"
| Sast.Node -> "node"
| Sast.Dict(dtk, dtv) -> "dict <" ^ type_to_str dtk ^ ", " ^ type_to_str dtv ^ ">"
| Sast.List(dt) -> "list <" ^ type_to_str dt ^ ">"
| Sast.Void -> "void"

(* returns the datatype of an Sast expressions *)
let get_expr_type = function
| Sast.NumLiteral(v, dt) -> dt
| Sast.StrLiteral(v, dt) -> dt
| Sast.Boolean(v, dt) -> dt
| Sast.Id(v, dt) -> dt
| Sast.Binop(e1, op, e2, dt) -> dt
| Sast.Assign(v, e, dt) -> Sast.Void
| Sast.AssignList(s, el, dt) -> Sast.Void
| Sast.DictAssign(k, v, dt) -> Sast.Void
| Sast.Call(v, el, dt) -> dt
| Sast.Access(v, e, dt) -> dt
| Sast.MemberVar(v, m, dt) -> dt
| Sast.MemberCall(v, m, el, dt) -> dt
| Sast.Undir(v1, v2, dt) -> Sast.Void
| Sast.Dir(v1, v2, dt) -> Sast.Void
| Sast.UndirVal(v1, v2, w, dt) -> Sast.Void
| Sast.DirVal(v1, v2, w, dt) -> Sast.Void
| Sast.BidirVal(w1, v1, v2, w2, dt) -> Sast.Void
| Sast.NoOp(v, dt) -> Sast.Void
| Sast.Noexpr -> Sast.Void

(******************************)
(* CONVERTS AN AST TO AN SAST *)
(******************************)
let convert_ast prog env =
(* convert an Ast.expr object to Sast.expr object *)
let rec expr env = function
| Ast.NumLiteral(v) -> Sast.NumLiteral(v, Sast.Num)
| Ast.StrLiteral(v) -> Sast.StrLiteral(v, Sast.String)
| Ast.Boolean(b) -> Sast.Boolean(b, Sast.Bool)
| Ast.Id(v) -> Sast.Id(v, find_var v env.var_types) (* uses find_var to determine the type of id *)
| Ast.Binop(e1, op, e2) -> Sast.Binop(expr env e1, op, expr env e2, Sast.String) (* TODO: figure out the type of the expression and use that *)
| Ast.Assign(v, e) -> (* checks that the var and expression are of the same type, then converts to Sast.Assign *)
      let s_e = expr env e in
      let e_dt = get_expr_type s_e in
      if not( (find_var v env.var_types) = e_dt)
      then raise (Failure ("assignment expression not of type: " ^ type_to_str (find_var v env.var_types) ))
      else Sast.Assign(v, s_e, e_dt)
| Ast.AssignList(v, el) -> Sast.AssignList(v, List.map (fun e -> expr env e) el, Sast.Void) (* TODO: figure out the type of v and check *)
| Ast.DictAssign(v, e) -> Sast.DictAssign(expr env v, expr env e, Sast.Void) (* TODO: figure out the type of v and check *)
| Ast.Call(f, el) -> Sast.Call(f, List.map (fun e -> expr env e) el, Sast.String) (* TODO: figure out the return type and use that *)
| Ast.Access(v, e) -> Sast.Access(v, expr env e, Sast.String) (* TODO: figure out the type of v and check *)
| Ast.MemberVar(v, m) -> Sast.MemberVar(v, m, Sast.String) (* TODO: figure out the type of v and check *)
| Ast.MemberCall(v, m, el) -> Sast.MemberCall(v, m, List.map (fun e -> expr env e) el, Sast.String) (* TODO: figure out the return type and use that *)
| Ast.Undir(v1, v2) -> Sast.Undir(v1, v2, Sast.Void)
| Ast.Dir(v1, v2) -> Sast.Dir(v1, v2, Sast.Void)
| Ast.BidirVal(w1, v1, v2, w2) -> Sast.BidirVal(expr env w1, v1, v2, expr env w2, Sast.Void)
| Ast.UndirVal(v1, v2, w) -> Sast.UndirVal(v1, v2, expr env w, Sast.Void)
| Ast.DirVal(v1, v2, w) -> Sast.DirVal(v1, v2, expr env w, Sast.Void)
| Ast.NoOp(v) -> Sast.NoOp(v, Sast.Void)
| Ast.Noexpr -> Sast.Noexpr
in
(* convert an Ast.stmt object to Sast.stmt object *)
let rec stmt env = function
| Ast.Block(sl) -> Sast.Block(List.map (fun s -> stmt env s) sl)
| Ast.Expr(e) -> Sast.Expr(expr env e)
| Ast.Vdecl(dt, id) -> 
    (try 
        StringMap.find id !(List.hd env.var_types); raise (Failure ("variable already declared in local scope: " ^ id))
     with | Not_found -> (List.hd env.var_types) := StringMap.add id (str_to_type dt) !(List.hd env.var_types); (* add type map *)
                (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
          | Failure(f) -> raise (Failure (f) ) 
    );
    Sast.Vdecl(str_to_type dt, id)
| Ast.ListDecl(dt, id) -> (* Sast.ListDecl(str_to_type dt, v) *)
    let vtype = Sast.List(str_to_type dt) in
    (try 
        StringMap.find id !(List.hd env.var_types); raise (Failure ("variable already declared in local scope: " ^ id))
     with | Not_found -> (List.hd env.var_types) := StringMap.add id vtype !(List.hd env.var_types); (* add type map *)
                (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
          | Failure(f) -> raise (Failure (f) ) 
    );
    Sast.Vdecl(vtype, id)
| Ast.DictDecl(dtk, dtv, id) -> (*Sast.DictDecl(str_to_type dtk, str_to_type dtv, v)*)
    let vtype = Sast.Dict(str_to_type dtk, str_to_type dtv) in
    (try
        StringMap.find id !(List.hd env.var_types); raise (Failure ("variable already declared in local scope: " ^ id))
     with | Not_found -> (List.hd env.var_types) := StringMap.add id vtype !(List.hd env.var_types);
                (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds);
          | Failure(f) -> raise (Failure (f) )
    ); 
    Sast.Vdecl(vtype, id)
(*| _ -> failwith "Unknown") *)
| Ast.Return(e) -> Sast.Return(expr env e)
| Ast.If(cond, s1, s2) -> Sast.If(expr env cond, stmt env s1, stmt env s2)
| Ast.For(v1, v2, sl) -> Sast.For(v1, v2, List.map (fun s -> stmt env s) sl)
| Ast.While(cond, sl) -> Sast.While(expr env cond, List.map (fun s -> stmt env s) sl)
in
let fdecl env func = 
  {
    Sast.s_fname = func.fname;
    Sast.s_rtype = str_to_type func.rtype; 
    Sast.s_formals = List.map (fun (dt, v) -> (str_to_type dt, v)) func.formals;
    Sast.s_body = List.map (fun s -> stmt env s) func.body
  }
in
{Sast.s_funcs = List.map (fun f -> fdecl env f) prog.funcs;
 Sast.s_cmds = List.map (fun s -> stmt env s) prog.cmds}

(* get printf fmt string for Sast.dataType types *)
let dt_fmt = function
| Sast.Num -> "%f"
| Sast.String -> "%s"
| Sast.Bool -> "%d"
| Sast.Graph -> "" (* TODO *)
| Sast.Node -> "" (* TODO *)
| Sast.Dict(dtk, dtv) -> "" (* TODO *)
| Sast.List(dt) -> "" (* TODO *)
| Sast.Void -> "" (* TODO *)

(**********************)
(* TRANSLATES AN SAST *)
(**********************)
(* the meat of the compiler *)
(* actually converts Sast objects into strings of C code *)
let translate (env, functions, cmds) =
    let rec translate_expr env = function 
    | Sast.NumLiteral(l, dt) -> l
    | Sast.StrLiteral(l, dt) -> "\"" ^ l ^ "\""
    | Sast.Boolean(b, dt) -> if b = Ast.True then "true" else "false"
    | Sast.Id(v, dt) -> 
      (try
           "l" ^ string_of_int(find_var v env.var_inds)
       with
       | Not_found -> raise (Failure("undeclared variable: " ^ v))
      )
    | Sast.Binop(e1, op, e2, dt) -> "TODO"
    | Sast.Assign(v, e, dt) ->
        if not( (find_var v env.var_types) = get_expr_type e)
        then raise (Failure ("assignment exprecssion not of type: " ^ type_to_str (find_var v env.var_types) ))
        else (translate_expr env (Sast.Id(v, dt))) ^ " = " ^ (translate_expr env e)
    | Sast.AssignList(v, el, dt) -> "TODO"
    | Sast.DictAssign(k, v, dt) -> "TODO"
    | Sast.Call(func_name, el, dt) -> 
        (match func_name with
        | "print" ->
          (* fmt is all the format types so far: ex. %s%f%f *)
          (* vals is what will be put into the format vals: ex. "foo", 8.3, 8,3 *)
          let rec build_str fmt vals = function
          | [] -> (fmt, vals)
          | hd :: tl -> build_str (fmt ^ (dt_fmt(get_expr_type hd))) (vals ^ "," ^ (translate_expr env hd)) tl
              in
              let result = build_str "" "" el
              in
              "printf(\"" ^ fst result ^ "\"" ^ snd result ^ ")"

        | fname -> (try
               string_of_int(find_var fname env.func_inds) ^ 
               "(" ^ String.concat ", " (List.map (fun e -> translate_expr env e) el) ^ ")"
            with Not_found -> raise (Failure ("undefined function " ^ fname))
           ) )
  | Sast.Access(v, e, dt) -> "TODO"
  | Sast.MemberVar(v, m, dt) -> "TODO"
  | Sast.MemberCall(v, f, el, dt) -> "TODO"
  | Sast.Undir(v1, v2, dt) -> "TODO"
  | Sast.Dir(v1, v2, dt) -> "TODO"
  | Sast.UndirVal(v1, v2, w, dt) -> "TODO"
  | Sast.DirVal(v1, v2, w, dt) -> "TODO"
  | Sast.BidirVal(w1, v1, v2, w2, dt) -> "TODO"
  | Sast.NoOp(s, dt) -> "TODO"
  | Sast.Noexpr -> "TODO"
    in

    let rec translate_stmt env = function 
    | Sast.Block(sl) -> (match sl with
        | [] -> ""
        | hd :: tl -> translate_stmt env hd ^ translate_stmt env (Sast.Block(tl)) 
    )
    | Sast.Expr(e) -> translate_expr env e ^ ";"
    | Sast.Vdecl(t, id) ->
      (try 
        StringMap.find id !(List.hd env.var_types); raise (Failure ("variable already declared in local scope: " ^ id))
      with | Not_found -> (List.hd env.var_types) := StringMap.add id t !(List.hd env.var_types); (* add type map *)
                (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
                translate_vdecl ("l" ^ string_of_int(find_var id env.var_inds)) t        
           | Failure(f) -> raise (Failure (f) ) )
    | Sast.Return(e) -> "TODO"
    | Sast.If (cond, s1, s2) -> "TODO"
    | Sast.For (temp, iter, sl) -> "TODO"
    | Sast.While (cond, sl) -> "TODO"
    in

    let main_func = { crtype = "int";
                  cfname = "main";
                  cformals = [("int", "argc"); ("char**", "argv")];
                  cbody = List.map (fun s -> translate_stmt env s) cmds}
    in

    print_endline ((String.concat "\n" (List.map (fun h -> "#include " ^ h) headers)) ^ "\n" ^
                   string_of_cfunc main_func )
    

  (* creates a new default environment *)
  let create_env =
      let basic_env = 
      let bf_names = [ "print"; "range";] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      let bf_type_map = ref (string_map_pairs StringMap.empty [(Sast.Void, "print"); (Sast.List(Sast.Num), "range")]) in
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_types = [bf_type_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in
        basic_env


let print_bindings m =
  let bindings = StringMap.bindings m in
  let rec printer = function
    | [] -> print_endline("")
    | (k, v)::tl -> print_endline(k ^ string_of_int(v)) ; printer tl
  in
  printer bindings

(* translate version *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast_prg = (Parser.program Scanner.token lexbuf) in (* outputs the Ast from parsing and scanning *)
  let sast_env = (* set up default environment *)
      let bf_names = [ "print"; "range";] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      let bf_type_map = ref (string_map_pairs StringMap.empty [(Sast.Void, "print"); (Sast.List(Sast.Num), "range")]) in
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_types = [bf_type_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in
  let prg = convert_ast {funcs = ast_prg.funcs; cmds = List.rev ast_prg.cmds} sast_env  in
  (* comment out for real: *) (* print_endline ("converted ast to sast"); *)
  let trans_env = (* set up default environ *)
      let bf_names = [ "print"; "range";] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      let bf_type_map = ref (string_map_pairs StringMap.empty [(Sast.Void, "print"); (Sast.List(Sast.Num), "range")]) in
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_types = [bf_type_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in
  translate (trans_env, prg.s_funcs, prg.s_cmds)
   (* print_endline (String.concat "\n" (List.map string_of_stmt (List.rev prg.cmds))) *)

(* pretty printing version *)
(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = string_of_program (prg.funcs, List.rev prg.cmds) in
  print_endline result;;
*)