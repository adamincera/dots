(* semantically checks dot sast and converts to c ast *)
open Ast
open Sast
open Translate

module StringMap = Map.Make(String)

(* "\"graph.h\"" *)
let headers = ["<stdio.h>"; "<stdlib.h>"; "<string.h>"]

type translation_env = {
            var_inds : int StringMap.t ref list;              (* var names to indices ex. x -> 1 so that we can just refer to it as v1 *)
            var_types : Sast.dataType StringMap.t ref list;   (* maps a var name to its type  ex. x -> num *)
            func_inds : int StringMap.t ref list;
            func_types : Sast.dataType StringMap.t ref list;  (* maps a func name to its return type *)
            return_type : Sast.dataType                       (* what should the return type be of the current scope *)
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

(* 
   returns the value associated with a given key,
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
    | Sast.Call(func_name, el, dt) -> "TODO"
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
        | [] -> Block([Expr(Noexpr)])
        | hd :: tl -> Block([Expr(Noexpr)])
       (*) | hd :: tl -> translate_stmt env hd ^ translate_stmt env (Sast.Block(tl)) *)
    )
    | Sast.Expr(e) -> (* Expr(translate_expr env e)  *) Expr(Noexpr)
    | Sast.Vdecl(t, id) ->
      (try 
        StringMap.find id !(List.hd env.var_types); raise (Failure ("variable already declared in local scope: " ^ id))
      with | Not_found -> (List.hd env.var_types) := StringMap.add id t !(List.hd env.var_types); (* add type map *)
                (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
                (*translate_vdecl ("l" ^ string_of_int(find_var id env.var_inds)) t  *)
                Expr(Noexpr)                   (*TODO*)     
           | Failure(f) -> raise (Failure (f) ) )
    | Sast.Return(e) -> Expr(Noexpr)                   (*TODO*)
    | Sast.If (cond, s1, s2) -> Expr(Noexpr)           (*TODO*)
    | Sast.For (temp, iter, sl) -> Expr(Noexpr)        (*TODO*)
    | Sast.While (cond, sl) -> Expr(Noexpr)            (*TODO*)
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
