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

   value: type 
   key: variable name  

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
| Sast.ListLiteral(el, dt) -> dt
| Sast.Boolean(v, dt) -> dt
| Sast.Id(v, dt) -> dt
| Sast.Binop(e1, op, e2, dt) -> dt
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

(*| Sast.Assign(v, e, dt) -> Sast.Void
| Sast.AssignList(s, el, dt) -> Sast.Void
*)

(**********************)
(* TRANSLATES AN SAST *)
(**********************)
(* determines whether a num string is an Int or a Float *)
let num_type num_str = 
    let numregex = Str.regexp "-?[0-9]+$"
    in
    if Str.string_match numregex num_str 0 then Int else Float

let dt_to_ct = function
| Sast.Num -> Float
| Sast.String -> Cstring
| Sast.Bool -> Int
| Sast.Graph -> Void (* TODO *)
| Sast.Node -> Void (* TODO *)
| Sast.List(dt) -> Void (* TODO *)
| Sast.Dict(dtk, dtv) -> Void (* TODO *)
| Sast.Void -> Void

(* the meat of the compiler *)
(* actually converts Sast objects into strings of C code *)
let translate (env, functions, cmds) =
    (* 
       every time a var has to be auto-created 
       incr a counter so that we don't repeat vars in C 
    *)
    let auto_cnt = ref 0 
    in

    let rec translate_expr env = function 
    | Sast.NumLiteral(l, dt) -> Literal(Float, l)
    | Sast.StrLiteral(l, dt) -> Literal(Cstring, l)
    | Sast.ListLiteral(el, dt) -> Noexpr (* TODO *)
    | Sast.Boolean(b, dt) -> if b = Ast.True then Literal(Int, "1") else Literal(Int, "0")
    | Sast.Id(v, dt) -> 
         let index = "v" ^ string_of_int(find_var v env.var_inds) (* see if id exists, get the num index of the var *)
         in
         Id(Void, index) 
    | Sast.Binop(e1, op, e2, dt) ->
        let ce1 = translate_expr env e1 in
        let ce2 = translate_expr env e2 in
(*         (
          match dt with
          | Num -> Binop(Float, ce1, op, ce2) (* how can we tell if it's really an int? *)
          | String -> Binop(Cstring, ce1, op, ce2)
          | Bool -> Binop(Int, ce1, op, ce2)
          | Graph -> Noexpr (* TODO *)
          | Node -> Noexpr (* TODO *)
          | List(dt) -> Noexpr (* TODO *)
          | Dict(dtk, dtv) -> Noexpr (* TODO *)
          | Void -> raise (Failure "why is there a void binop?")
        ) *)
            (match op with
              | Add -> Noexpr (* TODO *)
              | Sub -> Noexpr (* TODO *)
              | Mult | Div -> Binop(Float, ce1, op, ce2)
              | Equal -> Noexpr (* TODO *)
              | Neq -> Noexpr (* TODO *)
              | Less -> Noexpr (* TODO *)
              | Leq -> Noexpr (* TODO *)
              | Greater -> Noexpr (* TODO *)
              | Geq -> Noexpr (* TODO *)
              | LogAnd -> Noexpr (* TODO *)
              | LogOr -> Noexpr (* TODO *)
            )
    | Sast.DictAssign(k, v, dt) -> Noexpr (* TODO *)
    | Sast.Call(func_name, el, dt) -> 
        let cel = List.map (translate_expr env) el in
        let index = "f" ^ string_of_int(find_var func_name env.func_inds) in
        Call(dt_to_ct dt, index, cel)
  | Sast.Access(v, e, dt) -> 
      let index = "v" ^ string_of_int(find_var v env.var_inds) in
      let ce = translate_expr env e in
      Access(dt_to_ct dt, index, ce)
  | Sast.MemberVar(v, m, dt) -> Noexpr (* TODO *)
  | Sast.MemberCall(v, f, el, dt) -> Noexpr (* TODO *)
  | Sast.Undir(v1, v2, dt) -> Noexpr (* TODO *)
  | Sast.Dir(v1, v2, dt) -> Noexpr (* TODO *)
  | Sast.UndirVal(v1, v2, w, dt) -> Noexpr (* TODO *)
  | Sast.DirVal(v1, v2, w, dt) -> Noexpr (* TODO *)
  | Sast.BidirVal(w1, v1, v2, w2, dt) -> Noexpr (* TODO *)
  | Sast.NoOp(s, dt) -> Noexpr (* TODO *)
  | Sast.Noexpr -> Noexpr
    in

(*   if not( (find_var v env.var_types) = get_expr_type e)
          then raise (Failure ("assignment exprecssion not of type: " ^ type_to_str (find_var v env.var_types) ))
          else (translate_expr env (Sast.Id(v, dt))) ^ " = " ^ (translate_expr env e)
*)
    let rec translate_stmt env = function 
    | Sast.Block(sl) -> 
        let csl = List.map (translate_stmt env) sl in
        Block(csl)
    (*     (match sl with
        | [] -> Block([Expr(Noexpr)])
        | hd :: tl -> Block([Expr(Noexpr)])
       (* | hd :: tl -> translate_stmt env hd ^ translate_stmt env (Sast.Block(tl)) *)
    ) *)
    | Sast.Expr(e) -> Expr(translate_expr env e)
    | Sast.Vdecl(dt, id) ->
     (List.hd env.var_types) := StringMap.add id dt !(List.hd env.var_types); (* add type map *)
        (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index map *)
        let index = "v" ^ string_of_int(find_var id env.var_inds) in
        (match dt with
          | Num -> Vdecl(Float, index)
          | String -> Vdecl(Cstring, index)
          | Bool -> Vdecl(Int, index)
          | Graph -> Block([Vdecl(Ptr(Graph), index);
                            Expr(Assign(index, Call(Void, "init_graph", [])))
                          ]) (* C: graph_t *g1 = init_graph(); *)
          | Node -> Block([Vdecl(Ptr(Node), index); 
                           Expr(Assign(index, Call(Void, "init_node", [Literal(Cstring, "")])))
                         ]) (* C: node_t *x = init_node(""); *)
          | List(dt) -> Vdecl(Ptr(List), index) (* C: list_t *x; *)
          | Dict(dtk, dtv) -> Vdecl(Dict, index) (* TODO *)
          | Void -> raise (Failure ("should not be using Void as a datatype"))
        )
    | Sast.Assign(v, e, dt) ->
        let ce = translate_expr env e in
        let var_type = get_expr_type e in
        let index = "v" ^ string_of_int(find_var v env.var_inds) in
        let auto_var = "a" ^ string_of_int((auto_cnt := !auto_cnt + 1); !auto_cnt) in
        (match var_type with
            | Num | String | Bool | Node | Void -> Expr(Assign(index, ce))
            | List(dt) -> Expr(Assign(index, ce)) (* TODO *)   
            | Dict(dtk, dtv) -> Expr(Assign(index, ce)) (* TODO *)
            | Graph -> Expr(Assign(index, Call(Graph, "copy", [ce])))
        )
        (*         if not( (find_var v env.var_types) = get_expr_type e)
        then raise (Failure ("assignment expression not of type: " ^ type_to_str (find_var v env.var_types) ))
        else (translate_expr env (Sast.Id(v, dt))) ^ " = " ^ (translate_expr env e) *)
    | Sast.AssignList(v, el) -> Expr(Noexpr) (* TODO *)
          (* Block([Vdecl(Ptr(dt_to_ct dt), auto_var);
                           Cast(Ptr(var_type), Call("malloc", [Call("sizeof", type_to_str var_type)]))
                         ]) *)
    | Sast.Return(e) -> Expr(Noexpr)                   (*TODO*)
    | Sast.If (cond, s1, s2) -> Expr(Noexpr)           (*TODO*)
    | Sast.For (temp, iter, sl) ->
        let auto_var = "a" ^ string_of_int((auto_cnt := !auto_cnt + 1); !auto_cnt) in
        let index = "v" ^ string_of_int (find_var iter env.var_inds) in
        let dt = (find_var iter env.var_types) in
        let csl = List.map (translate_stmt env) sl in
        (match dt with
         | List(dt) -> Block([Vdecl(Ptr(List), auto_var); 
                              For(Assign(auto_var, Id(Ptr(List), index)),
                                  Id(Ptr(List), auto_var),
                                  Assign(auto_var, Member(Ptr(List), auto_var, "next")),
                                  csl
                                 )
                             ])
         | Dict(dtk, dtv) -> Expr(Noexpr) (* TODO *)
         | Node -> Block([Vdecl(Ptr(Node), auto_var); 
                          Expr(Assign(auto_var, Ref(Id(Ptr(Node), index))))] @ csl)

                      (* Block([Vdecl(Ptr(Node), auto_var); 
                          For(Assign(auto_var, Ref(Id(Ptr(Node), index))),
                              Id(Ptr(Node), auto_var),
                              Assign(auto_var, Literal(Void, "NULL")),
                              csl
                             )
                          ]) *)
         | Graph -> Block([Vdecl(Ptr(Node), auto_var); 
                              For(Assign(auto_var, Member(Ptr(Node), index, "nodes")),
                                  Id(Ptr(Node), auto_var),
                                  Assign(auto_var, Member(Ptr(Node), auto_var, "next")),
                                  csl
                                 )
                             ])
         | _ -> raise (Failure(iter ^ " is not iterable"))
       )
    | Sast.While (cond, sl) -> 
        let c_cond = translate_expr env cond in
        let csl = List.map (translate_stmt env) sl in
        While(c_cond, csl)
    in

    let main_func = { crtype = "int";
                  cfname = "main";
                  cformals = [("int", "argc"); ("char**", "argv")];
                  cbody = List.map (fun s -> translate_stmt env s) cmds}
     in
     main_func

    (*print_endline ((String.concat "\n" (List.map (fun h -> "#include " ^ h) headers)) ^ "\n" ^
                   string_of_cfunc main_func ) *)
    

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
