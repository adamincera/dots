(******************************)
(* CONVERTS AN AST TO AN SAST *) 
(*      STOP hoho time        *) 
(******************************)
open Ast
open Sast
open Analyzer


(* convert an Ast.expr object to Sast.expr object *)
let get_list_type = function
 | Sast.List(dt) ->  dt
 | _ -> "failure"

 let get_dict_type = function
 | Sast.Dict(dt1, dt2) ->  (dt1, dt2)
 | _ -> "failure"

let convert_ast prog env =

let rec expr env = function
| Ast.NumLiteral(v) -> Sast.NumLiteral(v, Sast.Num)
| Ast.StrLiteral(v) -> Sast.StrLiteral(v, Sast.String)
| Ast.Boolean(b) -> Sast.Boolean(b, Sast.Bool)
| Ast.Id(v) -> Sast.Id(v, find_var v env.var_types) (* uses find_var to determine the type of id *)
| Ast.Binop(e1, op, e2) -> 
    let s_e1 = expr env e1 in
    let s_e2 = expr env e2 in              
    let e1_dt = get_expr_type s_e1 in
    let e2_dt = get_expr_type s_e2 in
    (match op with
    | Add -> 
     (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Num)
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.String)
              | _ -> raise (Failure("wrong type dummy: Num"))
            )
        |  String -> 
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.String)
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.String)
              | _ -> "failure"
            )
        |  Graph -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
              | _ -> "failure"
            )
        |  Node -> 
            (match e2_dt with
                  | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
                  | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
                  | _ -> "failure"
            )
        |  List -> 
            (match e2_dt with
                      | List -> 
                          if (e1_dt = e2_dt) then
                            Sast.Binop(s_e1,op,s_e2,Sast.List)
                          else 
                            "failure"
                      | _ -> "failure"
            )
        |  _ -> "signify particular expr. shouldn't work"
    | Sub -> expr
    | Mult -> expr
    | Div -> expr
    | Equal -> expr
    | Neq -> expr
    | Less -> expr
    | Leq -> expr
    | Greater -> expr
    | Geq -> expr
    | LogAnd -> expr
    | LogOr -> expr
    | _ -> "raise failure")

    Sast.Binop(expr env e1, op, expr env e2, Sast.String) (* TODO: figure out the type of the expression and use that *)
| Ast.Assign(v, e) ->                     (* checks that the var and expression are of the same type, then converts to Sast.Assign *)
      let s_e = expr env e in             (* func rec until it knows datatype -- sast version of ast expr e *)
      let e_dt = get_expr_type s_e in     (* data type of that sast expr with function get_expr_type*)
      (try                                (*sees if variable defined*)
          (find_var v env.var_inds)
       with
       | Not_found -> raise (Failure("undeclared variable: " ^ v))
      );
      if not( (find_var v env.var_types) = e_dt) (* gets type of var trying to assign get type trying to assign to *)
      then raise (Failure ("assignment expression not of type: " ^ type_to_str (find_var v env.var_types) ))
      else Sast.Assign(v, s_e, e_dt)
| Ast.AssignList(v, el) -> Sast.AssignList(v, List.map (fun e -> expr env e) el, Sast.Void)         (* TODO: figure out the type of v and check *)
| Ast.DictAssign(v, e) -> Sast.DictAssign(expr env v, expr env e, Sast.Void)                        (* TODO: figure out the type of v and check *)
| Ast.Call(f, el) -> Sast.Call(f, List.map (fun e -> expr env e) el, Sast.String)                   (* TODO: figure out the return type and use that *)
| Ast.Access(v, e) -> Sast.Access(v, expr env e, Sast.String)                                       (* TODO: figure out the type of v and check *)
| Ast.MemberVar(v, m) -> Sast.MemberVar(v, m, Sast.String)                                          (* TODO: figure out the type of v and check *)
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

{
  Sast.s_funcs = List.map (fun f -> fdecl env f) prog.funcs;
  Sast.s_cmds = List.map (fun s -> stmt env s) prog.cmds
}

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