(*****************************)
(* CONVERTS AN AST TO AN SAST *) 
(*      STOP hoho time        *) 
(******************************)
open Ast
open Sast
open Analyzer


(* convert an Ast.expr object to Sast.expr object *)
let get_list_type = function
 | Sast.List(dt) ->  dt
 | _ -> raise (Failure("wrong type: not a list"))

(*       let s_e = expr env e in             (* func rec until it knows datatype -- sast version of ast expr e *)
      let e_dt = get_expr_type s_e in     (* data type of that sast expr with function get_expr_type*)
      (try                                (*sees if variable defined*)
          (find_var v env.var_inds)
       with
       | Not_found -> raise (Failure("undeclared variable: "))
      );
      if not( (find_var v env.var_types) = e_dt) (* gets type of var trying to assign get type trying to assign to *)
      then raise (Failure ("assignment expression not of type: " ^ type_to_str (find_var v env.var_types) ))
      else Sast.Assign(v, s_e, e_dt)
*)
let get_dict_type = function
 | Sast.Dict(dt1, dt2) ->  (dt1, dt2)
 | _ -> raise (Failure("wrong type: not a dict "))

let rec check_list env v_e = function
 | [] -> ""
 | hd::tl -> 
    if not(v_e = (get_expr_type hd)) then 
       raise (Failure ("list element not of type: " ^ type_to_str ( v_e )) )
    else 
        check_list env v_e tl

 let rec check_dict env v_e = function
 | [] -> ""
 | hd::tl -> 
    if not(v_e = (get_dict_type hd)) then
      raise (Failure ("assignment expression not of type: " ) )
    else 
        check_dict env v_e tl     

let convert_ast prog env =

let rec expr env = function
| Ast.NumLiteral(v) -> Sast.NumLiteral(v, Sast.Num)
| Ast.StrLiteral(v) -> Sast.StrLiteral(v, Sast.String)
| Ast.ListLiteral(el) ->
(* (try                                (*sees if variable defined*)
          let v_e = (find_var v env.var_types) in 
           let s_el = List.map (expr env) el in 
            check_list env v_e s_el;
            Sast.AssignList(v, s_el)
       with
       | Not_found -> raise (Failure("undeclared variable: "))
      );          List.map (expr env) el in (*sees if variable defined*) *)
  let s_el = List.map (expr env) el in
  (match el with
   | [] -> ListLiteral([], List(Void))
   | x ->  let dt = get_expr_type (List.hd s_el) 
           in
           check_list env dt s_el;
           ListLiteral(s_el, List(dt))
  )
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
              | _ -> raise (Failure("wrong type: Num + ? "))
            )
        |  String -> 
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.String)
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.String)
              | _ -> raise (Failure("wrong type: String + ? "))
            )
        |  Graph -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
              | _ -> raise (Failure("wrong type: Graph + ? "))
            )
        |  Node -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
              | _ -> raise (Failure("wrong type: Node + ? "))
            )
        |  List(dt) -> 
            (match e2_dt with
              | List(dt) -> 
                  if (e1_dt = e2_dt) then
                    Sast.Binop(s_e1,op,s_e2,e1_dt)
                  else 
                    raise (Failure("wrong type: List + List<?> "))
              | _ -> raise (Failure("wrong type: List + ? "))
            )
        |  _ -> raise (Failure("Expr using + has incompatible types"))
      )
    | Sub -> 
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Num)
              | _ -> raise (Failure("wrong type: Num - ? "))
            )
        |  Graph -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Graph)
              | _ -> raise (Failure("wrong type: Graph - ? "))
            )
        |  _ -> raise (Failure("Expr using - has incompatible types"))
      )
    | Mult -> 
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Num)
              | _ -> raise (Failure("wrong type: Num * ? "))
            )
        |  _ -> raise (Failure("Expr using * has incompatible types"))
      )
    | Div ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Num)
              | _ -> raise (Failure("wrong type: Num / ? "))
            )
        |  _ -> raise (Failure("Expr using / has incompatible types"))
      )
    | Equal ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Num == ? "))
            )
        |  String -> 
            (match e2_dt with
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: String == ? "))
            )
        |  Bool -> 
            (match e2_dt with
              | Bool -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Bool == ? "))
            )
        | Void -> 
            (match e2_dt with
              | Void -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Void == ? "))
            )
        |  Graph -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Graph == ? "))
            )
        |  Node -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Node == ? "))
            )
        |  List(dt) -> 
            (match e2_dt with
              | List(dt) -> 
                  if (e1_dt = e2_dt) then
                    Sast.Binop(s_e1,op,s_e2,Sast.Bool)
                  else 
                    raise (Failure("wrong type: List == List<?> "))
              | _ -> raise (Failure("wrong type: List == ? "))
            )
        |  Dict(dtk,dtv) -> 
            (match e2_dt with
              | Dict(dtk,dtv) -> 
                  if (e1_dt = e2_dt) then
                    Sast.Binop(s_e1,op,s_e2,Sast.Bool)
                  else 
                    raise (Failure("wrong type: Dict == Dict<?> "))
              | _ -> raise (Failure("wrong type: Dict == ? "))
            )
        |  _ -> raise (Failure("Expr using == has incompatible types"))
      )
    | Neq ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Num != ? "))
            )
        |  String -> 
            (match e2_dt with
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: String != ? "))
            )
        |  Bool -> 
            (match e2_dt with
              | Bool -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Bool != ? "))
            )
        | Void -> 
            (match e2_dt with
              | Void -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Void != ? "))
            )
        |  Graph -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Graph != ? "))
            )
        |  Node -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Node != ? "))
            )
        |  List(dt) -> 
            (match e2_dt with
              | List(dt) -> 
                  if (e1_dt = e2_dt) then
                    Sast.Binop(s_e1,op,s_e2,Sast.Bool)
                  else 
                    raise (Failure("wrong type: List != List<?> "))
              | _ -> raise (Failure("wrong type: List != ? "))
            )
        |  Dict(dtk,dtv) -> 
            (match e2_dt with
              | Dict(dtk,dtv) -> 
                  if (e1_dt = e2_dt) then
                    Sast.Binop(s_e1,op,s_e2,Sast.Bool)
                  else 
                    raise (Failure("wrong type: Dict != Dict<?> "))
              | _ -> raise (Failure("wrong type: Dict != ? "))
            )
        |  _ -> raise (Failure("Expr using != has incompatible types"))
      )

    | Less ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Num < ? "))
            )
        |  String -> 
            (match e2_dt with
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: String < ? "))
            )
        |  _ -> raise (Failure("Expr using < has incompatible types"))
      )
    | Leq ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Num <= ? "))
            )
        |  String -> 
            (match e2_dt with
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: String <= ? "))
            )
        |  _ -> raise (Failure("Expr using <= has incompatible types"))
      )
    | Greater ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Num > ? "))
            )
        |  String -> 
            (match e2_dt with
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: String > ? "))
            )
        |  _ -> raise (Failure("Expr using > has incompatible types"))
      )

    | Geq -> 
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Num >= ? "))
            )
        |  String -> 
            (match e2_dt with
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: String >= ? "))
            )
        |  _ -> raise (Failure("Expr using >= has incompatible types")))

    | LogAnd ->
      (match e1_dt with
        |  Bool ->
            (match e2_dt with
              | Bool -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Bool && ? "))
            )
        |  _ -> raise (Failure("Expr using && has incompatible types"))
      )
    | LogOr -> 
      (match e1_dt with
        |  Bool ->
            (match e2_dt with
              | Bool -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Bool || ? "))
            )
        |  _ -> raise (Failure("Expr using || has incompatible types"))
      )
    | _ -> raise (Failure("raise failure"))
  )
                        (* TODO: figure out the type of v and check *)
| Ast.Call(f, el) -> Sast.Call(f, List.map (fun e -> expr env e) el, Sast.String)                   (* TODO: figure out the return type and use that *)
| Ast.Access(v, e) -> 
    let s_e = expr env e in             (* func rec until it knows datatype -- sast version of ast expr e *)
    let e_dt = get_expr_type s_e in
    (try                                (*sees if variable defined*)
        let v_e = find_var v env.var_types in
         (match v_e with
           List(dt) -> 
              (match e_dt with 
                 | Sast.Num -> Sast.Access(v, s_e, dt)
                 | _ -> raise (Failure "expr to access list should be Num") 
              )
          | Dict(dk,dv) ->  
            if (e_dt = dk) then
                Sast.Access(v, s_e, dv)
            else 
                raise (Failure("wrong type: Dict != Dict<?> "))
          )
     with
     | Not_found -> raise (Failure("undeclared variable: "))
    );                                  
| Ast.MemberVar(v, m) -> 
    (try                                (*sees if variable defined*)
        let v_e = find_var v env.var_types in
         (match v_e with
          | Sast.Node -> 
              (try 
                let mem_r_type = StringMap.find m mem_vars in
                if (mem_r_type = Sast.Node) then
                  (match m with
                    | "in" -> Sast.MemberVar(v,m, Sast.List(Sast.Node))
                    | "out" -> Sast.MemberVar(v,m, Sast.List(Sast.Node))
                    | "values" -> Sast.MemberVar(v,m, (Sast.String))
                    | _ -> raise (Failure("undeclared variable: "))
                  )
                else 
                    raise (Failure("stupid "))
              with
                | Not_found -> raise (Failure("not a member function for graphs")) 
              )
          | Sast.Graph ->  
            (try
              let mem_r_type = StringMap.find m mem_vars in
              if (mem_r_type = Sast.Graph) then 
                (match m with
                  | "nodes" -> Sast.MemberVar(v, m, Sast.List(Sast.Node)) 
                  | _ -> raise (Failure("undeclared variable: "))
                )
              else 
                  raise (Failure("hosanna tired "))
            with
            | Not_found -> raise (Failure("not a member function for graphs"))
            ) 
          )
     with
     | Not_found -> raise (Failure("undeclared variable: "))
    )     
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
| Ast.Assign(v, e) ->                     (* checks that the var and expression are of the same type, then converts to Sast.Assign *)
      let s_e = expr env e in             (* func rec until it knows datatype -- sast version of ast expr e *)
      let e_dt = get_expr_type s_e in     (* data type of that sast expr with function get_expr_type*)
      (try                                (*sees if variable defined*)
          (find_var v env.var_inds)
       with
       | Not_found -> raise (Failure("undeclared variable: "))
      );
      if not( (find_var v env.var_types) = e_dt) (* gets type of var trying to assign get type trying to assign to *)
      then raise (Failure ("assignment expression not of type: " ^ type_to_str (find_var v env.var_types) ))
      else Sast.Assign(v, s_e, e_dt)
(* | Ast.AssignList(v, el) -> 
      (*insert a recursive function*) 
      (try                                (*sees if variable defined*)
          let v_e = (find_var v env.var_types) in 
           let s_el = List.map (expr env) el in 
            check_list env v_e s_el;
            Sast.AssignList(v, s_el)
       with
       | Not_found -> raise (Failure("undeclared variable: "))
      );   
| Ast.DictAssign(v, tl) -> 
    (try                                (*sees if variable defined*)
        let v_e = (find_var v env.var_types) in 
         let s_tl = List.map (expr env) tl in 
          check_dict env v_e s_tl;
          Sast.DictAssign(v, s_tl)
     with
     | Not_found -> raise (Failure("undeclared variable: "))
    ); 
      );                           (*sees if variable defined*) *)
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

  (* { Sast.s_funcs = List.map (fun f -> fdecl env f) prog.funcs;
  Sast.s_cmds = List.map (fun s -> stmt env s) prog.cmds } *)
 { Sast.s_cmds = List.map (fun s -> stmt env s) prog.cmds }

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