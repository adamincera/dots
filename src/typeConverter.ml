(*****************************)
(* CONVERTS AN AST TO AN SAST *) 
(******************************)
open Ast
open Sast
open Analyzer

(* extract dt from list *)
let get_list_type = function
 | Sast.List(dt) ->  dt
 | _ -> raise (Failure("wrong type: not a list"))

(* extract key type, val type from dict *)
let get_dict_type = function
 | Sast.Dict(dt1, dt2) ->  (dt1, dt2)
 | _ -> raise (Failure("wrong type: not a dict "))

(* make sure each element in a list is the right type *)
let rec check_list env v_e = function
 | [] -> ""
 | hd::tl -> 
    if not(v_e = (get_sexpr_type hd)) then 
       raise (Failure ("list element not of type: " ^ type_to_str ( v_e )) )
    else 
      check_list env v_e tl


let rec check_graph_list env = function
 | [] -> ""
 | hd::tl -> 
      (match hd with 
        | Sast.Id(v, dt) ->  
          let e_dt = get_sexpr_type hd in
          (if not (e_dt = Sast.Node || e_dt = Sast.Graph) then
            raise (Failure ("you can not have a graph def with type other than Node or Graph")) 
          else 
            check_graph_list env tl)
        | Sast.Undir(v1,v2,dt) | Sast.Dir(v1,v2,dt) -> 
            check_graph_list env tl 
        | Sast.UndirVal(v1,v2,e1,dt) | Sast.DirVal(v1,v2,e1,dt) -> 
           check_graph_list env tl 
        | Sast.BidirVal(e1,v1,v2,e2,dt) ->
            check_graph_list env tl 
        | _ -> raise (Failure ("type not expected in Graph Def")))

(* make sure each pair in dict assignment is right type *)
let rec check_dict env v_e = function
 | [] -> ""
 | hd::tl -> 
    if not((fst v_e = get_sexpr_type (fst hd)) && (snd v_e = get_sexpr_type (snd hd))) then
      raise (Failure ("assignment expression not of type: " ) )
    else 
      check_dict env v_e tl

(* match arguments to a function call to that func's definition *)
 let rec formal_check s_formal_list s_el = 
  match s_formal_list, s_el with
    | [], [] -> true
    | [], _ -> raise (Failure ("incorrect arguments" ) )
    | _, [] -> raise (Failure ("incorrect arguments" ) )
    | hd1::tl1, hd2::tl2 -> ((fst hd1) = (get_sexpr_type hd2)) && (formal_check tl1 tl2)  


(* converts Ast.program to Sast.program *)
let convert_ast prog env =

(* convert an Ast.expr object to Sast.expr object *)
let rec expr env = function
| Ast.NumLiteral(v) -> Sast.NumLiteral(v, Sast.Num)
| Ast.StrLiteral(v) -> Sast.StrLiteral(v, Sast.String)
| Ast.ListLiteral(el) ->
  let s_el = List.map (expr env) el in
  (match el with
   | [] -> ListLiteral([], List(Void))
   | x ->  let dt = get_sexpr_type (List.hd s_el) 
           in
           ignore (check_list env dt s_el); 
           ListLiteral(s_el, List(dt))
  )
| Ast.DictLiteral(el) -> 
(* key_type, elem_type, id [(Hello, 15)] *)
  let s_el =  List.map (fun f -> (expr env (fst f), expr env (snd f))) el in
  (match s_el with 
    | [] -> DictLiteral([], Dict(Void,Void))
    | x -> 
      let dt = (get_sexpr_type (fst(List.hd s_el)), get_sexpr_type (snd(List.hd s_el))) 
              in 
              ignore (check_dict env dt s_el);
              DictLiteral(s_el, Sast.Dict(fst dt, snd dt))
  ) 
| Ast.Boolean(b) -> Sast.Boolean(b, Sast.Bool)
| Ast.Id(v) -> 
    (try
        Sast.Id(v, find_var v env.var_types) (* uses find_var to determine the type of id *)
     with
     | Not_found -> raise (Failure ("undeclared variable: " ^ v))
    )
| Ast.Binop(e1, op, e2) -> 
    let s_e1 = expr env e1 in
    let s_e2 = expr env e2 in              
    let e1_dt = get_sexpr_type s_e1 in
    let e2_dt = get_sexpr_type s_e2 in
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
    | Mult | Div ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Num)
              | _ -> raise (Failure("wrong type: Num * or / ? "))
            )
        |  _ -> raise (Failure("Expr using / has incompatible types"))
      )
    | Equal | Neq ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Num ==/!= ? "))
            )
        |  String -> 
            (match e2_dt with
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: String ==/!= ? "))
            )
        |  Bool -> 
            (match e2_dt with
              | Bool -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Bool ==/!= ? "))
            )
        | Void -> 
            (match e2_dt with
              | Void -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Void ==/!= ? "))
            )
        |  Graph -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Graph ==/!= ? "))
            )
        |  Node -> 
            (match e2_dt with
              | Node -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | Graph -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Node ==/!= ? "))
            )
        |  List(dt) -> 
            (match e2_dt with
              | List(dt) -> 
                  if (e1_dt = e2_dt) then
                    Sast.Binop(s_e1,op,s_e2,Sast.Bool)
                  else 
                    raise (Failure("wrong type: List ==/!= List<?> "))
              | _ -> raise (Failure("wrong type: List ==/!= ? "))
            )
        |  Dict(dtk,dtv) -> 
            (match e2_dt with
              | Dict(dtk,dtv) -> 
                  if (e1_dt = e2_dt) then
                    Sast.Binop(s_e1,op,s_e2,Sast.Bool)
                  else 
                    raise (Failure("wrong type binop: Dict ==/!= Dict<?> "))
              | _ -> raise (Failure("wrong type: Dict ==/!= ? "))
            )
      )

    | Less | Leq | Greater | Geq ->
      (match e1_dt with
        |  Num ->
            (match e2_dt with
              | Num -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Num </>/<=/>= ? "))
            )
        |  String -> 
            (match e2_dt with
              | String -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: String </>/<=/>= ? "))
            )
        |  _ -> raise (Failure("Expr using </>/<=/>= has incompatible types"))
      )

    | LogAnd | LogOr ->
      (match e1_dt with
        |  Bool ->
            (match e2_dt with
              | Bool -> Sast.Binop(s_e1,op,s_e2,Sast.Bool)
              | _ -> raise (Failure("wrong type: Bool &&/|| ? "))
            )
        |  _ -> raise (Failure("Expr using &&/|| has incompatible types"))
      )
  )
| Ast.Call(f, el) ->
    let s_el = List.map (expr env) el in
  (*
  match range can only take 1 or two args not 0 or 3+ and make sure nums...
  instead of f try check with the func name 
  if it does exist put the value of the key function name for the map func_types
  s_formals : (dataType * string) list;
  range(1,5)
    [1,2,3,4,5]
   *)
  if f = "range" then 
      (let len = List.length el in 
      if (len = 1) then 
        let arg_types = [(Sast.Num, "foo")] in 
        ignore (formal_check arg_types s_el);
        Sast.Call(f, s_el , Sast.List(Sast.Num))
      else if (len = 2) then 
        let arg_types = [(Sast.Num, "foo"); (Sast.Num, "foo")] in 
        ignore (formal_check arg_types s_el);
        Sast.Call(f, s_el , Sast.List(Sast.Num))
      else 
        raise(Failure("range can only take 1 or 2 args"))
    )
  else if f = "print" then Sast.Call(f, s_el, Sast.Void)
  else if (f = "min" || f = "max") then    
      (try                            
        let s_el = List.map (expr env) el in
        let data_type = get_sexpr_type (List.hd s_el) in
        let len = List.length el in 
          if (len = 1) then
             (match data_type with 
                | Sast.List(dt) -> Sast.Call(f, s_el, dt)
                | Sast.Dict(dtk, dtv) -> Sast.Call(f, s_el, dtk) 
                | _ -> raise(Failure("member call failed")))
          else 
            raise(Failure("member call failed"))
        with 
          Not_found -> raise (Failure("undeclared variable: ")))
  else if f = "len" then 
        (try                            
        let s_el = List.map (expr env) el in
        let data_type = get_sexpr_type (List.hd s_el) in
        let len = List.length el in 
          if (len = 1) then
             (match data_type with 
                Sast.List(dt) -> Sast.Call(f, s_el, Sast.Num)
                | Sast.Dict(dtk, dtv) -> Sast.Call(f, s_el, Sast.Num) 
                | _ -> raise(Failure("member call failed")))
          else 
            raise(Failure("member call failed"))
        with 
          Not_found -> raise (Failure("undeclared variable: ")))
  else 
    (
      let fdecl = find_var f env.func_obj in 
       ignore (formal_check fdecl.s_formals s_el);
       let rtype = fdecl.s_rtype in
       Sast.Call(f, s_el , rtype)
    )
| Ast.Access(e1, e2) -> 
    let s_e1 = expr env e1 in             (* func rec until it knows datatype -- sast version of ast expr e *)
    let e1_dt = get_sexpr_type s_e1 in
    let s_e2 = expr env e2 in             (* func rec until it knows datatype -- sast version of ast expr e *)
    let e2_dt = get_sexpr_type s_e2 in
    (try                                (*sees if variable defined*)
         (match e1_dt with 
           List(dt) -> 
              (match e2_dt with 
                 | Sast.Num -> Sast.Access(s_e1, s_e2, dt)
                 | _ -> raise (Failure "expr to access list should be Num") 
              )
          | Dict(dk,dv) ->  
            if (e2_dt = dk) then
                Sast.Access(s_e1, s_e2, dv)
            else 
                raise (Failure("wrong type ast.access: sexpr type != Dict<?>, expected " ^ (type_to_str dk) ^ " got " ^ (type_to_str e2_dt) ^ " life sucks"))
          | _ -> raise (Failure("must use Dict or List with access!"))
          )
     with
     | Not_found -> raise (Failure("undeclared variable: "))
    );                                      
| Ast.MemberCall(e, m, el) -> 
(
    let s_e = expr env e in
    let e_dt = get_sexpr_type s_e in
    let num_args = List.length el in
    let s_el = List.map (expr env) el in

    match m with
    | "enqueue" | "push" -> 
        if num_args != 1 then raise (Failure ("enqueue/push requires 1 arg"))
        else   
            ignore((match e_dt with
             | List(_) -> ignore()
             | _ -> raise (Failure ("enqueue/push error: not a list"))
            ));
            ignore(check_list env (get_list_type e_dt) s_el); (* check that the arg is the type in the list *)
            Sast.MemberCall(s_e, m, s_el, Sast.Void)
    | "dequeue" | "pop" -> 
        if num_args != 0 then raise (Failure ("dequeue/pop requires 0 args"))
        else Sast.MemberCall(s_e, m, s_el, Sast.Void)
    | "peek" ->
        if num_args != 0 then raise (Failure ("peek requires 0 args"))
        else Sast.MemberCall(s_e, m, s_el, get_list_type e_dt)
    | "oute" | "ine" -> 
        if num_args != 0 then raise (Failure ("oute/ine requires 0 args"))
        else Sast.MemberCall(s_e, m, s_el, Dict(Node, Num))
    | "val" ->
        if num_args != 0 then raise (Failure ("val requires 0 args"))
        else Sast.MemberCall(s_e, m, s_el, String)
    | "remove" -> 
        if num_args != 1 then raise (Failure ("remove requires 1 arg"))
        else   
            ignore((match e_dt with
             | Dict(_) -> ignore()
             | _ -> raise (Failure ("remove error: not a dict"))
            ));
            ignore(check_list env (fst (get_dict_type e_dt)) s_el); (* check that the arg is the type in the list *)
            Sast.MemberCall(s_e, m, s_el, Sast.Void)
    | _ -> raise (Failure ("no member function: " ^ m))
)
| Ast.Undir(v1, v2) -> 
    (*check if v1 and v2 exist *)
    (try                                (*sees if variable defined*)
        let v1_e = find_var v1 env.var_types in
         if v1_e = Sast.Node then 
            let v2_e = find_var v2 env.var_types in 
              if v2_e = Sast.Node then 
                Sast.Undir(v1, v2, Sast.Void)
              else raise (Failure("Wrong variable types"))
          else 
            raise (Failure("Wrong variable types"))
      with
     | Not_found -> raise (Failure("undeclared variable: ")))

| Ast.Dir(v1, v2) -> 
   (try                                (*sees if variable defined*)
        let v1_e = find_var v1 env.var_types in
          if v1_e = Sast.Node then 
            let v2_e = find_var v2 env.var_types in 
              if v2_e = Sast.Node then 
                Sast.Dir(v1, v2, Sast.Void)
              else raise (Failure("undeclared variable: "))
          else raise (Failure("undeclared variable: "))
      with
     | Not_found -> raise (Failure("undeclared variable: ")))   
| Ast.BidirVal(w1, v1, v2, w2) -> 
     (try                                (*sees if variable defined*)
          if find_var v1 env.var_types = Sast.Graph then 
              if find_var v2 env.var_types = Sast.Graph then
                let s_w1 = expr env w1 in
                if get_sexpr_type s_w1 = Sast.Num then
                  let s_w2 = expr env w2 in
                  if get_sexpr_type s_w2 = Sast.Num then 
                    Sast.BidirVal(expr env w1, v1, v2, expr env w2, Sast.Void)
                  else 
                    raise (Failure("undeclared variable: "))
                else
                  raise (Failure("undeclared variable: "))
              else 
                raise (Failure("undeclared variable: "))
          else 
            raise (Failure("undeclared variable: "))
      with
     | Not_found -> raise (Failure("undeclared variable: ")))  
| Ast.UndirVal(v1, v2, w) -> 
      (try                                (*sees if variable defined*)
            if find_var v1 env.var_types = Sast.Node then 
                if find_var v2 env.var_types = Sast.Node then 
                  let s_w = expr env w in
                  if get_sexpr_type s_w = Sast.Num then 
                      Sast.UndirVal(v1, v2, expr env w, Sast.Void)
                  else 
                    raise (Failure("undeclared variable: "))
                else 
                  raise (Failure("undeclared variable: "))
            else 
              raise (Failure("undeclared variable: "))
      with
        | Not_found -> raise (Failure("undeclared variable: ")))
| Ast.DirVal(v1, v2, w) -> 
    (try 
        if find_var v1 env.var_types = Sast.Node then 
          if find_var v2 env.var_types = Sast.Node then
            let s_w = expr env w in
            if get_sexpr_type s_w = Sast.Num then 
                Sast.DirVal(v1, v2, expr env w, Sast.Void)
            else 
              raise (Failure("undeclared variable: "))
          else 
            raise (Failure("undeclared variable: "))
        else 
          raise (Failure("undeclared variable: "))
      with
         | Not_found -> raise (Failure("undeclared variable: ")))
| Ast.NoOp(v) -> Sast.NoOp(v, Sast.Void)
| Ast.Noexpr -> Sast.Noexpr
in

(* convert an Ast.stmt object to Sast.stmt object *)
let rec stmt env = function
| Ast.Block(sl) -> Sast.Block(List.map (fun s -> stmt env s) sl)
| Ast.Expr(e) -> Sast.Expr(expr env e)
| Ast.Vdecl(dt, id) -> 
    (try 
        ignore (StringMap.find id !(List.hd env.var_types)); 
        raise (Failure ("variable already declared in local scope: " ^ id))
     with | Not_found -> (List.hd env.var_types) := StringMap.add id (str_to_type dt) !(List.hd env.var_types); (* add type map *)
                (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
          | Failure(f) -> raise (Failure (f) ) 
    );
    Sast.Vdecl(str_to_type dt, id)
| Ast.ListDecl(dt, id) -> (* Sast.ListDecl(str_to_type dt, v) *)
    let vtype = Sast.List(str_to_type dt) in
    (try 
        ignore (StringMap.find id !(List.hd env.var_types)); 
        raise (Failure ("variable already declared in local scope: " ^ id))
     with | Not_found -> (List.hd env.var_types) := StringMap.add id vtype !(List.hd env.var_types); (* add type map *)
                (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
          | Failure(f) -> raise (Failure (f))
    );
    Sast.Vdecl(vtype, id)
| Ast.DictDecl(dtk, dtv, id) -> (*Sast.DictDecl(str_to_type dtk, str_to_type dtv, v)*)
    let vtype = Sast.Dict(str_to_type dtk, str_to_type dtv) in
    (try
        ignore (StringMap.find id !(List.hd env.var_types)); 
        raise (Failure ("variable already declared in local scope: " ^ id))
     with | Not_found -> (List.hd env.var_types) := StringMap.add id vtype !(List.hd env.var_types);
                (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds);
          | Failure(f) -> raise (Failure (f) )
    ); 
    Sast.Vdecl(vtype, id)
| Ast.Assign(v, e) ->                     (* checks that the var and expression are of the same type, then converts to Sast.Assign *)
    let s_e = expr env e in             (* func rec until it knows datatype -- sast version of ast expr e *)
    let s_v = expr env v in
    let e_dt = get_sexpr_type s_e in     (* data type of that sast expr with function get_sexpr_type*)
    let v_dt = get_sexpr_type s_v in
    if not (v_dt = e_dt)
    then raise (Failure ("assignment expression not of type: " ^ (type_to_str v_dt) ))
    else Sast.Assign(s_v, s_e, Sast.Void)
| Ast.AccessAssign(e1, e2, e3) -> 
    let s_e1 = expr env e1 in             (* func rec until it knows datatype -- sast version of ast expr e *)
    let e1_dt = get_sexpr_type s_e1 in
    let s_e2 = expr env e2 in             (* func rec until it knows datatype -- sast version of ast expr e *)
    let e2_dt = get_sexpr_type s_e2 in
    let s_e3 = expr env e3 in
    let e3_dt = get_sexpr_type s_e3 in
    (try                                (*sees if variable defined*)
         (match e1_dt with 
           List(dt) -> 
              (match e2_dt with 
                 | Sast.Num -> 
                 if (e3_dt = dt) then
                 Sast.AccessAssign(s_e1, s_e2, s_e3, Sast.Void)
                else
                raise (Failure("AccessAssign: Assigning wrong type"))

                 | _ -> raise (Failure ("expr to access list should be Num")) 
              )
          | Dict(dk,dv) ->  
            if (e2_dt = dk) then
                if(e3_dt = dv) then
                Sast.AccessAssign(s_e1, s_e2, s_e3, Sast.Void)
              else
              raise(Failure("AccessAssign: mismatched Value data type"))
            else 
                raise (Failure("wrong type accessassign: Dict != Dict<?> "))
          | _ -> raise (Failure("must use Dict or List with access!"))
          )
     with
     | Not_found -> raise (Failure("undeclared variable: "))
    );                                      

| Ast.NodeDef(v, e) -> (* (node id, what goes inside parens) of item *)
    (try 
      let v_e = (find_var v env.var_types) in 
      let s_e = expr env e in 
      let e_dt = get_sexpr_type s_e in 
      if v_e = Sast.Node then
        Sast.NodeDef(v, s_e, e_dt)
      else raise (Failure ("Node Def failure"))
    with 
      | Not_found -> raise (Failure("Node Def failure")))
|  Ast.GraphDef(v, el) ->
    (try
      let s_el = List.map (expr env) el in
      ignore(check_graph_list env s_el);
      Sast.GraphDef(v, s_el)
    with 
     Not_found -> raise (Failure ("GraphDef issue")))

| Ast.Return(e) -> 
    (try  
        let s_e = expr env e in
       (match get_sexpr_type s_e with  
         | Sast.Node -> Sast.Return(s_e, Sast.Node)
         | Sast.Num -> Sast.Return(s_e, Sast.Num)
         | Sast.String -> Sast.Return(s_e, Sast.String)
         | Sast.Bool -> Sast.Return(s_e, Sast.Bool)
         | Sast.Graph -> Sast.Return(s_e, Sast.Graph)
         | Sast.List(s_dt) -> Sast.Return(s_e, Sast.List(s_dt))
         | Sast.Dict(dtk, dtv) -> Sast.Return(s_e, Sast.Dict(dtk, dtv)) 
         | Sast.Void -> Sast.Return(s_e, Sast.Void))
    with 
     Not_found -> raise (Failure ("return issue")))
| Ast.If(cond, s1, s2) -> 
    (try  
      let s_cond = expr env cond in
      if (get_sexpr_type s_cond) = Sast.Bool then 
        Sast.If(expr env cond, stmt env s1, stmt env s2)
      else 
        raise (Failure ("if issue"))
    with 
     Not_found -> raise (Failure ("return issue")))
| Ast.For(v, e, sl) -> 
  (* iterable expr must have var which already has been been declared *)
  let s_e = expr env e in 
  let e_dt = get_sexpr_type s_e in 
  (match e_dt with 
    | Sast.List(dt) ->
      (try 
        ignore(find_var v env.var_inds);
        ignore(raise (Failure ("'" ^ v ^ "' has already been declared")))
       with
       | Not_found -> ignore()
       | Failure(f) -> raise (Failure f)
      );
      (* add the temp var to the symbol table *)
      (List.hd env.var_types) := StringMap.add v dt !(List.hd env.var_types); (* add type map *)
      (List.hd env.var_inds) := StringMap.add v (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
      Sast.For(v, s_e, List.map (fun s -> stmt env s) sl)    
    | Dict(dtk, dtv) ->
      (try 
        ignore(find_var v env.var_inds);
        ignore(raise (Failure ("'" ^ v ^ "' has already been declared")))
       with
       | Not_found -> ignore()
       | Failure(f) -> raise (Failure f)
      );
      (* add the temp var to the symbol table *)
      (List.hd env.var_types) := StringMap.add v dtk !(List.hd env.var_types); (* add type map *)
      (List.hd env.var_inds) := StringMap.add v (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
      Sast.For(v, s_e, List.map (fun s -> stmt env s) sl)    
    | Graph -> 
      (try 
        ignore(find_var v env.var_inds);
        ignore(raise (Failure ("'" ^ v ^ "' has already been declared")))
       with
       | Not_found -> ignore()
       | Failure(f) -> raise (Failure f)
      );
      (* add the temp var to the symbol table *)
      (List.hd env.var_types) := StringMap.add v Sast.Node !(List.hd env.var_types); (* add type map *)
      (List.hd env.var_inds) := StringMap.add v (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
      Sast.For(v, s_e, List.map (fun s -> stmt env s) sl)

    | Node -> 
      (try 
        ignore(find_var v env.var_inds);
        ignore(raise (Failure ("'" ^ v ^ "' has already been declared")))
       with
       | Not_found -> ignore()
       | Failure(f) -> raise (Failure f)
      );
      (* add the temp var to the symbol table *)
      (List.hd env.var_types) := StringMap.add v Sast.Node !(List.hd env.var_types); (* add type map *)
      (List.hd env.var_inds) := StringMap.add v (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index mapping *)
      Sast.For(v, s_e, List.map (fun s -> stmt env s) sl)
    | _ -> raise(Failure("Trying to for loop an expr that doesnt return an iterable"))
  )
 
| Ast.While(cond, sl) -> 
    (try 
      let s_cond = expr env cond in 
      if (get_sexpr_type s_cond) = Sast.Bool then 
          Sast.While(expr env cond, List.map (fun s -> stmt env s) sl)
      else 
        raise (Failure ("while issue"))
    with 
     Not_found -> raise (Failure ("while issue")))
| Ast.Fdecl(func) ->  (*Fdecl of func_decl and *)
   (try
     (* add formal variables to local scope variable maps *)
    let fname = func.fname in
    let formals = List.map (fun (dt, v) -> (f_dt_to_type dt, v)) func.formals in
    let rtype = f_dt_to_type func.rtype in

  (* add this function to symbol table *)
  let dummy_func_obj = {
                        Sast.s_fname = fname;
                        Sast.s_rtype = rtype;
                        Sast.s_formals = [];
                        Sast.s_body = [] 
                      }
  in
  (List.hd env.func_obj) := StringMap.add fname dummy_func_obj !(List.hd env.func_obj);
  (List.hd env.func_inds) := StringMap.add fname (find_max_index !(List.hd env.func_inds)+1) !(List.hd env.func_inds); (* add index map *)
  
  let func_env = {
          var_inds = ref StringMap.empty :: env.var_inds;              (* var names to indices ex. x -> 1 so that we can just refer to it as v1 *)
          var_types =  ref StringMap.empty :: env.var_types;   (* maps a var name to its type  ex. x -> num *)
          func_inds =   env.func_inds;            (* func names to indices ex. x -> 1 so that we can just refer to it as f1 *)
          func_obj =    env.func_obj;
          return_type = rtype;                       (* what should the return type be of the current scope *)
  }  
  in

  let rec fmls_adder env = function
  | [] -> ignore()
  | hd :: tl -> 
      (List.hd env.var_types) := StringMap.add (snd hd) (fst hd) !(List.hd env.var_types);
      (List.hd env.var_inds) := StringMap.add (snd hd) (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index map *)
      ignore(fmls_adder env tl)
  in 
  ignore(fmls_adder func_env formals);
  
 let populated_fdecl = {
                        Sast.s_fname = func.fname;
                        Sast.s_rtype = rtype;
                        Sast.s_formals = formals;
                        Sast.s_body = List.map (fun s -> stmt func_env s) func.body 
                       }
 in
 (List.hd env.func_obj) := StringMap.add fname populated_fdecl !(List.hd env.func_obj); (* replace the dummy fobj with the evaluated one *)
 Sast.Fdecl(populated_fdecl)
  with 
     Not_found -> raise (Failure ("fdecl issue")))
in

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
