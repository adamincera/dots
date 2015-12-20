(* semantically checks dot sast and converts to c ast *)
open Ast
open Sast
open Translate


module StringMap = Map.Make(String)
(* module DataTypeMap = Map.Make(dataType) *)
type s_program = { s_globals : s_stmt list; s_main: s_stmt list; s_funcs : s_fdecl list; } 

(* 
  DEALING WITH AUTOMATIC RESULT VARS:

  Step 1: After every "let x = ...." where "translate_expr env ..." is called,
          create a variable to hold the name of the result variable from that call
          ex. let e1_result =  "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in

  Step 2: After the end of all "translate_expr env ..." calls (i.e. when that function
          is no longer called), create a new auto_var to hold the result of the current
          function's translation.

          ex. let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in

  Step 3: Output a Block([]) that contains:
          a. a Vdecl object for result_var
          b. the C code that corresponds to the current expr's translation
          c. an Assign call that assigns the result of part b. to result_var

          ex. for if the result of e1 is the result of your expression
               Block([Vdecl(..., result_var);
                       c_e1;
                       Assign(Id(..., result_var), Id(..., e1_result))
                     ])
*)

(* read in Sast program creat list of glbs by skipping fdecls  
program = { s_cmds : s_stmt list } 

    Block of s_stmt list
  | Expr of s_expr
  | Vdecl of dataType * string
  | NodeDef of string * s_expr * dataType (* (node id, type, item id) *)
  | Assign of string * s_expr * dataType                     (* x = 5; *)
  | Return of s_expr * dataType                         (* return x (dataType) *)
  | If of s_expr * s_stmt * s_stmt             (* if (boolean) stmt; *)
  | For of string * string * s_stmt list       (* temp var, iterable var, var decls, stmts *)
  | While of s_expr * s_stmt list              (* condition, var decls, stmt list *)
  | Fdecl of s_fdecl and 

  *) 

(* let prgm = List.rev program.s_cmds in *)

(*
  @param *implicit* := list of Sast.stmts to sort
  @param sifted := a struct that contains the globals, 
      regular stmts, and func decls that have been sorted so far
 *)
let get_list_type = function
 | Sast.List(dt) ->  dt
 | _ -> raise (Failure("wrong type: not a list")) 

let rec stmt_sifter sifted = function
| [] -> sifted
| hd :: tl -> (match hd with
               | Sast.Vdecl(dt, id) -> stmt_sifter {s_globals = hd :: sifted.s_globals; 
                                               s_main = sifted.s_main; 
                                               s_funcs = sifted.s_funcs} tl
               | Sast.Assign(v, e, dt) -> 
                  stmt_sifter {s_globals = sifted.s_globals; 
                               s_main = hd :: sifted.s_main; 
                               s_funcs = sifted.s_funcs} tl
               | Sast.Expr(e) -> 
                  stmt_sifter {s_globals = sifted.s_globals; 
                               s_main = hd :: sifted.s_main; 
                               s_funcs = sifted.s_funcs} tl
               | Sast.NodeDef(id, e, dt) | Sast.Assign(Id(id, Sast.Node), e, dt) -> 
                   stmt_sifter {s_globals = sifted.s_globals; 
                   s_main = hd :: sifted.s_main; 
                   s_funcs = sifted.s_funcs} tl
               | Sast.AccessAssign(se1, se2, se3, dt) -> 
                   stmt_sifter {s_globals = sifted.s_globals; 
                   s_main = hd :: sifted.s_main; 
                   s_funcs = sifted.s_funcs} tl                
               | Sast.GraphDef(id, el) -> 
                   stmt_sifter {s_globals = sifted.s_globals; 
                   s_main = hd :: sifted.s_main; 
                   s_funcs = sifted.s_funcs} tl               
               | Sast.Return(e, dt) -> 
                   stmt_sifter {s_globals = sifted.s_globals; 
                   s_main = hd :: sifted.s_main; 
                   s_funcs = sifted.s_funcs} tl
               | Sast.Block(sl) -> 
                  let sifted_sl = stmt_sifter {s_globals = []; s_main = []; s_funcs = []} sl in
                  stmt_sifter {s_globals = sifted_sl.s_globals @ sifted.s_globals; 
                               s_main = Block(sifted_sl.s_main) :: sifted.s_main; 
                               s_funcs = sifted_sl.s_funcs @ sifted.s_funcs} tl
               | Sast.If(cond, s1, s2) -> 
                   let sifted_s1 = stmt_sifter {s_globals = []; s_main = []; s_funcs = []} [s1] in
                   let sifted_s2 = stmt_sifter {s_globals = []; s_main = []; s_funcs = []} [s2] in
                   let tmp = {s_globals = sifted_s1.s_globals @ sifted.s_globals; 
                              s_main = sifted_s1.s_main @ sifted.s_main;
                              s_funcs = sifted_s1.s_funcs @ sifted.s_funcs} in
                   stmt_sifter {s_globals = sifted_s2.s_globals @ tmp.s_globals; 
                                s_main = sifted_s2.s_main @ tmp.s_main;
                                s_funcs = sifted_s2.s_funcs @ sifted.s_funcs} tl
               | Sast.For (tmp, iter, sl) ->
                  let sifted_sl = stmt_sifter {s_globals = []; s_main = []; s_funcs = []} sl in
                  stmt_sifter {s_globals = sifted_sl.s_globals @ sifted.s_globals; 
                               s_main = For(tmp, iter, sifted_sl.s_main) :: sifted.s_main; 
                               s_funcs = sifted_sl.s_funcs @ sifted.s_funcs} tl
               | Sast.While (cond, sl) ->
                  let sifted_sl = stmt_sifter {s_globals = []; s_main = []; s_funcs = []} sl in
                  stmt_sifter {s_globals = sifted_sl.s_globals @ sifted.s_globals; 
                               s_main = While(cond, sifted_sl.s_main) :: sifted.s_main; 
                               s_funcs = sifted_sl.s_funcs @ sifted.s_funcs} tl
               | Sast.Fdecl(f) -> stmt_sifter {s_globals = sifted.s_globals; 
                           s_main = sifted.s_main; 
                           s_funcs = f :: sifted.s_funcs} tl
              )

type translation_env = {
            var_inds : int StringMap.t ref list;              (* var names to indices ex. x -> 1 so that we can just refer to it as v1 *)
            var_types : Sast.dataType StringMap.t ref list;   (* maps a var name to its type  ex. x -> num *)
            func_inds : int StringMap.t ref list;             (* func names to indices ex. x -> 1 so that we can just refer to it as f1 *)
            func_obj : Sast.s_fdecl StringMap.t ref list;  (* maps a func name to its return type *)
            return_type : Sast.dataType;                       (* what should the return type be of the current scope *)
    }

let mappings = [("ine", Sast.Node); ("oute", Sast.Node); ("value", Sast.Node); ("nodes", Sast.Graph)] 
let mem_vars =  List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty mappings

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
                | [] -> raise (Not_found )
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

let rec expr_type_str  = function
    | Sast.NumLiteral(v, dt) -> "NumLiteral"
    | Sast.StrLiteral(v, dt) -> "StrLiteral"
    | Sast.ListLiteral(el, dt) -> "ListLiteral"
    | Sast.DictLiteral(kvl, dt) -> "DictLiteral"
    | Sast.Boolean(v, dt) -> "Boolean"
    | Sast.Id(v, dt) -> "Id"
    | Sast.Binop(e1, op, e2, dt) -> "Binop"
    | Sast.Call(v, el, dt) -> "Call"
    | Sast.Access(v, e, dt) -> "Access"
    | Sast.MemberCall(v, m, el, dt) -> "MemberCall"
    | Sast.Undir(v1, v2, dt) -> "Undir"
    | Sast.Dir(v1, v2, dt) -> "Dir"
    | Sast.UndirVal(v1, v2, w, dt) -> "UndirVal"
    | Sast.DirVal(v1, v2, w, dt) -> "DirVal"
    | Sast.BidirVal(w1, v1, v2, w2, dt) -> "BidirVal"
    | Sast.NoOp(v, dt) -> "NoOp"
    | Sast.Noexpr -> "Noexpr"
    (* returns the datatype of an Sast expressions *)
let get_sexpr_type = function
    | Sast.NumLiteral(v, dt) -> dt
    | Sast.StrLiteral(v, dt) -> dt
    | Sast.ListLiteral(el, dt) -> dt
    | Sast.DictLiteral(kvl, dt) -> dt
    | Sast.Boolean(v, dt) -> dt
    | Sast.Id(v, dt) -> dt
    | Sast.Binop(e1, op, e2, dt) -> dt
    | Sast.Call(v, el, dt) -> dt
    | Sast.Access(v, e, dt) -> dt
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
    (* determines whether a num string is an Int or a Float *)
let num_type num_str = 
    let numregex = Str.regexp "-?[0-9]+$"
    in
    if Str.string_match numregex num_str 0 then Int else Float

let rec dt_to_ct = function
    | Sast.Num -> Float
    | Sast.String -> Cstring
    | Sast.Bool -> Int
    | Sast.Graph -> Void (* TODO *)
    | Sast.Node -> Node (* TODO *)
    | Sast.List(dt) -> List(dt_to_ct dt) (* TODO *)
    | Sast.Dict(dtk, dtv) -> Ptr(Ptr(Entry)) (* TODO *)
    | Sast.Void -> Void

    (* the meat of the compiler *)
    (* actually converts Sast objects into strings of C code *)
let translate (env, sast_prg) =
    

(*  Automatic Variables *)
(*  certain translations require creating vars automatically
    keep track of all auto vars created so far, so that we 
    don't repeat auto vars in C 
*)
(* let auto_cnt = ref 0  *)
(* let auto_cnt = ref StringMap.empty in *)

(* maps the given key to the next available int index
   returns the index/number that the key was mapped to
   
   Note: a key is a dots variable name
   ex. "key" : 3      := means that var "key" represents auto var "a3"
 *)
let create_auto env key dt = 
      (* let ind = (find_max_index !auto_cnt) + 1 in *)
      let ind = (find_max_index !(List.hd env.var_inds)+1) in
      let var_name = (match key with
         | "" -> "v" ^ string_of_int(ind)
         | _ -> key
      ) in
     (*  auto_cnt := StringMap.add var_name ind !auto_cnt; (* add new auto_var ref *) *)
      (List.hd env.var_types) := StringMap.add var_name dt !(List.hd env.var_types); (* add type map *)
      (List.hd env.var_inds) := StringMap.add var_name ind !(List.hd env.var_inds); (* add index map *)      
      ind
in

    (* 
   char str[50];
   int len;

   strcpy(str, "This is tutorialspoint.com");
   len = strlen(str);
    *)
let string_len c_v = 
  let cdt1 = Translate.get_cexpr_type c_v in
  if cdt1 = Cstring then
      let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.Num)) in
       (auto_var, Block([
              Vdecl(Int, auto_var);
              Expr(Assign(Id(Int, auto_var), 
                     Call(Int, "strlen", [c_v])))]))
  else 
    raise (Failure("only possible with string "))
  in

let string_concat c_v1 c_v2 = 
  let cdt2 = Translate.get_cexpr_type c_v2 in

  let len_c1 = ((string_len c_v1)) in 
  let len_c2 = ((string_len c_v2)) in 
  let len_new =  Assoc(Binop(Int, Id(Int, (fst len_c1)), Add, Id(Int,(fst len_c2)))) in

  let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.String)) in
    if cdt2 = Cstring then
   (auto_var, 
    Block([
      (snd len_c1);
      (snd len_c2);
      Vdecl(Cstring, auto_var);
      Expr(Assign(
             Id(Cstring, auto_var), 
             Call(Ptr(Void), 
                  "malloc", 
                 [Binop(Int, Call(Int, "sizeof", [Id(Void, "int")]),  Mult, len_new)])
            ));
      Expr(Call(Void,
           "strcpy",
           [Id(Cstring, auto_var);
            c_v2]));
      Expr(Call(Void,
           "strcat",
           [Id(Cstring, auto_var);
            c_v1]))
      ]))
 else 
    raise (Failure("only accesible for strings"))

 in

let string_of_stmt c_v = 
  let cdt = Translate.get_cexpr_type c_v in
  let s_dt  = Translate.type_to_str cdt in  
  let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.String)) in
  (match cdt with
      | Int ->
          (auto_var, Block([
              Vdecl(Cstring, auto_var);
              Expr(Assign(Id(Cstring, auto_var), 
                     Call(Ptr(Void), 
                          "malloc", 
                         [Binop(Int, 
                                Call(Int, "sizeof", [Id(Void, "char")]), 
                                Mult,
                                Literal(Int, "400"))
                         ] )
                    ));
              Call(Void,
                   "itoa",
                   [c_v;
                    Id(Cstring, auto_var);
                    Literal(Int,"10")
                   ])
          ]))
      | Float -> 
          (auto_var, Block([
                  Vdecl(Cstring, auto_var);
                    Expr(Assign(Id(Cstring, auto_var), 
                           Call(Ptr(Void), 
                                "malloc", 
                               [Binop(Int, 
                                      Call(Int, "sizeof", [Id(Void, "char")]), 
                                      Mult,
                                      Literal(Int, "400"))
                               ] )
                          ));
                     Expr(Call(Void, 
                          "sprintf",
                          [Id(Void, auto_var);
                            Literal(Cstring,"%d.%02u");
                            Cast(Int, c_v);
                            Cast(Int, 
                                (Binop(Float,
                                      (Binop(Float, c_v, Sub, Cast(Int, c_v))),
                                      Mult,
                                      Literal(Int, "100"))
                                ))
                          ]))
                    ]))
      | _ -> raise (Failure ("cannot convert type to cstring: " ^  s_dt)))
in
                  
(* char *snum = malloc(sizeof(char) * 256); 
    sprintf (string,"%d.%02u", (int) number, (int) ((number - (int) number ) * precision) );
*)

let rec translate_expr env = function 
    | Sast.NumLiteral(l, dt) -> 
        let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in
        Block([   Vdecl(Ptr(Float), result_var);
                    Expr(Assign(Id(Ptr(Float), result_var), 
                         Call(Ptr(Void), "malloc", [ Call(Int, "sizeof", [Id(Void, "float")] ) ])
                         ));
                  Expr(Assign( Deref(Float, Id( Ptr(Float), result_var)), Literal(Float, l)))
              ])
    | Sast.StrLiteral(l, dt) ->
        let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in
            Block([   
              (*create string*)
              Vdecl(Ptr(Cstring), result_var); (* char **result *)
              Expr(Assign(Id(Ptr(Cstring), result_var), 
                          Call(Ptr(Void), "malloc", [ Call(Int, "sizeof", [Id(Void, "char *")] ) ])
                          )
             ); (* *result = malloc(strlen(literal)) *)
             Expr(Assign(Deref(Cstring, Id(Ptr(Cstring), result_var)), 
                         Call(Ptr(Void), "malloc", 
                              [ Call(Int, "strlen", [Literal(Cstring, l)]) ]
                             )
                       )
              ); (* strcpy( *result, literal) *)
              Expr(Call(Ptr(Void),
                    "strcpy", [Deref(Cstring, Id(Ptr(Cstring), result_var)); 
                                   Literal(Cstring, l)
                                  ]
                        )
              )
            ]) 
    | Sast.ListLiteral(el, dt) -> 
        let c_dt = dt_to_ct dt in
        let elem_stype = get_list_type dt in
        let elem_ctype = dt_to_ct elem_stype in
        let enq_func = (match elem_stype with
                        | Num -> "num_add_back"
                        | String -> "string_add_back"
                        | Node -> "node_add_back"
                        | Graph -> "graph_add_back"
                        | _ -> raise (Failure("can not enqueue this datatype"))
                       ) in
        let temp_list = "v" ^ string_of_int(create_auto env "" (dt)) in (* the variable containing the list *)

        let rec build_enqueue ops = function
        | [] -> ops
        | hd :: tl -> 
            let elem_c = translate_expr env hd in (* translate list element being added *)
            let elem_result =  "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result of element translation *)
            let enq_call = Call(c_dt, enq_func, [Deref(c_dt, Id(Ptr(c_dt), temp_list));
                                                 Id(Ptr(elem_ctype), elem_result) (* element translation result *)
                                                ]
                          ) in
            build_enqueue ((elem_c, enq_call) :: ops) tl
        in
        let enq_calls = build_enqueue [] el in (* get calls for each element in the list *)

        let rec build_list stmts = function
        | [] -> stmts
        | hd :: tl -> 
            let en_stmt = Block([(fst hd);
                                 Expr(Assign(Deref(c_dt, Id(Ptr(c_dt), temp_list)),
                                             (snd hd)
                                     )
                                 )
                          ]) in
            build_list (en_stmt :: stmts) tl
        in
        let c_stmts = build_list [] enq_calls in (* combines the element translation and enqueue calls *)

        let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in (* will equal the temporary list created earlier *)

        Block(([Vdecl(Ptr(c_dt), temp_list);
                Expr(Assign(Id(Ptr(c_dt), temp_list), 
                            Call(Ptr(Void), "malloc", [ Call(Int, "sizeof", [Id(Void, "list_t *")]) ])
                ));
                Expr(Assign(Deref(c_dt, Id(Ptr(c_dt), temp_list)), Id(Void, "NULL")));
                Vdecl(Ptr(c_dt), result_var)
               ] 
              @ 
              (List.rev (Expr(Assign(Id(Ptr(c_dt), result_var), Id(Ptr(c_dt), temp_list))) 
               :: (List.rev c_stmts) 
              )))) (* add the assignment to the end of enqueue calls *)
        (* ListLiteral(dt_to_ct dt, List.map (fun f -> translate_expr env f) el) (* TODO *) *)

    | Sast.DictLiteral(kvl, dt) ->  
        DictLiteral(dt_to_ct dt, 
                    List.map (fun f -> (translate_expr env (fst f), translate_expr env (snd f))) kvl)(* TODO *)
    | Sast.Boolean(b, dt) -> 
        if b = Ast.True then Literal(Int, "1") else Literal(Int, "0")
    | Sast.Id(v, dt) -> 
            let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in
            let index = "v" ^ string_of_int(find_var v env.var_inds) in (* see if id exists, get the num index of the var *)
            let v_type = dt_to_ct dt in
            Block([
                      Vdecl(Ptr(v_type), result_var);
                      Expr(Assign(Id(Ptr(v_type), result_var), Ref(Ptr(v_type), Id(v_type, index) )))
                  ])
    | Sast.Binop(e1, op, e2, dt) ->
        let c_dt = dt_to_ct dt in
        let ce1 = translate_expr env e1 in
          let result_e1 = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result var of e1's translation *)
          let e1_cdt = dt_to_ct (get_sexpr_type e1) in    (*gets is the c data type of the expression*)
        let ce2 = translate_expr env e2 in
          let result_e2 = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result var of e1's translation *)
          let e2_cdt = dt_to_ct (get_sexpr_type e2) in 
        let cdt1 = Translate.get_cexpr_type ce1 in
        let cdt2 = Translate.get_cexpr_type ce2 in 
        let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in (* create a new auto_var to store THIS EXPR'S result *)
        let args = [Id(cdt1, result_e1); Id(cdt2, result_e2)] in
        let result_decl = Vdecl(Ptr(c_dt), result_var) in                (* declare this expr's result var *)
        let binop_func = 
            (match op with
              | Add -> 
                (match e1_cdt with
                  |  Float -> 
                      (match e2_cdt with
                        | Float -> Translate.Binop(Float, 
                                                  Deref(e1_cdt, Id(Ptr(e1_cdt), result_e1)), 
                                              op, Deref(e2_cdt, Id(Ptr(e2_cdt), result_e2)))
                        | Cstring -> 
                            let float_convert = string_of_stmt ce1 in 
                            Block(
                            [(snd float_convert) ;
                             translate_expr env (Sast.Binop(Id((fst float_convert), String), Add, e2, String))])
                        | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                      )
                  |  Cstring -> 
                      (match e2_cdt with
                        | Float -> 
                            let float_convert = string_of_stmt ce2 in 
                            Block([(snd float_convert) ;
                             translate_expr env (Sast.Binop(Id((fst float_convert), String), Add, e1, String))])
                        | Cstring ->  
                             let c_string = string_concat ce1 ce2 in 
                             Block([(snd c_string)])
                        | Int -> 
                            let int_convert = string_of_stmt ce2 in 
                            Block([(snd int_convert) ;
                             translate_expr env (Sast.Binop(Id((fst int_convert), String), Add, e1, String))])
                        | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                      )
                  |  Graph -> 
                      (match e2_cdt with
                        | Node -> 
                            let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.Graph)) in
                            let index = "v" ^ string_of_int(find_var auto_var env.var_inds) in
                            Vdecl(Ptr(Graph), index);

                            Block([Vdecl(Ptr(Graph), index);
                                   Expr(Assign(Id(Graph, index), Call(Void, "init_graph", [])));
                                   Expr(Call(Void, "add_node", [Id(Graph, index); ce2]));
                                   Expr(Assign(Id(Graph, index), Call(Graph, "plus", [ce1;ce2])));

                            ])
                        | Graph -> 
                            (* g1 = plus(g2, g3); *)
                            let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.Graph)) in
                            let index = "v" ^ string_of_int(find_var auto_var env.var_inds) in
                            Block([Vdecl(Ptr(Graph), index);
                                   Expr(Assign(Id(Graph, index), Call(Graph, "plus", [ce1;ce2])));

                                 ])
                        | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                      )
                  |  Node -> 
                      (match e2_cdt with
                        | Node -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                        | Graph -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                        | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                      )
                  |  List(dt) -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                  |  Int -> 
                      (match e2_cdt with 
                        | Cstring -> 
                            let int_convert = string_of_stmt ce1 in 
                            Block(
                            [(snd int_convert) ;
                             translate_expr env (Sast.Binop(Id((fst int_convert), String), Add, e2, String))])
                        | Int -> Translate.Binop(Int, 
                                                  Deref(e1_cdt, Id(Ptr(e1_cdt), result_e1)), 
                                              op, Deref(e2_cdt, Id(Ptr(e2_cdt), result_e2)))
                        | _ -> raise (Failure("invalid operation"))
                      )
                  |  _ -> raise (Failure("Invalid c type for + binop " ^ (Translate.type_to_str cdt2)))          
                )
              | Sub -> 
                (match e1_cdt with
                  |  Float -> Translate.Binop(Float, 
                                                  Deref(e1_cdt, Id(Ptr(e1_cdt), result_e1)), 
                                              op, Deref(e2_cdt, Id(Ptr(e2_cdt), result_e2)))
                  |  Graph -> 
                      (match cdt2 with
                        | Node -> Translate.Binop(cdt1, ce1, op, ce2) (* TODO *)
                        | Graph -> Translate.Binop(cdt1, ce1, op, ce2) (* TODO *)
                        | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                      )
                  | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                )
              | Mult | Div -> Translate.Binop(Float, ce1, op, ce2)
              | Equal | Neq -> 
              (* This one isn't complete, dict maps to what c type? confusion *)
                (match e1_cdt with
                  |  Float -> Translate.Binop(Float, 
                                                  Deref(e1_cdt, Id(Ptr(e1_cdt), result_e1)), 
                                              op, Deref(e2_cdt, Id(Ptr(e2_cdt), result_e2)))
                  |  Int -> Translate.Binop(Float, 
                                                  Deref(e1_cdt, Id(Ptr(e1_cdt), result_e1)), 
                                              op, Deref(e2_cdt, Id(Ptr(e2_cdt), result_e2)))
                  |  Cstring -> 
                        (* (strcmp(check,input) = 0) *)
                        let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.Num)) in 
                         Assign(Id(Int, auto_var), (Call(Int, "strcmp", [ce1;ce2])));
                  |  Graph -> 
                      (match e2_cdt with
                        | Node -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                        | Graph -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                        | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                      )
                  |  Node -> 
                      (match e2_cdt with
                        | Node -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                        | Graph -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                        | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                      )
                  |  List(dt) -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                  |  Void -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                  |  _ -> raise (Failure("Invalid c type for ==/!= binop"))     
                )
              | Less | Leq | Greater | Geq -> 
                (match e1_cdt with
                  | Float -> Translate.Binop(Float, 
                                                  Deref(e1_cdt, Id(Ptr(e1_cdt), result_e1)), 
                                              op, Deref(e2_cdt, Id(Ptr(e2_cdt), result_e2)))
                  | Int -> 
                      Translate.Binop(Int, 
                                                  Deref(e1_cdt, Id(Ptr(e1_cdt), result_e1)), 
                                              op, Deref(e2_cdt, Id(Ptr(e2_cdt), result_e2)))
                  | Long -> Translate.Binop(Long, 
                                                  Deref(e1_cdt, Id(Ptr(e1_cdt), result_e1)), 
                                              op, Deref(e2_cdt, Id(Ptr(e2_cdt), result_e2)))
                  |  Cstring -> 
                      let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.Num)) in 
                         Assign(Id(Int, auto_var), (Call(Int, "strcmp", [ce1;ce2])));
                  | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                )
              | LogAnd | LogOr -> Translate.Binop(Int,ce1,op,ce2)
            ) in
            Block([
                 ce1;
                 ce2;
                 result_decl;
                 Expr(Assign(Deref(c_dt, Id(Ptr(c_dt), result_var)), binop_func
                 ))(* store the result of Access in our result_var *)
            ]) 
    | Sast.Call(func_name, el, dt) -> 
        (
            match func_name with
            | "print" ->
                let rec print_builder elems = function
                | [] -> List.rev elems
                | hd :: tl -> 
                    let hd_type = get_sexpr_type hd in
                    let print_expr = translate_expr env hd in (* elem to print *)
                    let print_type = dt_to_ct hd_type in (* type of elem *)
                    let print_result = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* result of elem translation *)

                    (match hd_type with
                      | Num | String | Bool | Node -> 
                          print_builder (Block([print_expr;
                                                Expr(Call(Void, "printf", [Literal(Cstring, get_fmt_str print_type); 
                                                                          Deref(print_type, Id(Ptr(print_type), print_result))
                                                                          ]))
                                                ]) :: elems)
                                        tl
                      | List(dt) -> 
                          let elem_type = dt_to_ct dt in
                          let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.List(dt))) in
                          print_builder 
                            (
                                (* b/c of building the list up backwards,
                                   this list must be declared in reverse order

                                    // c translation:
                                    // print(num_list)
                                    list_t* auto;
                                    for (auto = num_list; auto; auto = auto->next) {
                                      print( *auto );
                                    } 
                                  *)
                                [
                                 For(Assign(Id(elem_type, auto_var), print_expr),
                                     Id(List(elem_type), auto_var),
                                     Assign(Id(elem_type, auto_var), Member(List(dt_to_ct dt), auto_var, "next")),
                                     [Expr(Call(Void, "f1", [Deref(elem_type, Member(elem_type, auto_var, "data"))]))]
                                 );
                                 Vdecl(List(dt_to_ct dt), auto_var)
                                ]
                               @ elems
                            )
                            tl
                      | Dict(dtk, dtv) -> 
                          let key_type = dt_to_ct dtk in
                          let val_type = dt_to_ct dtv in
                          let auto_hash = "v" ^ string_of_int(create_auto env "" (Sast.Num)) in
                          let auto_entry = "v" ^ string_of_int(create_auto env "" (Sast.Dict(dtk, dtv))) in
                          let auto_key = "v" ^ string_of_int(create_auto env "" dtk) in
                          let auto_first = "v" ^ string_of_int(create_auto env "" Sast.Num) in
                          let auto_val = "v" ^ string_of_int(create_auto env "" Sast.Num) in
                          (* build the print value statement for the specific key type *)
                          let call_stmt = (match dtk with
                                            | Num -> Call(Ptr(val_type), "get_num", [print_expr; Deref(key_type, Id(Float, auto_key))])
                                            | String ->Call(Ptr(val_type), "get_string", [print_expr; Deref(key_type, Id(Cstring, auto_key))])
                                            | Node -> Call(Ptr(val_type), "get_other", [print_expr; Ref(Ptr(Node), Id(Ptr(Node), auto_key))])
                                            | _ -> raise (Failure ("dict keys can't be of type: " ^ type_to_str dtk))
                                          ) in
                          (*
                            // C code: 
                            int i;
                            entry_t *temp;
                            void *key;
                            /* print "{"; */
                            int first = 1;
                            for(i = 0; i < TABLE_SIZE; i = i + 1) {
                                for(temp = d[i]; temp; temp = temp->next) {
                                    key = temp->key;
                                    if(first) {
                                        first = 0;
                                        /* print key, ": ", value */ 
                                    } else {
                                        /* print ", " , key, ": ", value */
                                    }
                                }
                            }
                           *)
                          print_builder 
                              (
                               [
                                For(Assign(Id(Int, auto_hash), Literal(Int, "0")),
                                    Binop(Int, Id(Int, auto_hash), Less, Id(Int, "TABLE_SIZE")),
                                    Assign(Id(Int, auto_hash), 
                                           Binop(Int, Id(Int, auto_hash), Add, Literal(Int, "1"))
                                         ),
                                    [For(Assign(Id(Ptr(Entry), auto_entry),
                                                Access(Ptr(Entry), print_expr, Id(Int, auto_hash))
                                               ),
                                         Id(Ptr(Entry), auto_entry),
                                         Assign(Id(Ptr(Entry), auto_entry),
                                                Member(Ptr(Entry), auto_entry, "next")),
                                         [Expr(Assign(Id(Ptr(key_type), auto_key),
                                                 Cast(Ptr(key_type), Member((Ptr(Void), auto_entry, "key")))
                                                ));
                                          Vdecl(val_type, auto_val);
                                          Expr(Assign(Id(val_type, auto_val),
                                                 Deref(val_type, 
                                                       Cast(Ptr(val_type), 
                                                            (* specific call for the key type *)
                                                            call_stmt
                                                           )
                                                      )
                                              ));
                                          If(Id(Int, auto_first),
                                             [Expr(Assign(Id(Int, auto_first),
                                                    Literal(Int, "0")
                                                   ))
                                             ],
                                             [
                                               (*translate_expr env (Sast.Call("print", [Sast.StrLiteral(",", Sast.String)], Sast.Void))*)
                                               Expr(Call(Void, "f1", [Literal(Cstring, ",")]))
                                             ]
                                            );
                                          (*translate_expr env (Sast.Call("print", [Id(auto_key, dtk)],Sast.Void))*)
                                          (* prints Num|String|Bool|Node TODO: handle if the key is a graph *)
                                          Expr(Call(Void, "f1", [Deref(key_type, Id(Ptr(key_type), auto_key))]));
                                          Expr(Call(Void, "f1", [Literal(Cstring, " : ")]));
                                          Expr(Call(Void, "f1", [Id(val_type, auto_val)]));

                                         ]
                                    )]
                                   );
                                Expr(Assign(Id(Int, auto_first), Literal(Int, "1")));
                                Vdecl(Int, auto_first);
                                Vdecl(Ptr(key_type), auto_key);
                                Vdecl(Ptr(Entry), auto_entry);
                                Vdecl(Int, auto_hash)
                               ]
                               @ elems
                              ) 
                              tl (*TODO*)
                      | Graph -> print_builder (Nostmt :: elems) tl (*TODO*)
                      | Void -> raise (Failure "stop trying to print Void -- it's not gonna happen")
                    )
                in
                Block( print_builder [] el (* TODO *) )
            | "len" ->
                    let arg = List.hd el in
                    let arg_type = get_sexpr_type arg in
                    let arg_ctype = dt_to_ct arg_type in
                    let c_arg = translate_expr env arg in
                    let c_dt = dt_to_ct dt in
                    let arg_result = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in
                    let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in (* create a new auto_var to store THIS EXPR'S result *)
                    let result_decl = Vdecl(c_dt, result_var) in (* declare this expr's result var *)
                    let call = (match arg_type with
                    | List(dt) -> Call(Int, "list_len", [Deref(arg_ctype, Id(Ptr(arg_ctype), arg_result))])
                    | Dict(dtk, dtv) -> Call(Int, "dict_len", [Deref(arg_ctype, Id(Ptr(arg_ctype), arg_result))])
                    | _-> raise (Failure "len not implemented for this type")
                     
                    ) in
                    Block([
                        c_arg;
                        result_decl;
                        Assign(Id(c_dt, result_var), call);
                    ])
            | _ -> 
                let cel = List.map (translate_expr env) el in
                let index = "f" ^ string_of_int(find_var func_name env.func_inds) in
                Call(dt_to_ct dt, index, cel)
        )
            
    | Sast.Access(e1, e2, dt) -> 
        let c_dt = dt_to_ct dt in
        let e1_dt = get_sexpr_type e1 in
        let e2_dt = get_sexpr_type e2 in
        let c_e1 = translate_expr env e1 in (* translate e1 to c *)
          let result_e1 = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result var of e1's translation *)
        let c_e2 = translate_expr env e2 in (* translate e2 to c *)
          let result_e2 = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result var of e2's translation *)
        
        let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in (* create a new auto_var to store THIS EXPR'S result *)
        let result_decl = Vdecl(Ptr(c_dt), result_var) in (* declare this expr's result var *)
        let args = [Id(dt_to_ct e1_dt, result_e1); Id(dt_to_ct e2_dt, result_e2)] in (* specific to access function calls *)
        let access_call = (match e1_dt with
                      | List(dt) -> 
                              Call(Ptr(Void), "list_access", args)
                      | Dict(dtk, dtv) ->
                              let c_dtk = dt_to_ct dtk in
                              (match c_dtk with
                              | Float -> Call(Ptr(Void), "get_num", args)
                              | Cstring -> Call(Ptr(Void), "get_string", args)
                              | Graph -> Call(Ptr(Void), "get_graph", args)
                              | Node -> Call(Ptr(Void), "get_node", args)
                              | _ -> raise(Failure("unsupported dict type"))
                              )
                      | _ -> raise(Failure("unsupported access"))
                    ) in (* evaluate an Access expression *)
        Block([
                 c_e1;
                 c_e2;
                 result_decl;
                 Assign(Deref(c_dt, Id(Ptr(c_dt), result_var)), Cast(Ptr(c_dt), access_call))(* store the result of Access in our result_var *)
              ])

        (*     let index = "v" ^ string_of_int(find_var v env.var_inds) in
            let ce = translate_expr env e in
            Access(dt_to_ct dt, index, ce)
            list_t *l3; //
            g2[1];

            l3 = (num_add_back(l3, 1.24));
             *)
    | Sast.MemberCall(e, f, el, dt) -> 
        let c_dt = dt_to_ct dt in
        
        let c_e = translate_expr env e in (* translate e1 to c *)
        let result_e = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result var of e1's translation *)
        let e_dt = get_sexpr_type e in 
        let c_id = Id(dt_to_ct e_dt, result_e) in  

         let result_var = "v" ^ string_of_int(create_auto env "" (dt)) in (* create a new auto_var to store THIS EXPR'S result *)
         let result_decl = Vdecl(dt_to_ct e_dt, result_var) in (* declare this expr's result var *)
         let final_result = Id(dt_to_ct e_dt, result_var) in

            (match f with
                  | "enqueue" | "push" ->
                   let e_list_type = (get_list_type e_dt) in
                      let suffix = (if f = "enqueue" then "back" else "front") in
                      let func_name = 
                        (match e_list_type with
                        | Num -> "num_add_" ^ suffix
                        | String -> "string_add_" ^ suffix
                        | Node -> "node_add_" ^ suffix
                        | Graph -> "graph_add_" ^ suffix
                        | _ -> raise (Failure("can not enqueue this datatype"))) in
                          
                      let elem_c = (translate_expr env (List.hd el)) in 
                      let arg_e = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in
                      let arg_dt = get_cexpr_type elem_c in 
                      let arg_id = Id(arg_dt, arg_e) in 
                      

                           (*let e1_cdt = dt_to_ct (get_sexpr_type e1) in *)
                      Block([
                        c_e;
                        elem_c;
                        result_decl; 
                        Expr(Assign(final_result, 
                                Call(c_dt, func_name, 
                                           [Deref((dt_to_ct e_dt), c_id); 
                                            arg_id])
                        ))
                             (* store the result of Access in our result_var *)
                        ]) 
                  | "dequeue" | "pop" -> 
                           Block([
                                c_e;
                                result_decl; 
                                Expr(Assign(final_result, 
                                        Call(c_dt, "pop", 
                                                   [Deref((dt_to_ct e_dt), c_id)])
                                ))
                             (* store the result of Access in our result_var *)
                        ])
                  | "ine" ->                    
                      Block([Expr(Assign(Id(Ptr(Entry),result_e), Member(Ptr(Entry), result_var, "out")))])
                  | "oute" -> 
                     Block([
                         Expr(Assign(Id(Ptr(Entry),result_e), Member(Ptr(Entry), result_var, "out")));
                         c_e;
                         result_decl;
                         Expr(Assign(Deref(c_dt, Id(Ptr(c_dt), result_var)), Cast(Ptr(c_dt), 
                                Call(c_dt, "pop", [c_e] ))))
                         (* store the result of Access in our result_var *)
                    ]) 
      (* Node * n; returns value  snippet n= n.val() datamember of *t Node->data  *)
                  | "val" -> 
                      Block([
                         Expr(Assign(Id(Ptr(Entry),result_e), Member(Ptr(Entry), result_var, "data")));
                         c_e;
                         result_decl;
                         Expr(Assign(Deref(c_dt, Id(Ptr(c_dt), result_var)), Cast(Ptr(c_dt), 
                            Call(c_dt, "pop", [c_e] )
                          )))
                         (* store the result of Access in our result_var *)
                    ]) 
              | _ -> raise (Failure("not enqueue"))) 
    | Sast.Undir(v1, v2, dt) -> 
            let v1_index = "v" ^ string_of_int (find_var v1 env.var_inds) in
            let v2_index = "v" ^ string_of_int (find_var v2 env.var_inds) in
            Call(Void, "connect_undir", [Id(Ptr(Node), v1_index); Id(Ptr(Node), v2_index)])
    | Sast.Dir(v1, v2, dt) ->
            let v1_index = "v" ^ string_of_int (find_var v1 env.var_inds) in
            let v2_index = "v" ^ string_of_int (find_var v2 env.var_inds) in
            Call(Void, "connect_dir", [Id(Ptr(Node), v1_index); Id(Ptr(Node), v2_index)])
    | Sast.UndirVal(v1, v2, w, dt) -> 
            let v1_index = "v" ^ string_of_int (find_var v1 env.var_inds) in
            let v2_index = "v" ^ string_of_int (find_var v2 env.var_inds) in
            let w_c = translate_expr env w in
            let w_result = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in
            let w_deref = Deref(dt_to_ct (get_sexpr_type w), Id(Ptr(dt_to_ct
            (get_sexpr_type w)), w_result)) in
            Call(Void, "connect_undir_weighted", [Id(Ptr(Node), v1_index);
            Id(Ptr(Node), v2_index); w_deref])
    | Sast.DirVal(v1, v2, w, dt) -> Nostmt (* TODO *)
    (*
            let v1_index = "v" ^ string_of_int (find_var v1 env.var_inds) in
            let v2_index = "v" ^ string_of_int (find_var v2 env.var_inds) in
            Call(Void, "connect_undir", [Id(Ptr(Node), v1_index); Id(Ptr(Node), v2_index)])
*)
    | Sast.BidirVal(w1, v1, v2, w2, dt) -> Nostmt (* TODO *)
    | Sast.NoOp(s, dt) -> Nostmt (* TODO *)
    | Sast.Noexpr -> Nostmt
            in
let rec translate_stmt env = function 
    | Sast.Block(sl) -> 
            let csl = List.map (translate_stmt env) sl in
            Block(csl)
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
                               (*  Expr(Assign(Id(Graph, index), Call(Void, "init_graph", []))) *)
                               ]) (* C: graph_t *g1 = init_graph(); *)
              | Node -> (* Block([Vdecl(Ptr(Node), index); 
                               Expr(Assign(Id(Node, index), Call(Void, "init_node", [Literal(Cstring, "")])))
                              ]) *) (* C: node_t *x = init_node(""); *)
                        Block([Vdecl(Ptr(Node), index);])
              | List(dt) -> Vdecl(List(dt_to_ct dt), index) (* C: list_t *x; *)
              | Dict(dtk, dtv) -> Vdecl(Ptr(Ptr(Entry)), index) (* TODO *)
              | Void -> raise (Failure ("should not be using Void as a datatype"))
            )
    | Sast.Assign(v, e, dt) ->
            let ce = translate_expr env e in
            let e_result = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in
            let cv = translate_expr env v in
            let v_result = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in
            let e_type = get_sexpr_type e in
            let v_type = get_sexpr_type v in
            let var_type = dt_to_ct v_type in
            (*let index = "v" ^ string_of_int(find_var v  = get_sexpr_type KEYYY HOSANNA
            ) in*)
            (*let auto_var = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in*)
            (*(match var_type with
              figure out what the expr actually is if
              its a binop then we get the automatic variable it returns and set that equal to your 
              current variable. Var find Max index which will the last automatic variable
              (find_max_index !(List.hd env.var_inds)+1)
            e --> block (Binop --> translate --> find max -> index =  autovar )
            Expr(Assign(
                     Id(Int, auto_var), 
                     Id(Int, auto_var))
            | Num | String | Bool | Node | Void -> Expr(Assign(Id((dt_to_ct var_type), index), ce))
            | List(dt) -> Expr(Assign(Id((dt_to_ct var_type), index), ce)) (* TODO *)   
            | Dict(dtk, dtv) -> Expr(Assign(Id((dt_to_ct var_type), index), ce)) (* TODO *)
            | Graph -> Expr(Assign(Id((dt_to_ct var_type), index), Call(Graph, "copy", [ce]))) 
            ) *)
(*
            ( match e with 
              | Binop(e1, op, e2, dt) -> 
                  Block([ce;
                         (*Expr(Assign(Id((dt_to_ct var_type), index), Id((dt_to_ct var_type), auto_var)))]) *)
                         Expr(Assign(cv, Id((dt_to_ct var_type), auto_var)))])
              | StrLiteral(s1, dt) | NumLiteral(s1, dt) | Id(s1, dt) -> (* Expr(Assign(Id((dt_to_ct var_type), index), ce)) *)
                  Expr(Assign(cv, ce))
              | DictLiteral(tl, dt) -> Expr(Assign(cv, Id(Void, "NULL"))) (* TODO *)
              | _ -> raise (Failure("Assign don't work like that ")))
*)
(*
            (match var_type with
            | Dict(dtk, dtv) -> 
                    Block([
                        ce;
                        cv;
                        Assign(Id(var_type, v_result), Call(var_type,
                        "init_dict", []))
                    ])

            | _ ->
                    *)
                Block([
                    ce; (* translation of assignment *)
                    cv; (* translation of asignee *)
                    Expr(Assign(Deref(var_type, Id(Ptr(var_type), v_result)), Deref(var_type, Id(Ptr(var_type), e_result))))
                  ])

            
                    (*         if not( (find_var v env.var_types) = get_sexpr_type e)
        then raise (Failure ("assignment expression not of type: " ^ type_to_str (find_var v env.var_types) ))
        else (translate_expr env (Sast.Id(v, dt))) ^ " = " ^ (translate_expr env e) *)

                    (* | Sast.AssignList(v, el) -> Expr(Noexpr)
                    | Sast.DictAssign(k, v) -> Expr(Noexpr) 
           Block([Vdecl(Ptr(dt_to_ct dt), auto_var);
           Cast(Ptr(var_type), Call("malloc", [Call("sizeof", type_to_str var_type)]))
                         ]) *)
    | Sast.AccessAssign(e1, e2, e3, dt) -> (* check access first *)
        let e1_dt = get_sexpr_type e1 in
        let e2_dt = get_sexpr_type e2 in
        let e3_dt = get_sexpr_type e3 in
        let c_e1 = translate_expr env e1 in
          let result_e1 = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result var of e1's translation *)
          let e1_deref = Deref(dt_to_ct e1_dt, Id(Ptr(dt_to_ct e1_dt), result_e1)) in
        let c_e2 = translate_expr env e2 in
          let result_e2 = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result var of e1's translation *)
          let e2_deref = Deref(dt_to_ct e2_dt, Id(Ptr(dt_to_ct e2_dt), result_e2)) in
        let c_e3 = translate_expr env e3 in
          let result_e3 = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* get result var of e1's translation *)
          let e3_deref = Deref(dt_to_ct e3_dt, Id(Ptr(dt_to_ct e3_dt), result_e3)) in
        let args = [c_e1; c_e2; c_e3] in
        let call = (match e1_dt with
                        | List(dt) -> 
                          if (e2_dt = Sast.Num) then 
                            if (e3_dt = dt) then
                              let c_dt = dt_to_ct dt in 
                              (match c_dt with
                                | Float -> Call(Ptr(Void), "num_index_insert", args)
                                | Cstring -> Call(Ptr(Void), "string_index_insert", args)
                                | Graph -> Call(Ptr(Void), "graph_index_insert", args)
                                | Node -> Call(Ptr(Void), "node_index_insert", args)
                                | _ -> raise(Failure("unsupported list type"))
                              )
                            else
                              raise(Failure("accessassign: expr right of = is not same type as list"))
                          else
                            raise(Failure("AccessAssign: assign expr on left is wrong for list"))
                        | Dict(dtk, dtv) ->
                          if (e2_dt = dtk) then
                            if (e3_dt = dtv) then
                              let c_dtk = dt_to_ct dtk in
                              let c_dtv = dt_to_ct dtv in
                              (* try to get rid of problem with & *)
                              let auto_var = "v" ^ string_of_int(create_auto env "" dtv) in
                              (match c_dtk with
                              | Float -> Block([Vdecl(c_dtv, auto_var);
                                         Expr(Assign(Id(c_dtv, auto_var), e3_deref));
                                         Expr(Call(Ptr(Void), "put_num", 
                                         [e1_deref; e2_deref; Cast(Ptr(Void),Ref(c_dtv, Id(c_dtv,auto_var)))]))])
                              | Cstring -> Block([Vdecl(c_dtv,auto_var);
                                           Expr(Assign(Id(c_dtv, auto_var), e3_deref));
                                           Expr(Call(Ptr(Void), "put_string", 
                                           [e1_deref; e2_deref; Cast(Ptr(Void),Ref(c_dtv, Id(c_dtv,auto_var)))]))])
                              | Node -> let auto_var2 = "v" ^ string_of_int(create_auto env "" dtk) in
                                        Block([Vdecl(c_dtk,auto_var2);
                                        Expr(Assign(Id(c_dtk,auto_var2),
                                             Deref(dt_to_ct e2_dt,
                                             Id(Ptr(dt_to_ct e2_dt),
                                             result_e2))));
                                        Vdecl(c_dtv,auto_var);
                                        Expr(Assign(Id(c_dtv,auto_var), e3_deref));
                                        Expr(Call(Ptr(Void), "put_node", 
                                        [e1_deref; Cast(Ptr(Void),Ref(c_dtk, Id(c_dtk,auto_var2))); Cast(Ptr(Void),Ref(c_dtv, Id(c_dtv,auto_var)))]))])
                              | _ -> raise(Failure("unsupported dict type"))
                              )
                            else
                              raise(Failure("accessassign: expr right of = is not dict value type"))
                          else
                            raise(Failure("accessassign: assign expr on left for dict is wrong"))
                (*                 let c_dtk = dt_to_ct dtk in
                                (match c_dtk with
                                | Float -> Call(Ptr(Void), "get_num", args)
                                | Cstring -> Call(Ptr(Void), "get_string", args)
                                | Graph -> Call(Ptr(Void), "get_graph", args)
                                | Node -> Call(Ptr(Void), "get_node", args)
                                | _ -> raise(Failure("unsupported dict type"))
                                ) *)
                        | _ -> raise(Failure("unsupported access"))
                    ) in
        Block([
            c_e1;
            c_e2;
            c_e3;
            call;
        ])

    | Sast.Return(e, dt) -> Translate.Return( translate_expr env e)           
    | Sast.NodeDef (id, s, dt) -> 
        let index = "v" ^ string_of_int(find_var id env.var_inds) in
        (match s with
          | Sast.Noexpr ->                                                 
            Block([Expr(Assign(Id(Node, index), Call(Void, "init_node", [Literal(Cstring, "")]))); 
              Expr(Assign(Member(Ptr(Void), index, "data"), Literal(Cstring,"")))])
          | _ -> 
            Block([Expr(Assign(Id(Node, index), Call(Void, "init_node", [Literal(Cstring, "")])));
              Expr(Assign(Member(Ptr(Void), index, "data"), translate_expr env s))])
        )     
    | Sast.GraphDef(id, sl) ->
        let index = "v" ^ string_of_int(find_var id env.var_inds) in
        let graph_shit = [Expr(Assign(Id(Graph, index), Call(Void, "init_graph", [])))] in
        Block(graph_shit @ List.map (fun f -> Expr(translate_expr env f)) sl)  
    | Sast.While (cond, sl) -> 
        let c_cond = translate_expr env cond in
        let csl = List.map (translate_stmt env) sl in
        While(c_cond, csl)                                    
    | Sast.If (cond, s1, s2) ->
        let c_cond = translate_expr env cond in 
        let c_s1 =  translate_stmt env s1 in
        let c_s2 =  translate_stmt env s2 in 
        If (c_cond, [c_s1], [c_s2])
    | Sast.For (key, iter, sl) ->
            let iter_stype = get_sexpr_type iter in
            let iter_ctype = dt_to_ct iter_stype in
            let c_iter = translate_expr env iter in (* evaluate the iterable *)
            let iter_result = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in (* result of iterable *)
            
            let loop_var = "v" ^ string_of_int(create_auto env key (Sast.Void)) in
            let key_var = "v" ^ string_of_int(create_auto env key iter_stype) in (* the temp var in "for x in iterable " *)
            (*let index = "v" ^ string_of_int (find_var iter env.var_inds) in*)
            
            (*let iter_type = (find_var iter env.var_types) in*)

            let csl = List.map (translate_stmt env) sl in
            Block(
              (*
            [Vdecl(Ptr(dt_to_ct iter_type), auto_index); 
             Expr(Assign(Id(dt_to_ct iter_type, auto_index), translate_expr env iter))
            ]
            @
          *)
            c_iter :: 
            [
                (match iter_stype with
                  | List(dt) -> 
                      let key_var_type = dt_to_ct dt in
                      let iter_deref = Deref(iter_ctype, Id(Ptr(iter_ctype), iter_result)) in
                      Block([Vdecl(key_var_type, key_var); (*  *)
                             Vdecl(iter_ctype, loop_var);
                             For(Assign(Id(iter_ctype, loop_var), iter_deref),
                                 Id(iter_ctype, loop_var),
                                 Assign(Id(iter_ctype, loop_var), Member(iter_ctype, loop_var, "next")),

                                 Expr(Assign(Id(key_var_type, key_var), 
                                             Deref(key_var_type, Cast(Ptr(key_var_type), Member(Ptr(Void), loop_var, "data")))
                                      )
                                 )
                                 :: csl
                            )
                      ])
                  | Dict(dtk, dtv) -> 
                          let iter_deref = Deref(Ptr(Entry), Id(Ptr(Ptr(Entry)), iter_result)) in
                          let int_var = "v" ^ string_of_int(create_auto env "" (Sast.Num)) in
                          Block([Vdecl(Int, int_var); 
                                 Vdecl(Ptr(Entry), loop_var);
                                 Vdecl(Ptr(Void), key_var); 
                                 For(Assign(Id(Int, int_var), Literal(Int, "0")),
                                     Binop(Int, Id(Int, int_var), Ast.Less, Id(Int, "TABLE_SIZE")),
                                     Assign(Id(Int, int_var), 
                                            Binop(Int, Id(Int, int_var), Ast.Add,
                                                  Literal(Int, "1"))),
                                     [For(Assign(Id(Ptr(Entry), loop_var), Access(Entry, Assoc(iter_deref), Id(Int, int_var))), 
                                          Id(Entry, loop_var),
                                          Assign(Id(Ptr(Entry), loop_var), Member(Entry, loop_var, "next")),
                                          ( Expr(Assign(Id(Ptr(Void), key_var), 
                                                        Member(Ptr(Void), loop_var, "key")
                                                )
                                            )
                                            :: List.map (translate_stmt env) sl
                                          )
                                     )]
                                )
                          ])
                  | Node -> 
                      let iter_deref = Deref(Node, Id(Ptr(Node), iter_result)) in
                      Block([Vdecl(Ptr(Node), key_var); 
                             Expr(Assign(Id(Ptr(Node), key_var), iter_deref))
                      ] @ csl)
                  | Graph -> 
                      let iter_deref = Deref(Graph, Id(Ptr(Graph), iter_result)) in
                      Block([Vdecl(Node, key_var);
                                    Vdecl(Entry, loop_var); 
                                    For(Assign(Id(Entry, loop_var), Member(Entry, "*" ^ iter_result, "nodes")),
                                      Id(Entry, loop_var),
                                      Assign(Id(Entry, loop_var), Member(Entry, loop_var, "next")),
                                      csl
                                      )
                                   ])
                  | _ -> raise (Failure("for loop iter is not iterable"))
                )
            ])

    | Sast.Fdecl (func) -> Nostmt
                    
                    (* ***** TODO *********)
                    (*
                    { crtype = Translate.Void;
                    cfname = "empty";
                    cformals = [(Int, "argc"); (Ptr(Cstring), "argv")];
                    cbody = []}
                  *)
                  
and
translate_fdecl env func = (
    (* add the parameter names to the env *)
    let formals = func.s_formals in
    let rtype = func.s_rtype in

    (* add formal variables to local scope variable maps *)
    let map_builder fmls m = (List.map (fun f -> m := (StringMap.add (snd f) (fst f) !m); "") formals) in
    let types_map = ref StringMap.empty in
      ignore (map_builder formals types_map);
    let fml_inds = enum 1 1 (List.map (fun f -> (snd f)) formals) in
    let inds_map = ref (string_map_pairs StringMap.empty fml_inds) in

    let func_env = {
      var_inds = inds_map :: env.var_inds;              (* var names to indices ex. x -> 1 so that we can just refer to it as v1 *)
      var_types =  types_map :: env.var_types;   (* maps a var name to its type  ex. x -> num *)
      func_inds =   ref StringMap.empty :: env.func_inds;            (* func names to indices ex. x -> 1 so that we can just refer to it as f1 *)
      func_obj = ref StringMap.empty :: env.func_obj;
      return_type = rtype;                       (* what should the return type be of the current scope *)
    }  in

    {crtype = dt_to_ct func.s_rtype; 
     cfname = "f" ^ string_of_int(find_var func.s_fname env.func_inds); 
     cformals = List.map (fun f -> (dt_to_ct (fst f), "v" ^ string_of_int(find_var (snd f) func_env.var_inds))) func.s_formals;
     cbody = (List.map (fun f -> translate_stmt func_env f) func.s_body)}
)
in

(* convert the sast_prg to cast_prg *)
let global_vars = List.map (fun f -> translate_stmt env f) sast_prg.s_globals in 
let main_func = {crtype = Translate.Int;
                 cfname = "main";
                 cformals = [(Int, "argc"); (Ptr(Cstring), "argv")];
                 cbody = List.map (fun f -> translate_stmt env f) sast_prg.s_main}
in
let cfunc_list = List.map (fun f -> translate_fdecl env f) sast_prg.s_funcs in
{globals = global_vars; cfuncs = List.rev (main_func :: List.rev cfunc_list)}

let print_bindings m =
    let bindings = StringMap.bindings m in
    let rec printer = function
        | [] -> print_endline("")
        | (k, v)::tl -> print_endline(k ^ string_of_int(v)) ; printer tl
    in
    printer bindings
