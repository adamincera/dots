(* semantically checks dot sast and converts to c ast *)
open Ast
open Sast
open Translate

module StringMap = Map.Make(String)
(* module DataTypeMap = Map.Make(dataType) *)

type s_program = { s_globals : s_stmt list; s_main: s_stmt list; s_funcs : s_fdecl list; } 

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
               | Sast.AccessAssign(se1, se2, dt) -> 
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

    (* returns the datatype of an Sast expressions *)
let get_expr_type = function
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
    | Sast.Dict(dtk, dtv) -> Void (* TODO *)
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
  let cdt1 = Translate.get_expr_type c_v in
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
  let cdt2 = Translate.get_expr_type c_v2 in

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
  let cdt = Translate.get_expr_type c_v in
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
        Literal(Float, l)
    | Sast.StrLiteral(l, dt) -> 
        Literal(Cstring, l)
    | Sast.ListLiteral(el, dt) -> 
        ListLiteral(dt_to_ct dt, List.map (fun f -> translate_expr env f) el) (* TODO *)
    | Sast.DictLiteral(kvl, dt) ->  
        DictLiteral(dt_to_ct dt, List.map (fun f -> (translate_expr env (fst f), translate_expr env (snd f))) kvl)(* TODO *)
    | Sast.Boolean(b, dt) -> 
        if b = Ast.True then Literal(Int, "1") else Literal(Int, "0")
    | Sast.Id(v, dt) -> 
            let index = "v" ^ string_of_int(find_var v env.var_inds) in (* see if id exists, get the num index of the var *)
            Id(dt_to_ct dt, index) 
    | Sast.Binop(e1, op, e2, dt) ->
        let ce1 = translate_expr env e1 in
        let ce2 = translate_expr env e2 in
        let cdt1 = Translate.get_expr_type ce1 in
        let cdt2 = Translate.get_expr_type ce2 in 
        (match op with
          | Add -> 
            (match cdt1 with
              |  Float ->
                  (match cdt2 with
                    | Float -> Translate.Binop(Float, ce1, op, ce2)
                    | Cstring -> 
                        let float_convert = string_of_stmt ce1 in 
                        Block(
                        [(snd float_convert) ;
                         translate_expr env (Sast.Binop(Id((fst float_convert), String), Add, e2, String))])
                    | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                  )
              |  Cstring -> 
                  (match cdt2 with
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
                  (match cdt2 with
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
                  (match cdt2 with
                    | Node -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                    | Graph -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                    | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                  )
              |  List(dt) -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
              |  Int -> 
                  (match cdt2 with 
                    | Cstring -> 
                        let int_convert = string_of_stmt ce1 in 
                        Block(
                        [(snd int_convert) ;
                         translate_expr env (Sast.Binop(Id((fst int_convert), String), Add, e2, String))])
                    | Int -> Translate.Binop(Int, ce1, op, ce2)
                    | _ -> raise (Failure("invalid operation"))
                  )
              |  _ -> raise (Failure("Invalid c type for + binop"))          
            )
          | Sub -> 
            (match cdt1 with
              |  Float -> Translate.Binop(cdt1, ce1, op, ce2)
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
            (match cdt1 with
              |  Float -> Translate.Binop(Float, ce1, op, ce2)
              |  Int -> Translate.Binop(Int, ce1, op, ce2)
              |  Cstring -> 
                    (* (strcmp(check,input) = 0) *)
                    let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.Num)) in 
                     Assign(Id(Int, auto_var), (Call(Int, "strcmp", [ce1;ce2])));
              |  Graph -> 
                  (match cdt2 with
                    | Node -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                    | Graph -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                    | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                  )
              |  Node -> 
                  (match cdt2 with
                    | Node -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                    | Graph -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
                    | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
                  )
              |  List(dt) -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
              |  Void -> Translate.Binop(cdt1, ce1, op, ce2) (*TODO*)
              |  _ -> raise (Failure("Invalid c type for ==/!= binop"))     
            )
          | Less | Leq | Greater | Geq -> 
            (match cdt1 with
              | Float -> Translate.Binop(Float,ce1,op,ce2)
              | Int -> Translate.Binop(Int,ce1,op,ce2)
              | Long -> Translate.Binop(Long,ce1,op,ce2)
              |  Cstring -> 
                  let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.Num)) in 
                     Assign(Id(Int, auto_var), (Call(Int, "strcmp", [ce1;ce2])));
              | _ -> raise(Failure("With the type checking in Sast, this should never be reached...")) 
            )
          | LogAnd | LogOr -> Translate.Binop(Int,ce1,op,ce2)
        )
    | Sast.Call(func_name, el, dt) -> 
        (
            match func_name with
            | "print" ->
                let rec print_builder elems = function
                | [] -> List.rev elems
                | hd :: tl -> 
                    let print_expr = translate_expr env hd in
                    let e_t = get_expr_type hd in
                    (match e_t with
                      | Num | String | Bool | Node -> 
                          print_builder (Expr(Call(Void, "f1", [print_expr])) :: elems) tl
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
                                     Id(Ptr(List(elem_type)), auto_var),
                                     Assign(Id(elem_type, auto_var), Member(Ptr(List(dt_to_ct dt)), auto_var, "next")),
                                     [Call(Void, "f1", [Deref(elem_type, Member(elem_type, auto_var, "data"))])]
                                 );
                                 Vdecl(Ptr(List(dt_to_ct dt)), auto_var)
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
            | _ -> 
                let cel = List.map (translate_expr env) el in
                let index = "f" ^ string_of_int(find_var func_name env.func_inds) in
                Call(dt_to_ct dt, index, cel)
        )
            
    | Sast.Access(v, e, dt) -> Nostmt
        (*     let index = "v" ^ string_of_int(find_var v env.var_inds) in
            let ce = translate_expr env e in
            Access(dt_to_ct dt, index, ce) *)
    | Sast.MemberCall(v, f, el, dt) -> Nostmt (* TODO *)
    | Sast.Undir(v1, v2, dt) -> Nostmt (* TODO *)
    | Sast.Dir(v1, v2, dt) -> Nostmt (* TODO *)
    | Sast.UndirVal(v1, v2, w, dt) -> Nostmt (* TODO *)
    | Sast.DirVal(v1, v2, w, dt) -> Nostmt (* TODO *)
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
              | List(dt) -> Vdecl(Ptr(List(dt_to_ct dt)), index) (* C: list_t *x; *)
              | Dict(dtk, dtv) -> Vdecl(Ptr(Ptr(Entry)), index) (* TODO *)
              | Void -> raise (Failure ("should not be using Void as a datatype"))
            )
    | Sast.Assign(v, e, dt) ->
            let ce = translate_expr env e in
            let cv = translate_expr env v in
            let var_type = get_expr_type e in
            (*let index = "v" ^ string_of_int(find_var v env.var_inds) in*)
            let auto_var = "v" ^ string_of_int (find_max_index !(List.hd env.var_inds)) in
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
            ( match e with 
              | Binop(e1, op, e2, dt) -> 
                  Block([ce;
                         (*Expr(Assign(Id((dt_to_ct var_type), index), Id((dt_to_ct var_type), auto_var)))]) *)
                         Expr(Assign(cv, Id((dt_to_ct var_type), auto_var)))])
              | StrLiteral(s1, dt) | NumLiteral(s1, dt) | Id(s1, dt) -> (* Expr(Assign(Id((dt_to_ct var_type), index), ce)) *)
                  Expr(Assign(cv, ce))
              | DictLiteral(tl, dt) -> Expr(Assign(cv, Id(Void, "NULL"))) (* TODO *)
              | _ -> raise (Failure("Assign don't work like that ")))

            
                    (*         if not( (find_var v env.var_types) = get_expr_type e)
        then raise (Failure ("assignment expression not of type: " ^ type_to_str (find_var v env.var_types) ))
        else (translate_expr env (Sast.Id(v, dt))) ^ " = " ^ (translate_expr env e) *)

                    (* | Sast.AssignList(v, el) -> Expr(Noexpr)
                    | Sast.DictAssign(k, v) -> Expr(Noexpr) 
           Block([Vdecl(Ptr(dt_to_ct dt), auto_var);
           Cast(Ptr(var_type), Call("malloc", [Call("sizeof", type_to_str var_type)]))
                         ]) *)
    | Sast.AccessAssign(e1, e2, dt) -> Nostmt
    | Sast.Return(e, dt) -> Translate.Return( translate_expr env e)           
    | Sast.NodeDef (id, s, dt) -> 
        (match s with
          | Sast.Noexpr ->                                                 
            let index = "v" ^ string_of_int(find_var id env.var_inds) in
            Block([Expr(Assign(Id(Node, index), Call(Void, "init_node", [Literal(Cstring, "")]))); 
              Expr(Assign(Member(Ptr(Void), index, "data"), Literal(Cstring,"")))])
          | _ -> 
            let index = "v" ^ string_of_int(find_var id env.var_inds) in
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
    | Sast.For (temp, iter, sl) ->
            let iter_type = get_expr_type iter in
            let auto_index = "v" ^ string_of_int(create_auto env temp iter_type) in
            let auto_var = "v" ^ string_of_int(create_auto env temp (Sast.Void)) in
            (*let index = "v" ^ string_of_int (find_var iter env.var_inds) in*)
            
            (*let iter_type = (find_var iter env.var_types) in*)

            let csl = List.map (translate_stmt env) sl in
            Block(
            [Vdecl(Ptr(dt_to_ct iter_type), auto_index); Expr(Assign(Id(dt_to_ct iter_type, auto_index), translate_expr env iter))]
            @
            [
            (match iter_type with
            | List(dt) -> Block([Vdecl(Ptr(List(dt_to_ct dt)), auto_var); 
                For(Assign(Id(dt_to_ct dt, auto_var), Id(Ptr(List(dt_to_ct dt)), auto_index)),
                Id(Ptr(List(dt_to_ct dt)), auto_var),
                Assign(Id(dt_to_ct dt, auto_var), Member(Ptr(List(dt_to_ct dt)), auto_var, "next")),
                csl
                )
                                 ])
            | Dict(dtk, dtv) -> 
                    let int_var = "v" ^ string_of_int(create_auto env "" (Sast.Num)) in
                    let entry_var = "v" ^ string_of_int(create_auto env "" (Sast.Dict(Void, Void))) in
                    let key_var = "v" ^ string_of_int(create_auto env "" (Sast.Void)) in
                    Block([Vdecl(Int, int_var); Vdecl(Ptr(Entry), entry_var);
                        Vdecl(Ptr(Void), key_var); 
                        For(Assign(Id(Int, int_var), Literal(Int, "0")),
                            Binop(Int, Id(Int, int_var), Ast.Less, Id(Int, "TABLE_SIZE")),
                            Assign(Id(Int, int_var), 
                                Binop(Int, Id(Int, int_var), Ast.Add,
                                      Literal(Int, "1"))),
                            [For(Assign(Id(Ptr(Entry), entry_var), Access(Entry, Id(Ptr(Entry), auto_index), Id(Int, int_var))), 
                                 Id(Entry, entry_var),
                                 Assign(Id(Ptr(Entry), entry_var), Member(Entry, entry_var, "next")),
                                 List.map (translate_stmt env) sl
                            )]
                        )
                    ])
            | Node -> 
                Block([Vdecl(Ptr(Node), auto_var); 
                Expr(Assign(Id(Ptr(Node), auto_var), Ref(Node, Id(Ptr(Node), auto_index))))] @ csl)
            | Graph -> Block([Vdecl(Ptr(Node), auto_var); 
                              For(Assign(Id(Ptr(Node), auto_var), Member(Ptr(Node), auto_index, "nodes")),
                                Id(Ptr(Node), auto_var),
                                Assign(Id(Ptr(Node), auto_var), Member(Ptr(Node), auto_var, "next")),
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
