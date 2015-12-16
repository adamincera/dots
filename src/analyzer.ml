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
               | Sast.Expr(e) -> 
                   stmt_sifter {s_globals = sifted.s_globals; 
                   s_main = hd :: sifted.s_main; 
                   s_funcs = sifted.s_funcs} tl
               | Sast.NodeDef(id, e, dt) | Sast.Assign(id, e, dt) -> 
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
               | Sast.Fdecl(f) -> {s_globals = sifted.s_globals; 
                           s_main = sifted.s_main; 
                           s_funcs = f :: sifted.s_funcs}
              )

type translation_env = {
            var_inds : int StringMap.t ref list;              (* var names to indices ex. x -> 1 so that we can just refer to it as v1 *)
            var_types : Sast.dataType StringMap.t ref list;   (* maps a var name to its type  ex. x -> num *)
            func_inds : int StringMap.t ref list;             (* func names to indices ex. x -> 1 so that we can just refer to it as f1 *)
            func_obj : Sast.s_fdecl StringMap.t ref list;  (* maps a func name to its return type *)
            return_type : Sast.dataType;                       (* what should the return type be of the current scope *)
    }

let mappings = [("in", Sast.Node); ("out", Sast.Node); ("value", Sast.Node); ("nodes", Sast.Graph)] 
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
    | Sast.DictLiteral(kvl, dt) -> dt
    | Sast.Boolean(v, dt) -> dt
    | Sast.Id(v, dt) -> dt
    | Sast.Binop(e1, op, e2, dt) -> dt
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
    | Sast.Node -> Node (* TODO *)
    | Sast.List(dt) -> Void (* TODO *)
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
         | "" -> "a" ^ string_of_int(ind)
         | _ -> key
      ) in
     (*  auto_cnt := StringMap.add var_name ind !auto_cnt; (* add new auto_var ref *) *)
      (List.hd env.var_types) := StringMap.add var_name dt !(List.hd env.var_types); (* add type map *)
      (List.hd env.var_inds) := StringMap.add var_name ind !(List.hd env.var_inds); (* add index map *)      
      ind
in

let rec translate_expr env = function 
    | Sast.NumLiteral(l, dt) -> Literal(Float, l)
    | Sast.StrLiteral(l, dt) -> Literal(Cstring, l)
    | Sast.ListLiteral(el, dt) -> Nostmt (* TODO *)
    | Sast.DictLiteral(kvl, dt) -> Nostmt (* TODO *)
    | Sast.Boolean(b, dt) -> if b = Ast.True then Literal(Int, "1") else Literal(Int, "0")
    | Sast.Id(v, dt) -> 
            let index = "v" ^ string_of_int(find_var v env.var_inds) in (* see if id exists, get the num index of the var *)
            Id(dt_to_ct dt, index) 
    | Sast.Binop(e1, op, e2, dt) ->
            let ce1 = translate_expr env e1 in
            let ce2 = translate_expr env e2 in
            let cdt = Translate.get_expr_type ce1 in
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
        )
        ) *)
        (match op with
        | Add -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | Sub -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | Mult | Div -> Binop(Float, ce1, op, ce2)
        | Equal -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | Neq -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | Less -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | Leq -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | Greater -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | Geq -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | LogAnd -> Binop(cdt, ce1, op, ce2) (* TODO *)
        | LogOr -> Binop(cdt, ce1, op, ce2) (* TODO *)
        )
    | Sast.Call(func_name, el, dt) -> 
        (
            match func_name with
            | "print" ->
                let rec print_builder elems = function
                | [] -> elems
                | hd :: tl -> 
                    let e_t = get_expr_type hd in
                    (match e_t with
                      | Num | String | Bool | Node -> 
                          print_builder (List.rev(Expr(Call(Void, "f1", [translate_expr env hd])) :: List.rev elems)) tl
                      | List(dt) -> 
                          let auto_var = "v" ^ string_of_int(create_auto env "" (Sast.List(dt))) in
                          (* print_builder 
                          ( For()
                            :: elems
                          ) 
                          tl *) 
                         [Nostmt](*TODO*)
                      | Dict(dtk, dtv) -> print_builder (Nostmt :: elems) tl (*TODO*)
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
            
    | Sast.Access(v, e, dt) -> 
            let index = "v" ^ string_of_int(find_var v env.var_inds) in
            let ce = translate_expr env e in
            Access(dt_to_ct dt, index, ce)
    | Sast.MemberVar(v, m, dt) -> Nostmt (* TODO *)
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
                                Expr(Assign(Id(Graph, index), Call(Void, "init_graph", [])))
                               ]) (* C: graph_t *g1 = init_graph(); *)
              | Node -> Block([Vdecl(Ptr(Node), index); 
                               Expr(Assign(Id(Node, index), Call(Void, "init_node", [Literal(Cstring, "")])))
                              ]) (* C: node_t *x = init_node(""); *)
              | List(dt) -> Vdecl(Ptr(List), index) (* C: list_t *x; *)
              | Dict(dtk, dtv) -> Vdecl(Ptr(Ptr(Entry)), index) (* TODO *)
              | Void -> raise (Failure ("should not be using Void as a datatype"))
            )
    | Sast.Assign(v, e, dt) ->
            let ce = translate_expr env e in
            let var_type = get_expr_type e in
            let index = "v" ^ string_of_int(find_var v env.var_inds) in
            (match var_type with
            | Num | String | Bool | Node | Void -> Expr(Assign(Id((dt_to_ct var_type), index), ce))
            | List(dt) -> Expr(Assign(Id((dt_to_ct var_type), index), ce)) (* TODO *)   
            | Dict(dtk, dtv) -> Expr(Assign(Id((dt_to_ct var_type), index), ce)) (* TODO *)
            | Graph -> Expr(Assign(Id((dt_to_ct var_type), index), Call(Graph, "copy", [ce])))
            )
                    (*         if not( (find_var v env.var_types) = get_expr_type e)
        then raise (Failure ("assignment expression not of type: " ^ type_to_str (find_var v env.var_types) ))
        else (translate_expr env (Sast.Id(v, dt))) ^ " = " ^ (translate_expr env e) *)

                    (* | Sast.AssignList(v, el) -> Expr(Noexpr)
                    | Sast.DictAssign(k, v) -> Expr(Noexpr) 
           Block([Vdecl(Ptr(dt_to_ct dt), auto_var);
           Cast(Ptr(var_type), Call("malloc", [Call("sizeof", type_to_str var_type)]))
                         ]) *)
    | Sast.Return(e, dt) -> Nostmt                                         (*TODO*)
    | Sast.NodeDef (id, s, dt) -> 
        (match s with
        | Noexpr ->                                                 (*TODO*)
          let index = "v" ^ string_of_int(find_var id env.var_inds) in
          Expr(Assign(Member(Ptr(Void), index, "data"), Literal(Cstring,"")))
        | _ -> 
          let index = "v" ^ string_of_int(find_var id env.var_inds) in
          Expr(Assign(Member(Ptr(Void), index, "data"), translate_expr env s))
        )          
    | Sast.While(cond, sl) -> Nostmt                                      (*TODO*)
    | Sast.If (cond, s1, s2) -> Nostmt                                    (*TODO*)
    | Sast.For (temp, iter, sl) ->
            let auto_var = "v" ^ string_of_int(create_auto env temp (Sast.Void)) in
            let index = "v" ^ string_of_int (find_var iter env.var_inds) in
            let iter_type = (find_var iter env.var_types) in
            let csl = List.map (translate_stmt env) sl in
            (match iter_type with
            | List(dt) -> Block([Vdecl(Ptr(List), auto_var); 
                For(Assign(Id(dt_to_ct dt, auto_var), Id(Ptr(List), index)),
                Id(Ptr(List), auto_var),
                Assign(Id(dt_to_ct dt, auto_var), Member(Ptr(List), auto_var, "next")),
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
                            [For(Assign(Id(Ptr(Entry), entry_var), Access(Entry, index, Id(Int, int_var))), 
                                 Id(Entry, entry_var),
                                 Assign(Id(Ptr(Entry), entry_var), Member(Entry, entry_var, "next")),
                                 List.map (translate_stmt env) sl
                            )]
                        )
                    ])
            | Node -> 
                Block([Vdecl(Ptr(Node), auto_var); 
                Expr(Assign(Id(Ptr(Node), auto_var), Ref(Id(Ptr(Node), index))))] @ csl)
            | Graph -> Block([Vdecl(Ptr(Node), auto_var); 
                              For(Assign(Id(Ptr(Node), auto_var), Member(Ptr(Node), index, "nodes")),
                                Id(Ptr(Node), auto_var),
                                Assign(Id(Ptr(Node), auto_var), Member(Ptr(Node), auto_var, "next")),
                                csl
                                )
                             ])
            | _ -> raise (Failure(iter ^ " is not iterable"))
            )
            | Sast.While (cond, sl) -> 
                    let c_cond = translate_expr env cond in
                    let csl = List.map (translate_stmt env) sl in
                    While(c_cond, csl)
            | Sast.Fdecl (func) -> Nostmt
                    
                    (* ***** TODO *********)
                    (*
                    { crtype = Translate.Void;
                    cfname = "empty";
                    cformals = [(Int, "argc"); (Ptr(Cstring), "argv")];
                    cbody = []}
                  *)
                  
and
translate_fdecl env func = 
    {crtype = dt_to_ct func.s_rtype; 
     cfname = func.s_fname; 
     cformals = List.map (fun f -> (dt_to_ct (fst f), (snd f))) func.s_formals;
     cbody = (List.map (fun f -> translate_stmt env f) func.s_body)}
in
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
