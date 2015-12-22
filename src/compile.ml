(* ties together parsing scanning ast sast the whole fucking thing *)
open Ast
open Sast
open TypeConverter
open Translate
open Analyzer

(* translate version *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast_prg =
    (try
         (Parser.program Scanner.token lexbuf) (* outputs the Ast from parsing and scanning *)
    with exn -> 
          let curr = lexbuf.Lexing.lex_curr_p in
          let line = curr.Lexing.pos_lnum in
          let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
          let tok = Lexing.lexeme lexbuf in
          raise (Failure ("Parsing error: line " ^ string_of_int(line) ^ ", char " ^ string_of_int(cnum) ^ ", token " ^ tok))
    )
    in

      let print_decl = {
          s_fname = "print";
          s_rtype = Sast.Void;
          s_formals = [];
          s_body = [];
      } in
      let range_decl = {
          s_fname = "range";
          s_rtype = Sast.List(Sast.Num);
          s_formals = [];
          s_body = [];
      } in

(* set up default environment *)
  let sast_env = 
      (* built-in function set-up *)
      let bf_names = [ "print"; "range";] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      let bf_fdecl_map = ref (string_map_pairs StringMap.empty [(print_decl, "print"); (range_decl, "range")]) in
     (* build default symbol tables: *)
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_obj = [bf_fdecl_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in

  (* convert Ast to Sast *)
  let sast_prg = convert_ast { cmds = List.rev ast_prg.cmds} sast_env  in

  (* massage Sast into a form more suitable for C Ast *)
  (* i.e. split up variable declarations, function definitions, and other *)
  let sifted_prg = stmt_sifter {s_globals = []; s_main = []; s_funcs = []} sast_prg.s_cmds in

  (* construct Sast program object from sifted *)
  let sifted_prg = {s_globals = List.rev sifted_prg.s_globals; 
                    s_main = List.rev sifted_prg.s_main; 
                    s_funcs = List.rev sifted_prg.s_funcs} in

  (* set up default environ *)
  let trans_env = 
      let bf_names = [ "print"; "range"; "len"; "min"; "max"] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      let bf_fdecl_map = ref (string_map_pairs StringMap.empty [(print_decl, "print"); (range_decl, "range")]) in
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_obj = [bf_fdecl_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in

  (* add declared functions to symbol tables *)
  let rec func_def_adder env = function
  | [] -> ignore()
  | hd :: tl -> 
      (List.hd env.func_obj) := StringMap.add hd.s_fname hd !(List.hd env.func_obj);
      (List.hd env.func_inds) := StringMap.add hd.s_fname (find_max_index !(List.hd env.func_inds)+1) !(List.hd env.func_inds); (* add index map *)
      ignore(func_def_adder env tl)
  in
  ignore(func_def_adder trans_env sifted_prg.s_funcs);

 (* convert Sast to C Ast *)
 let cprg = translate(trans_env, sifted_prg) in

  (* output C code from C Ast *)
  print_endline (translate_c(cprg.globals, cprg.cfuncs))

