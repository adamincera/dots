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

  let sast_env = (* set up default environment *)
      let bf_names = [ "print"; "range";] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      (*  s_fdecl = {
    s_fname : string;
    s_rtype : dataType; 
    (*formals : (string * string) list; *)
    s_formals : (dataType * string) list;
    s_body : s_stmt list;
  }*)
      let bf_fdecl_map = ref (string_map_pairs StringMap.empty [(print_decl, "print"); (range_decl, "range")]) in
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_obj = [bf_fdecl_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in
  (* let sast_prg = convert_ast {funcs = ast_prg.funcs; cmds = List.rev ast_prg.cmds} sast_env  in *)
  let sast_prg = convert_ast { cmds = List.rev ast_prg.cmds} sast_env  in
  let sifted_prg = stmt_sifter {s_globals = []; s_main = []; s_funcs = []} sast_prg.s_cmds in
  let sifted_prg = {s_globals = List.rev sifted_prg.s_globals; 
                    s_main = List.rev sifted_prg.s_main; 
                    s_funcs = List.rev sifted_prg.s_funcs} in
  (* comment out for real: *)  (* print_endline ("converted ast to sast");  *)
  let trans_env = (* set up default environ *)
      let bf_names = [ "print"; "range"; "len"; "min"; "max"] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      let bf_fdecl_map = ref (string_map_pairs StringMap.empty [(print_decl, "print"); (range_decl, "range")]) in
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_obj = [bf_fdecl_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in
 (* let main = translate (trans_env, sast_prg.s_funcs, sast_prg.s_cmds) in *)
 (*let main = translate (trans_env, sast_prg.s_cmds) in*)
 (* let cprg = {libs = ["<stdio.h>"]; globals = [] ; cfuncs = [main]} in *)
 (* let cprg = {libs = ["<stdio.h>"]; globals = [] ; cfuncs = [main]} in *)
 let cprg = translate(trans_env, sifted_prg) in
  print_endline (translate_c(cprg.globals, cprg.cfuncs))

  (* print_endline (String.concat "\n" (List.map string_of_stmt (List.rev prg.cmds))) *)

(* pretty printing version *)

(*  let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = string_of_program ([], List.rev prg.cmds) in
  print_endline result;; 
  *)