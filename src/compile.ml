(*ties together parsing scanning ast sast the whole fucking thing *)
open Ast
open Sast
open Translate

(* translate version *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast_prg = (Parser.program Scanner.token lexbuf) in (* outputs the Ast from parsing and scanning *)
  let sast_env = (* set up default environment *)
      let bf_names = [ "print"; "range";] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      let bf_type_map = ref (string_map_pairs StringMap.empty [(Sast.Void, "print"); (Sast.List(Sast.Num), "range")]) in
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_types = [bf_type_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in
  let prg = convert_ast {funcs = ast_prg.funcs; cmds = List.rev ast_prg.cmds} sast_env  in
  (* comment out for real: *) (* print_endline ("converted ast to sast"); *)
  let trans_env = (* set up default environ *)
      let bf_names = [ "print"; "range";] in
      let bf_inds = enum 1 1 bf_names in
      let bf_ind_map = ref (string_map_pairs StringMap.empty bf_inds) in
      let bf_type_map = ref (string_map_pairs StringMap.empty [(Sast.Void, "print"); (Sast.List(Sast.Num), "range")]) in
     {var_types = [ref StringMap.empty];
                       var_inds = [ref StringMap.empty];
                       func_types = [bf_type_map];
                       func_inds = [bf_ind_map];
                       return_type = Sast.Void} in
  translate (trans_env, prg.s_funcs, prg.s_cmds)
   (* print_endline (String.concat "\n" (List.map string_of_stmt (List.rev prg.cmds))) *)

(* pretty printing version *)
(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = string_of_program (prg.funcs, List.rev prg.cmds) in
  print_endline result;;
*)