open Ast
open Translate

module StringMap = Map.Make(String)

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs


let translate (functions, stmts) = 
	let built_in_functions = StringMap.add "print" (-1) StringMap.empty in

    let function_indexes = string_map_pairs built_in_functions
      (enum 1 1 (List.map (fun f -> f.fname) functions)) in

    let stmt_list = ["printf(\"hello world\")"] in

    print_main stmt_list

(*
type func_decl = {
    fname : string;
    formals : string list;
    body : stmt list;
  }
*)

(* translate version *)
(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  translate (prg.funcs, prg.cmds)
*)

(* pretty printing version *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = string_of_program (prg.funcs, prg.cmds) in
  print_endline result;;
