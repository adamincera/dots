(* ties together parsing scanning ast sast the whole fucking thing *)
open Ast
open Sast
open TypeConverter
open Translate
open Analyzer

(* pretty printing version *)

let _ =
let lexbuf = Lexing.from_channel stdin in
let prg = Parser.program Scanner.token lexbuf in
let result = string_of_program ([], List.rev prg.cmds) in
print_endline result;; 