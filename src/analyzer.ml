open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = string_of_program (prg.vars, prg.funcs, prg.cmds) in
  print_endline result;;