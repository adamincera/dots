open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lexbuf in
  let result = eval prg in
  print_endline (string_of_program result);;