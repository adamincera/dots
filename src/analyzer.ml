open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_program result);;