(* dots compiler similar to microC *)

type action = ast | compiler 

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", ast); ("-c", compiler); ]
  else compiler in
  
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    ast -> print_string (Ast.string_of_program program)
  | compiler-> let listing = 
  		compiler.string_of_prog (Compile.translate program)
    in print_endline listing
  | Compile -> Execute.execute_prog (Compile.translate program)
  | _ -> print_string "Unrecognized option"

  type action = Ast | Interpret | Bytecode | Compile