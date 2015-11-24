type c_func = { crtype : string;
                cfname : string;
                cformals : (string * string) list;
                cbody : string list; }

let string_of_cfunc func = 
    func.crtype ^ " " ^ func.cfname ^ " (" ^ 
    String.concat ", " (List.map (fun f -> fst f ^ " " ^ snd f) func.cformals) ^
     ")\n{\n" ^
    String.concat "\n" func.cbody ^
    "\n}\n"

let string_of_fname = function
| 1 -> "printf"
| x -> "f" ^ string_of_int(x)