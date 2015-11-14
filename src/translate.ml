let print_main stmt_list = 
   print_endline ("int main (int argc, char **argv) {") ;
   print_endline ("    " ^ String.concat "\n" stmt_list) ;
   print_endline ("}")