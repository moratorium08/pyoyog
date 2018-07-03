open Syntax

let rec read_eval_print () =
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  ((print_cmd cmd);
   (print_string "\n");
   (read_eval_print  ()))


let _ = read_eval_print ()
