open Syntax
open Rules
open Eval

let rec read_eval_print rls =
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  ((print_cmd cmd);
   (print_string "\n");
    let (rls, s) = eval_cmd rls cmd in
    (match s with
    | None -> print_string "false\n"
    | Some s -> print_string "true\n");
    (read_eval_print rls))


let _ = read_eval_print (make ())
