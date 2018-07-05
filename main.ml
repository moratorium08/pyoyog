open Syntax
open TySyntax
open Rules
open Eval

let rec read_eval_print rls env =
  print_string "?- ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  ((print_cmd cmd);
   (print_string "\n");
    let (rls, env, s) = eval_cmd rls env cmd in
    (match s with
    | Fail -> print_string "false"
    | Rule -> ()
    | Found (s, g, _) ->
      (print_solution s;
      ));
  (print_string "\n");
  (read_eval_print rls env))


let _ = read_eval_print (make ()) []
