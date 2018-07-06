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
    let (rls', env', r) = eval_cmd rls env cmd in
    (match r with
     | Fail -> print_string "false\n"
     | Rule -> print_string "true\n"
     | Found (s, q) ->
       ((print_solution s);
          (let c = read_line () in
           if c = ";" then
             (let rec inner q = match search_solution rls' env' q with
                 | (_, _, Fail) -> print_string "false\n"
                 | (_, _, Rule) -> print_string "true\n"
                 | (_, _, Found(s, q)) ->
                   ((print_solution s);
                  (let c = read_line () in
                   if c = ";" then
                    inner q
                   else
                     ()
                   ))
              in inner q
             )
           else
             ()
          )
       )
    );
    (print_string "\n");
    (read_eval_print rls' env'))


let _ = read_eval_print (make ()) []
