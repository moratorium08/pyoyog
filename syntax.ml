type term = (*EConstInt of int
          | EConstStr of string*)
          | EConstSym of string
          | EVar of string
          | EFunctor of string * (term list)

type notflag = bool

type predicate =
  | PNot of predicate
  | PPredicate of term

type goal = predicate list

(* factは長さ0のリストを持つruleにする *)
type rule = term * goal

type solution = (string * term) list

type command =
  | CRule of rule
  | CAsk  of goal

let rec print_term t = match t with
  (*| EConstInt i -> print_int i
  | EConstStr s -> (print_string "'";  print_string s; print_string "'")*)
  | EConstSym s -> print_string s
  | EVar s -> print_string s
  | EFunctor (s, tl) ->
    let rec print_args l = match l with
      | [] -> ()
      | x::xs ->
        ((print_term x);
         (if List.length xs > 0
         then (print_string ", ")
         else ());
         (print_args xs))
    in
    ((print_string s); (print_string "("); (print_args tl); (print_string ")"))

let rec print_terms terms = match terms with
  | [] -> ()
  | x::(y::xs) ->
    (print_term x;
     print_string ", ";
     print_terms (y::xs))
  | x::xs ->
    (print_term x;
     print_terms xs)

let rec print_predicate p = match p with
  | PNot p ->
    (print_string "\\+";
     print_predicate p)
  | PPredicate t ->
    print_term t


let rec print_goal g = match g with
  | [] -> ()
  | x::(y::xs) ->
    (print_predicate x;
     print_string ", ";
     print_goal (y::xs))
  | x::xs ->
    (print_predicate x;
     print_goal xs)

let print_rule (p, g) =
  ((print_term p);
   (print_string " :- ");
   (print_goal g))

let print_solution s =
  let rec inner s = match s with
  | [] -> ()
  | (name, t) :: xs ->
    (print_string name;
     print_string " = ";
     print_term t;
     print_string "\n";
     inner xs
    )
  in match s with
  | [] -> print_string "true"
  | n -> inner n

let print_cmd cmd =((match cmd with
  | CRule r -> print_rule r
  | CAsk  g -> print_goal g);
   print_string ".")
