open Syntax

type rules = {rules: rule array; length: int}

let make () = {rules=Array.make 20 (("", []), []);
               length=0}

let add p rls =
  if rls.length = Array.length rls.rules
  then {rules=Array.append rls.rules (Array.make rls.length p);
        length=rls.length + 1}
  else ((Array.set rls.rules rls.length p);
  {rules=rls.rules; length=rls.length + 1})

let rec len l = match l with
  | [] -> 0
  | _::xs -> 1 + (len xs)

let rec eq_rule ((name, ts), g) (name2, ts2) =
  name = name2 && (len ts) = (len ts2)

let rec find_predicate rls idx p =
  let rules = rls.rules in
  let length = rls.length in
  match idx with
  | x when x = length -> None
  | idx ->
    let rule = Array.get rules idx in
    if eq_rule rule p then Some (rule, idx)
    else find_predicate rls (idx + 1) p

