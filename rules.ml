open TySyntax

type rules = {rules: trule list; length: int}

let make () = {rules= [];
               length=0}

let add r rls =
  let rules = rls.rules in
  let length = rls.length in
  {rules = r::rules;
   length= length + 1}

let rec len l = match l with
  | [] -> 0
  | _::xs -> 1 + (len xs)

let rec eq_rule ((name, ts), g) (name2, ts2) =
  name = name2 && (len ts) = (len ts2)

