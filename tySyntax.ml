open Syntax
type tyvar = int

let index = ref 0
let new_tyvar () = (index := !index + 1); !index

type ty =
  | TySym of string
  | TyVar of tyvar
  | TyFun of string * (ty list)

type tynot =
  | TNone
  | TNot
  | TDoubleNot

type tpredicate =
  tynot * ty

type tgoal = tpredicate list
type trule = ty * tgoal
type env = (string * tyvar) list

exception TyError
exception NotCannotBeIncluded

let rec term2ty env t =
  let rec lookup env x =
    (try
      Some (List.assoc x env )
    with
      Not_found -> None) in
  let rec inner env t = match t with
    | EConstSym s -> (TySym s, env)
    | EVar s -> (match lookup env s with
        | Some x -> (TyVar x, env)
        | None ->
          let v = new_tyvar () in
          (TyVar v, (s, v) :: env))
    | EFunctor(s, tl) ->
      (let rec inner2 env tl = (match tl with
        | [] -> (raise TyError)
        | [x] ->
          let (v, env) = (inner env x) in
          ([v], env)
        | x::xs ->
          let (v, env) = inner env x in
          let (vl, env) = inner2 env xs in
          (v :: vl, env)
        ) in
       let (vl, env) = inner2 env tl in
       (TyFun(s, vl), env)
      )
  in
  inner env t


let rec print_env env = match env with
  | [] -> ()
  | (n, i) :: xs ->
    (print_string n;
     print_string " = ";
     print_int i)

let rec inv_assoc env v = match env with
  | [] -> raise TyError
  | (s, w)::xs -> if v = w then s else inv_assoc xs v

let rec ty2term env t = match t with
  | TySym s ->  EConstSym s
  | TyVar v -> EVar (inv_assoc env v)
  | TyFun (s, args) ->
      (let rec inner env tl = (match tl with
        | [] -> []
        | x::xs ->
          let v = ty2term env x in
          let vl = inner env xs in
          v :: vl
        ) in
       let vl = inner env args in
       EFunctor(s, vl)
      )


(*let rec tgoal2goal env g = match g with
  | [] -> []
  | x::xs ->
    try
        (let t = (tpredicate2predicate env x)  in
        let ts = (tgoal2goal env xs) in
        t::ts)
    with
        TyError ->
        let ts = (tgoal2goal env xs) in ts*)

let predicate2tpredicate env p =
  let rec inner p cnt = match p with
  | PNot p -> (inner p (cnt + 1))
  | PPredicate t ->
    let (t, env) = (term2ty env t) in
    match cnt with
    | 0 -> ((TNone, t), env)
    | n when n mod 2 = 0 ->
      ((TDoubleNot, t), env)
    | n ->
      ((TNot, t), env)
  in inner p 0

let rec goal2tgoal env g = match g with
  | [] -> ([], env)
  | x::xs ->
    let (t, env') = (predicate2tpredicate env x)  in
    let (ts, env'') = (goal2tgoal env' xs) in
    (t::ts, env'')

let rule2trule env (t, g) =
  let (t, env) = term2ty env t in
  let (g, env) = goal2tgoal env g in
  ((t, g), env)

let rec print_type t = match t with
  | TySym s ->
    (print_string s)
  | TyVar v ->
    (print_string "VAR<";
     print_int v;
     print_string ">")
  | TyFun (s, tl) ->
    let rec inner tl = match tl with
      | [] -> ()
      | x::xs ->
        (print_type x;
         print_string ", ";
         (inner xs))
    in (
      print_string s;
      print_string "(";
      inner tl;
      print_string ")"
    )

let rec print_tpredicate p = match p with
  | (TNone, t) ->
    (print_type t)
  | (TNot, t) ->
    (print_string "\\+";
     print_type t)
  | (TDoubleNot, t) ->
    (print_string "\\+\\+";
     print_type t)

let rec print_tgoal g = match g with
  | [] -> ()
  | x::xs -> (print_tpredicate x; print_tgoal xs)

let rec print_trule (t, g) =
  (print_type t;
   print_string ":-";
   print_tgoal g)
