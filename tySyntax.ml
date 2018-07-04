open Syntax
type tyvar = int

let index = ref 0
let new_tyvar () = (index := !index + 1); !index

type ty =
  | TySym of string
  | TyVar of tyvar
  | TyFun of string * (ty list)

type tgoal = ty list
type trule = ty * tgoal
type env = (string * tyvar) list

exception TyError

let rec term2ty env t =
  let rec lookup env x =
    (try
      Some (List.assoc x env )
    with
      Not_found -> None) in
  let rec inner env t = match t with
    | EConstSym s -> (TySym s, env)
    | EVar s -> (match lookup env s with
        | Some x -> (TyVar x, [])
        | None ->
          let v = new_tyvar () in
          (TyVar v, (s, v) :: env))
    | EFunctor(s, tl) ->
      (let rec inner2 env tl = (match tl with
        | [] -> raise TyError
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

let rec goal2tgoal env g = match g with
  | [] -> ([], [])
  | x::xs ->
    let (t, env) = (term2ty env x)  in
    let (ts, env) = (goal2tgoal env xs) in
    (t::ts, env)

let rule2trule (t, g) =
  let (t, env) = term2ty [] t in
  let (g, _) = goal2tgoal env g in
  (t, g)


let rec print_type t = match t with
  | TySym s ->
    (print_string s)
  | TyVar v ->
    (print_string "VAR<";
     print_int v;
     print_string ">")
  | TyFun (_, tl) ->
    let rec inner tl = match tl with
      | [] -> ()
      | x::xs -> (print_type x; (inner xs))
    in inner tl

let rec print_tgoal g = match g with
  | [] -> ()
  | x::xs -> (print_type x; print_tgoal xs)

let rec print_trule (t, g) =
  (print_type t;
   print_string ":-";
   print_tgoal g)
