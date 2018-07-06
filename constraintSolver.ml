open TySyntax
exception TyError

type subst = (tyvar * ty) list
type constraints = (ty * ty) list

let debug_flag = false

let debug s =
  if debug_flag then
    (print_string s; print_string "\n")
  else
    ()

let rec print_substs l = match l with
  | [] -> ()
  | (var, t)::xs ->
    ((print_type (TyVar var));
    (print_string " := ");
    (print_type t);
    (print_string "\n");
    (print_substs xs))

let rec print_constraints l = match l with
  | [] -> ()
  | (v, t)::xs ->
    ((print_type v);
    (print_string " = ");
    (print_type t);
    (print_string "\n");
    (print_constraints xs))

let rec occurance_check t alpha = match t with
  | TySym _ -> true
  | TyVar v -> v <> alpha
  | TyFun (_, tl) ->
    let rec inner tl = match tl with
      | [] -> true
      | x::xs -> (occurance_check x alpha ) && (inner xs)
    in inner tl

let rec ty_subst_search sigma v =  match sigma with
  | [] -> TyVar v
  | (u, t)::xs -> if u = v then t else (ty_subst_search xs v)

(*let rec ty_subst_inner sigma t = match t with*)
let rec ty_subst sigma t = (*print_string "ty_subst"*) match t with
  | TySym s -> TySym s
  | TyVar v -> ty_subst_search sigma v
  | TyFun (s, tl) ->
    let rec inner tl = match tl with
      | [] -> []
      | x::xs -> (ty_subst sigma x) :: (inner xs)
    in TyFun (s, inner tl)

let rec ty_subst_goal sigma g = match g with
  | [] -> []
  | x::xs ->
    let t = ty_subst sigma x in
    let ts = ty_subst_goal sigma xs in
    t ::ts

let rec ty_subst_c c sub = (*(print_int (List.length c)); (print_string "\n");*) match c with
  | [] -> []
  | (t1, t2) :: xs -> (((ty_subst [sub] t1), (ty_subst [sub] t2)) ::
                        (ty_subst_c xs sub))

let rec compose_inner s1 t = match t with
  | TySym s -> TySym s
  | TyVar u -> ty_subst_search s1 u
  | TyFun (s, tl) ->
    let rec inner tl = match tl with
      | [] -> []
      | x::xs -> (compose_inner s1 x) :: (inner xs)
    in TyFun (s, inner tl)

let rec _compose s1 s2 = match s2 with
  | [] -> []
  | (v, t)::xs -> (v, (compose_inner s1 t)) :: (_compose s1 xs)

let compose s1 s2 =  match (s1, s2) with
  | ([], []) -> []
  | ([], n) -> n
  | (n, []) -> n
  | (s1, s2) -> (_compose s1 s2) @ s1

let rec _ty_unify l =(*print_string "ty_unify"*) match l with
  | [] -> []
  | (t1, t2)::xs -> (match (t1, t2) with
    | (TySym s, TySym t) when s = t -> _ty_unify xs
    | (TyVar v, TyVar u) when v = u -> _ty_unify xs
    | (TyVar v, t)
    | (t, TyVar v) -> if occurance_check t v then
        ((_ty_unify(ty_subst_c xs (v, t))) @ [(v, t)])
      else
        ((debug "occurance check");
        (raise TyError))
    | (TyFun (s1, tl1), TyFun(s2, tl2))
      when s1 = s2 && (List.length tl1) = (List.length tl2) ->
      _ty_unify ((List.combine tl1 tl2) @ xs)
    | _ -> (debug "no match"; raise TyError)
    )

let rec ty_unify_inner l = match l with
 | [] -> []
 | x::xs -> compose [x] (ty_unify_inner xs)

let rec unify l =
  (*(print_constraints l);*)
  let r = _ty_unify l in
  (*(print_substs r);*)
  (ty_unify_inner r)
