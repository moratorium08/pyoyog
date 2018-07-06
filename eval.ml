open Syntax
open TySyntax
open ConstraintSolver
open Rules

let eval_goal rls env g q sigma =
  let rules = rls.rules in
  if List.length g = 0 then
    true
  else
    (* goalの先頭を取り出して、rulesからunifiableな
     * ものを探す。
     * 見つかる度にunifyをして代入した新しいgoalを
     * goalの一番後ろに追加したものとその
     * 代入のペアをqに追加する。
     * 一つも見つからなければfail
     *)
    let t = List.hd g in
    let rec loop l =
      match l with
      | [] -> ()
      | x::xs ->
        let ((s, tg), env) = rule2trule [] x in
        try
          let theta = unify [(t, s)] in
          let rec subst_goal g = match g with
            | [] -> []
            | t::ts -> (ty_subst theta t) :: (subst_goal ts)
          in
          (Queue.push ((subst_goal tg), compose theta sigma) q;
           loop xs)
        with
        TyError -> (loop xs)
    in (loop rules; false)

let rec search rls env q =
  if Queue.is_empty q
  then
    None
  else
    let (g, theta) = Queue.take q in
    if eval_goal rls env g q theta then
      Some theta
    else
      search rls env q

let gen_queue l =
  let q = Queue.create () in
  (Queue.push (l, []) q; q)


type result = Fail
            | Found of solution * ((tgoal * subst) Queue.t)
            | Rule

let rec gen_solution env theta = match env with
  | [] -> []
  | (name, v) :: xs ->
    let s = ty2term env (ty_subst theta (TyVar v)) in
    (name, s) :: (gen_solution xs theta)

let search_solution rls env q =
    match search rls env q with
    | None -> (rls, env, Fail)
    | Some theta ->
      let sol = gen_solution env theta in
      (rls, env, Found (sol, q))

let eval_cmd rls env cmd = match cmd with
  | CRule r ->
    (add r rls, env, Rule)
  | CAsk g ->
    let (tg, env) = goal2tgoal [] g in
    let q = gen_queue tg in
    search_solution rls env q
