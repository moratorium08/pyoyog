open Syntax
open TySyntax
open ConstraintSolver
open Rules


type eval_result =
  | ESucceeded
  | EFailure

exception NotButMatch

let edebug_flag = false

let edebug_msg s =
  if edebug_flag then
    (print_string s;
    print_string "\n")
  else
    ()

let rec contains_var t = match t with
  | TySym _ -> false
  | TyVar _ -> true
  | TyFun (s, t) ->
    let rec inner l = match l with
      | [] -> true
      | x::xs -> (contains_var x) && (inner xs)
    in inner t

let rec no_var_goals l = match l with
  | [] -> true
  | (f, x)::xs -> match f with
    | TNone -> false
    | _ ->
    (contains_var x) && (no_var_goals xs)

let eval_goal rls env g q sigma =
  let rules = rls.rules in
  if List.length g = 0 then
    ESucceeded
  else
    (* goalの先頭を取り出して、rulesからunifiableな
     * ものを探す。
     * 見つかる度にunifyをして代入した新しいgoalを
     * goalの一番後ろに追加したものとその
     * 代入のペアをqに追加する。
     * 一つも見つからなければfail
     *)
    let (nflag, t)  = List.hd g in
    let after = List.tl g in
    match nflag with
    | TNone
    | TDoubleNot ->
        (let rec loop l =
          match l with
          | [] -> ()
          | x::xs ->
            let ((s, tg), env) = rule2trule [] x in
            try
              let theta = unify [(t, s)] in
              let rec subst_goal g = match g with
                | [] -> []
                | (f, t)::ts -> (f, (ty_subst theta t)) :: (subst_goal ts)
              in
              ((if List.length tg = 0 then
                edebug_msg "tg is 0"
               else
                 ());
              (Queue.push ((subst_goal after) @ (subst_goal tg), compose theta sigma) q;
               loop xs))
            with
            TyError -> (loop xs)
        in (loop rules; EFailure))
    | TNot ->
      let rec loop_ret x ret = match ret with
          | [] -> []
          | y::ys -> (x::y)::(loop_ret x ys)
      in
      let rec loop_goal g ret = match g with
          | [] -> ret
          | x::xs -> loop_goal xs (loop_ret x ret)
      in
      let rec loop_rules l ret sigma = match l with
          | [] -> (ret, sigma)
          | x::xs ->
            let ((s, tg), env) = rule2trule [] x in
            try
              let theta = unify [(t, s)] in
              let rec subst_goal g = match g with
                | [] -> []
                | (f, t)::ts ->
                  match f with
                  | TNot -> (TDoubleNot, (ty_subst theta t)) :: (subst_goal ts)
                  | _ -> (nflag, (ty_subst theta t)) :: (subst_goal ts)
              in
              let g = subst_goal tg in
              (* not だがfactにmatchしてしまった *)
              if List.length g = 0 then
                (
                edebug_msg "g is 0";
                raise NotButMatch)
              else
                let new_theta = compose theta sigma in
                let new_ret = loop_goal g ret in
                loop_rules xs new_ret new_theta
            with
              TyError -> (loop_rules xs ret sigma)
      in
      let rec add_q l sigma q = match l with
        | [] -> ()
        | x::xs ->
          ((Queue.push (x, sigma) q); add_q xs sigma q)
      in
      try
        if contains_var t then
          ((*print_type (ty_subst sigma t);*)
          if no_var_goals after then
            (edebug_msg "no no-var goals"; EFailure)
          else
            (edebug_msg "there is var goals"; (Queue.push (after @ [(nflag, t)], sigma) q); EFailure))
        else
          let (ret, sigma) = loop_rules rules [after] sigma in
          (* 否定がtrueになるのは、unifiableな別のルールを探したが
           * 見つからなかった時*)
          if List.length ret = 0 then
            (edebug_msg "not found"; Queue.clear q; ESucceeded)
          else
            (add_q ret sigma q; EFailure)
      with
        NotButMatch -> EFailure


(* goalとqueueを受け取って、
 * queueを舐め、
 * 書き換えたgoalとqueueを返す *)

let rec contract_middle g q r =
  (* g g' : 引数となるgoal
   * h h' : notをcontractionしていった結果
  *)
  let rec reverse l r = match l with
    | [] -> r
    | x::xs -> reverse xs (x::r)
  in
  let rec inner f g g' h h' env = match (g, g') with
    | ([], []) -> Some(reverse h [], reverse h' [])
    | ((fx, x)::xs, (fy, y)::ys) ->
      let (a, env) = eq_type x y env in
      if a then
        (if is_opposite fx fy then
           if f then
             None
           else
            inner true xs ys h h' env
        else
          inner f xs ys ((fx, x) :: h) ((fy, y) :: h') env
       )
      else
        None
    | _ -> None
  in

  if Queue.is_empty q then
      (g, r)
  else
    let (g', theta) = Queue.take q in
    if List.length g <> List.length g' then
      (Queue.push (g', theta) r;
      contract_middle g q r)
    else
      match inner false g g' [] [] [] with
      | Some(g, g') ->
      (Queue.push (g', theta) r;
       contract_middle g q r)
      | None ->
      (Queue.push (g', theta) r;
      contract_middle g q r)

let rec search rls env q =
  if Queue.is_empty q
  then
    None
  else
    let (g, theta) = Queue.take q in
    let q' = Queue.copy q in
    let _ = Queue.clear q in
    let (g, q) = contract_middle g q' q in
    match eval_goal rls env g q theta with
    | ESucceeded ->
      Some theta
    | EFailure ->
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
