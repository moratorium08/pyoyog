open Syntax

type tyvar
type ty =
  | TySym of string
  | TyVar of tyvar
  | TyFun of string * (ty list)

type tgoal = ty list
type trule = ty * tgoal
type env = (string * tyvar) list

val print_tgoal: tgoal -> unit
val print_trule: trule -> unit

val new_tyvar : unit -> tyvar

val print_type : ty -> unit

val term2ty : env -> term -> ty * env
val rule2trule : rule -> trule
val goal2tgoal : env -> goal -> tgoal * env
