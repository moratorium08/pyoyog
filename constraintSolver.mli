exception TyError

(*
 * the type of substitution
 *)
type subst = (TySyntax.tyvar * TySyntax.ty) list

(*
 * the type of constraints
 *   a list of equations t1 = t2 for types t1 and t2
 *)
type constraints = (TySyntax.ty * TySyntax.ty) list

(*
 * return the most general unifier of the constraints
 * raise TyError if unification fails
 *)
val unify : constraints -> subst

val compose: (TySyntax.tyvar * TySyntax.ty) list
  -> (TySyntax.tyvar * TySyntax.ty) list
  -> (TySyntax.tyvar * TySyntax.ty) list

val print_constraints: (TySyntax.ty * TySyntax.ty) list -> unit
val print_substs: (TySyntax.tyvar * TySyntax.ty) list -> unit
(*
 * apply the substitution to the type
 *)
val ty_subst : subst -> TySyntax.ty -> TySyntax.ty
