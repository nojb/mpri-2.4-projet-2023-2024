(** Infer contains the logic to generate an inference constraint from
    an untyped term, that will elaborate to an explicitly-typed term
    or fail with a type error. *)

(* You have to implement the [has_type] function below,
   which is the constraint-generation function. *)

module Make(T : Utils.Functor) = struct
  module Constraint = Constraint.Make(T)
  open Constraint
  module Untyped = Untyped.Make(T)

  (** The "environment" of the constraint generator maps each program
      variable to an inference variable representing its (monomorphic)
      type.

      For example, to infer the type of the term [lambda x. t], we
      will eventually call [has_type env t] with an environment
      mapping [x] to a local inference variable representing its type.
  *)
  module Env = Untyped.Var.Map
  type env = variable Env.t

  type err = eq_error =
    | Clash of STLC.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  let eq v1 v2 = Eq(v1, v2)
  let decode v = MapErr(Decode v, fun e -> Cycle e)
  let exists name ?eq k =
    let v = Var.fresh name in
    Exist(v, eq, k v)

  (** This is a helper function to implement constraint generation for
      the [Annot] construct.

      [bind ty k] takes a type [ty], and a constraint [k] parametrized
      over a constraint variable. It creates a constraint context that
      binds a new constraint variable [?w] that must be equal to [ty],
      and places [k ?w] within this context.

      For example, if [ty] is the type [?v1 -> (?v2 -> ?v3)] , then
      [bind ty k] could be the constraint
        [∃(?w1 = ?v2 -> ?v3). ∃(?w2 = ?v1 -> ?w1). k ?w2], or equivalently
        [∃?w3 ?w4. ?w3 = ?v1 -> ?w4 ∧ ?w4 = ?v2 -> ?v3 ∧ k ?w3].
  *)
  let rec bind (ty : STLC.ty) (k : Constraint.variable -> ('a, 'e) t) : ('a, 'e) t =
    let Constr c = ty in
    match c with
    | Var v ->
      exists (Structure.TyVar.name v) ~eq:(Var v) k
    | Arrow (ty1, ty2) ->
      bind ty1 @@ fun w1 ->
      bind ty2 @@ fun w2 ->
      exists "warr" ~eq:(Arrow (w1, w2)) k
    | Prod tys ->
      let rec loop ws = function
        | [] ->
          exists "wprod" ~eq:(Prod (List.rev ws)) k
        | ty :: tys ->
          bind ty (fun w -> loop (w :: ws) tys)
      in
      loop [] tys

  (** This function generates a typing constraint from an untyped term:
      [has_type env t w] generates a constraint [C] which contains [w] as
      a free inference variable, such that [C] has a solution if and only
      if [t] is well-typed in [env], and in that case [w] is the type of [t].

      For example, if [t] is the term [lambda x. x], then [has_type env t w]
      generates a constraint equivalent to [∃?v. ?w = (?v -> ?v)].

      Precondition: when calling [has_type env t], [env] must map each
      term variable that is free in [t] to an inference variable.
  *)
  let rec has_type (env : env) (t : Untyped.term) (w : variable) : (STLC.term, err) t =
    match t with
    | Untyped.Var x ->
      let+ () = eq w (Env.find x env) in
      STLC.Var x
    | Untyped.App (t, u) ->
      exists "wu" @@ fun wu ->
      exists "wt" ~eq:(Structure.Arrow (wu, w)) @@ fun wt ->
      let+ t = has_type env t wt
      and+ u = has_type env u wu in
      STLC.App (t, u)
    | Untyped.Abs (x, t) ->
      exists (Untyped.Var.name x) @@ fun wx ->
      exists "wt" @@ fun wt ->
      exists "warr" ~eq:(Arrow (wx, wt)) @@ fun warr ->
      let+ () = eq w warr
      and+ ty = decode wx
      and+ t = has_type (Env.add x wx env) t wt in
      STLC.Abs (x, ty, t)
    | Untyped.Let (x, t, u) ->
      exists "wt" @@ fun wt ->
      let+ t = has_type env t wt
      and+ u = has_type (Env.add x wt env) u w
      and+ ty = decode wt in
      STLC.Let (x, ty, t, u)
    | Untyped.Annot (t, ty) ->
      bind ty @@ fun wt ->
      let+ c = has_type env t wt
      and+ () = eq wt w in
      c
    | Untyped.Tuple ts ->
      let rec loop i wl = function
        | [] ->
          exists "wprod" ~eq:(Prod (List.rev wl)) @@ fun wprod ->
          let+ () = eq w wprod in
          []
        | t :: ts ->
          exists (Printf.sprintf "w%d" i) @@ fun wt ->
          let+ t = has_type env t wt
          and+ ts = loop (i+1) (wt :: wl) ts in
          t :: ts
      in
      let+ ts = loop 1 [] ts in
      STLC.Tuple ts
    | Untyped.LetTuple (xs, t, u) ->
      let rec loop env ws = function
        | x :: xs ->
          exists (Untyped.Var.name x) @@ fun w ->
          loop (Env.add x w env) (w :: ws) xs
        | [] ->
          let ws = List.rev ws in
          exists "wt" ~eq:(Prod ws) @@ fun wt ->
          let rec loop = function
            | w :: ws ->
              let+ ty = decode w
              and+ tys, t, u = loop ws in
              ty :: tys, t, u
            | [] ->
              let+ t = has_type env t wt
              and+ u = has_type env u w in
              [], t, u
          in
          loop ws
      in
      let+ tys, t, u = loop env [] xs in
      STLC.LetTuple (List.map2 (fun x ty -> x, ty) xs tys, t, u)

(*

Alternatively, one could use the simpler approach below (with only one loop
instead of two). The only difference is that the function above produces a
constraint of the form

  (ex x (ex y (ex z ... (conj (decode x) (conj (decode y) (conj (decode z) ...))))))

while the function below generates an equivalent constraint of the form

  (ex x (conj (ex y (conj (ex z (conj ...)) (decode z)) (decode y)) (decode x)))

My first attempt was to use the function below (which also seems nicely dual to
the Tuple case), but in order to match the output in run.t I switched to the one
above (for which the printer also produces "flatter" output).

   let rec loop env ws = function
     | x :: xs ->
       exists (Untyped.Var.name x) @@ fun w ->
       let+ tys, t, u = loop (Env.add x w env) (w :: ws) xs
       and+ ty = decode w in
       ty :: tys, t, u
     | [] ->
       exists "wt" ~eq:(Prod (List.rev ws)) @@ fun wt ->
       let+ t = has_type env t wt
       and+ u = has_type env u w in
       [], t, u
   in
   let+ tys, t, u = loop env [] xs in
   STLC.LetTuple (List.map2 (fun x ty -> x, ty) xs tys, t, u)

*)

    | Do p ->
      (* Feel free to postone this until you start looking
         at random generation. Getting type inference to
         work on all the other cases is a good first step. *)
      Utils.not_yet "Infer.has_type: Do case" (env, p, fun () -> has_type)
end
