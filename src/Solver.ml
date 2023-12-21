(*
   As explained in the README.md ("Abstracting over an effect"),
   this module as well as other modules is parametrized over
   an arbitrary effect [T : Functor].
*)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make(T)
  module SatConstraint = SatConstraint.Make(T)
  module ConstraintSimplifier = ConstraintSimplifier.Make(T)
  module ConstraintPrinter = ConstraintPrinter.Make(T)

  type env = Unif.Env.t
  type log = PPrint.document list

  let make_logger c0 =
    let logs = Queue.create () in
    let c0_erased = SatConstraint.erase c0 in
    let add_to_log env =
      let doc =
        c0_erased
        |> ConstraintSimplifier.simplify env
        |> ConstraintPrinter.print_sat_constraint
      in
      Queue.add doc logs
    in
    let get_log () =
      logs |> Queue.to_seq |> List.of_seq
    in
    add_to_log, get_log

  (** See [../README.md] ("High-level description") or [Solver.mli]
      for a description of normal constraints and
      our expectations regarding the [eval] function. *)
  type ('a, 'e) normal_constraint =
    | NRet of 'a Constraint.on_sol
    | NErr of 'e
    | NDo of ('a, 'e) Constraint.t T.t

  let eval (type a e) ~log (env : env) (c0 : (a, e) Constraint.t)
    : log * env * (a, e) normal_constraint
  =
    let add_to_log, get_log =
      if log then make_logger c0
      else ignore, (fun _ -> [])
    in
    (* We recommend calling the function [add_to_log] above
       whenever you get an updated environment. Then call
       [get_log] at the end to get a list of log message.

       $ dune exec -- minihell --log-solver foo.test

       will show a log that will let you see the evolution
       of your input constraint (after simplification) as
       the solver progresses, which is useful for debugging.

       (You can also tweak this code temporarily to print stuff on
       stderr right away if you need dirtier ways to debug.)
    *)
    let rec eval : type a e. env -> (a, e) Constraint.t -> env * (a, e) normal_constraint =
      fun env c ->
        match c with
        | Ret ret -> env, NRet ret
        | Err err -> env, NErr err
        | Eq (w1, w2) ->
          begin match Unif.unify env w1 w2 with
          | Ok env ->
            add_to_log env;
            env, NRet ignore
          | Error (Cycle c) ->
            env, NErr (Cycle c)
          | Error (Clash (var1, var2)) ->
            env, NErr (Clash (Decode.decode env var1, Decode.decode env var2))
          end
        | Map (c, f) ->
          let env, c = eval env c in
          env,
          begin match c with
          | NRet ret -> NRet (fun g -> f (ret g))
          | NErr _ as err -> err
          | NDo _ -> Utils.not_yet "Solver.eval: Do case" ()
          end
        | MapErr (c, f) ->
          let env, c = eval env c in
          env,
          begin match c with
          | NRet _ as ret -> ret
          | NErr err -> NErr (f err)
          | NDo _ -> Utils.not_yet "Solver.eval: Do case" ()
          end
        | Conj (c1, c2) ->
          let env, c1 = eval env c1 in
          let env, c2 = eval env c2 in
          env,
          begin match c1, c2 with
          | NRet ret1, NRet ret2 -> NRet (fun f -> ret1 f, ret2 f)
          | (NErr _ as err), _ | _, (NErr _ as err) -> err
          | NDo _, _ | _, NDo _ -> Utils.not_yet "Solver.eval: Do case" ()
          end
        | Exist (w, s, c) ->
          let env = Unif.Env.add w s env in
          add_to_log env;
          eval env c
        | Decode var ->
          let repr = Unif.Env.repr var env in
          env,
          NRet (
            match repr.structure with
            | None -> fun f -> f var
            | Some s -> fun f -> Constr (Structure.map f s)
          )
        | Do _ ->
          Utils.not_yet "Solver.eval: Do case" (env, c0, add_to_log, get_log)
    in
    add_to_log env;
    let env, c = eval env c0 in
    get_log (), env, c

end
