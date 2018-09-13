module A         = Tiger_absyn
module Env       = Tiger_env
module E         = Tiger_error
module Map       = Tiger_map
module Pos       = Tiger_position
module Symbol    = Tiger_symbol
module Translate = Tiger_translate
module Type      = Tiger_env_type
module Value     = Tiger_env_value

(* The only reason for having this seemingly-superfluous inner module is to
 * have this nice signature as a summary of what each function does. *)
module Semant : sig
  type expty =
    { exp : Translate.exp
    ; ty  : Type.t
    }

  (* Violating normal naming convention just to make it easier to follow
   * Appel's
   *)
  val transExp : env:Env.t -> A.exp -> expty
  val transVar : env:Env.t -> A.var -> expty
  val transDec : env:Env.t -> A.dec -> Env.t
  val transTy  : env:Env.t -> A.ty  -> Type.t  (* needs only type env *)
end = struct
  type expty =
    { exp : Translate.exp
    ; ty  : Type.t
    }

  let unimplemented () =
    failwith "unimplemented"

  (* TODO: Perhaps a wrapper for env.get that raises semantic error if not found *)

  let transExp ~env:_ exp =
    (match exp with
    | A.NilExp ->
        unimplemented ()
    | A.IntExp _ ->
        unimplemented ()
    | A.StringExp {string=_; _} ->
        unimplemented ()
    | A.CallExp {func=_; args=_; pos=_} ->
        unimplemented ()
    | A.OpExp {left=_; oper=_; right=_; pos=_} ->
        unimplemented ()
    | A.RecordExp {fields=_; typ=_; pos=_} ->
        unimplemented ()
    | A.SeqExp _ ->
        unimplemented ()
    | A.AssignExp {var=_; exp=_; _} ->
        unimplemented ()
    | A.IfExp {test=_; then'=_; else'=_; _} ->
        unimplemented ()
    | A.WhileExp {test=_; body=_; _} ->
        unimplemented ()
    | A.ForExp {var=_; lo=_; hi=_; body=_; _} ->
        unimplemented ()
    | A.BreakExp _ ->
        unimplemented ()
    | A.LetExp {decs=_; body=_; _} ->
        unimplemented ()
    | A.ArrayExp {typ=_; size=_; init=_; _} ->
        unimplemented ()
    | A.VarExp _ ->
        unimplemented ()
    )

  let transVar ~env:_ var =
    (match var with
    | A.SimpleVar {symbol=_; _} ->
        unimplemented ()
    | A.FieldVar {var=_; symbol=_; _} ->
        unimplemented ()
    | A.SubscriptVar {var=_; exp=_; _} ->
        unimplemented ()
    )

  let transDec ~env:_ dec =
    (match dec with
    | A.VarDec {name=_; typ=_; init=_; pos=_; escape=_} ->
        unimplemented ()
    | A.TypeDecs _ ->
        unimplemented ()
    | A.FunDecs _ ->
        unimplemented ()
    )

  let transTy ~env:_ typ =
    (match typ with
    | A.NameTy {symbol = _; pos = _} ->
        unimplemented ()
    | A.RecordTy _ ->
        unimplemented ()
    | A.ArrayTy {symbol = _; pos = _} ->
        unimplemented ()
    )
end

let transProg absyn =
  let open Semant in
  let {exp = _; ty = _} = transExp absyn ~env:Env.base in
  ()
