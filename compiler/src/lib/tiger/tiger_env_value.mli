type t =
  | Var of
      {ty : Tiger_env_type.t}
  | Fun of
      { formals : Tiger_env_type.t list
      ; result  : Tiger_env_type.t
      }

type env =
  (Tiger_symbol.t, t ) Tiger_map.t

val built_in : env
