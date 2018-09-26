type t =
  | Var of
      { access : Tiger_translate.access
      ; ty     : Tiger_env_type.t
      }
  | Fun of
      { formals : Tiger_env_type.t list
      ; result  : Tiger_env_type.t
      ; level   : Tiger_translate.Level.t
      ; label   : Tiger_temp.Label.t
      }

type env =
  (Tiger_symbol.t, t ) Tiger_map.t

val built_in : env
