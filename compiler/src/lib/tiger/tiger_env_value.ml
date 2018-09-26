module List    = ListLabels

module Map    = Tiger_map
module Symbol = Tiger_symbol
module Temp      = Tiger_temp
module Translate = Tiger_translate
module Type   = Tiger_env_type

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
  (Symbol.t, t ) Map.t

let built_in =
  [ ("print"     , [Type.String]                     , Type.Unit   )
  ; ("flush"     , []                                , Type.Unit   )
  ; ("getchar"   , []                                , Type.String )
  ; ("ord"       , [Type.String]                     , Type.Int    )
  ; ("chr"       , [Type.Int]                        , Type.String )
  ; ("size"      , [Type.String]                     , Type.Int    )
  ; ("substring" , [Type.String; Type.Int; Type.Int] , Type.String )
  ; ("concat"    , [Type.String; Type.String]        , Type.String )
  ; ("not"       , [Type.Int]                        , Type.Int    )
  ; ("exit"      , [Type.Int]                        , Type.Unit   )
  ]
  |> List.map ~f:(fun (name, formals, result) ->
      let key   = Symbol.of_string name in
      let level = Translate.Level.init in
      let label = Temp.Label.gen () in
      let value = Fun {formals; result; level; label} in
      (key, value)
    )
  |> Map.of_list
